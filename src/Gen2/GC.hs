{-# LANGUAGE QuasiQuotes #-}

module Gen2.GC where

import           Language.Javascript.JMacro

import           Data.Monoid

import           Gen2.RtsSettings
import           Gen2.RtsTypes
import           Gen2.Utils

{-

  garbage collector
  1. find roots: heap objects reachable from stacks and registers
  2. find all reachable heap object by following pointers
  3. enumerate reachable heap objects form low to high
     - keep offset in var, skip while offset == 0
     - update offset

  incremental collection:
  1. don't consider references below start (usually start of dynamic heap)

  heap objects:
  entry functions have associated layout information tables:
  x.gi = [size, offset1, offset2, ...], stack info, size is size of frame, offsets are offsets
                                         where heap pointers are stored (negative)
  x.gi = [size, offset1, offset2, ...], heap info, size is size of frame, offsets are offsets
                                         where heap pointers are stored (positive)

  x.gtag ===  0  --> no gtag information, use slow list?
  x.gtag === -1  --> layout in object: [entry, tag, arg0, arg1, ..., argn]
  x.gtag  <  -1  --> heap object is Pap object of size -gtag-1, use embedded function to recover layout
  x.gtag  >   0  --> layout in gtag:   [entry, arg0, arg1, ..., argn]

  - gtag & 0xff                 --> size of whole object in array entries (including entry and tag)
  - (gtag >> (8+n)) & 1 === 1   --> argument n is a pointer
                                        (note: offset of argn differs depending on whether the layout is
                                         inside the object)
-}

{-

todo:
- static heap can contain ind closures:
   1. if nothing refers to an ind, restore original thunk?
   2. update reference if moved
- boxed array support: one heap object argument is a new array with heap pointers, follow them!

- dynamic heap never points directly to static heap
   1. x.sgi contains static refs? how? [staticEntry_1, staticEntry_2]
   2. staticEntry.sidx = heap index of original closure
   3.

- finalizers: single object: { idx: [finalizers] }

compact:
sort marked array
- walk array, update mark to shift (non-positive), push to marked array
- walk array again: update pointers
- final walk: move objects

-}

garbageCollector :: JStat
garbageCollector =
  [j|
     var gcTotal = 0;  // total time (ms) spent in GC
     // returns new hp
     // fixme multithreading, more than one stack, pass stack, sp, registers, next all in tso
     fun gc  heap          // array    heap
             hp            // int      index of first free element of heap
             stack         // array    stack
             sp            // int      points to top of stack
             next          // function next function to run
             dyn           // int      first element of dynamic heap
             old           // int      first element of new generation
             forward       // array    indices of old-gen closures that point to the new gen
             forwardStatic // array    indices of static closures that point to dynamic (fixme?)
             incremental   // bool     do incremental collection (only new gen)
             {
        `traceGc $ "hp: " |+ hp |+ " sp: " |+ sp`;
        `checkGc $ toStat ("heapCheck" |^^ [heap, hp])`;

//        dumpHeapTo heap hp;
//        dumpStack stack sp;
        var startTime = new Date().getTime();
        var startTime0 = startTime;
        var work = [];
        var gcStart = incremental ? old : dyn;

        var reachable   = [];  // reachable objects
        var reachable_e = [];  // entry functions saved in here, because overwritten
        var indirect    = [];

        // load all roots >= gcStart
        walkStack stack sp heap gcStart indirect reachable reachable_e;

        loadArgs next work gcStart;

        // add roots from older gen pointing to newer
        if(incremental) {
          for(var i=forward.length - 1;i>=0;i--) {
            var fwi = forward[i];
            var fwic = heap[fwi];
            `withPtrs fwi fwic heap (pushForwardPtr work gcStart)`;
          }
        }
        `emitTime startTime "follow stack"`;

        // follow everything in work stack
        var c,c0,e,i,j,g,n;
        c = work.pop();
        var rl = reachable.length;
        var il = indirect.length;
        `followPtr work gcStart heap indirect il reachable reachable_e rl c`;
        `traceGc $  "func: "  |+ (next|."i"|!!1)
                 |+ " gai: "  |+ stringify (next|."gai")
                 |+ " regs: " |+ stringify (enumFromTo R1 R5)`;
        // fixme static heap that indirects to dynamic?
        `emitTime startTime "follow function arguments"`;

        sortBoth reachable reachable_e;
        `traceGc $ "reachable heap objects: " |+ stringify reachable`;
        `traceGc $ "heap length: " |+ (heap|."length")`;
        `emitTime startTime "sorted"`;
        // now we have an array of reachable objects in order, calculate offsets
        // and store them in the entry location
        var htgt = gcStart;
        for(var i=0;i<reachable.length;i++) {
          var c  = reachable[i];
          var o = reachable_e[i];
          heap[c] = htgt - c;
          `traceGc $ "move: " |+ c |+ " -> " |+ htgt`;
          var size;
          `objSize size c (Just o)`;
          htgt += size;
        }
        `traceGc $ "heap length: " |+ (heap|."length")`;
        // update indirection pointers
        for(var i=indirect.length - 1;i>=0;i--) {
          var c = indirect[i];
          var tgt = heap[c+1];
          if(heap[tgt] === 1) {
            throw "todo: follow/update chains of indirections";
          }
          var htgt = heap[tgt];
          if(typeof(htgt) !== "number") { htgt = 0; } // if entry point not overwritten, object is not moved
          var adjTgt = tgt + htgt;
          `traceGc $ "updating indirect: " |+ c |+ " -> " |+ adjTgt |+ " : " |+ (adjTgt |- c)`;
          heap[c] = adjTgt - c;
        }
        `traceGc $ "heap length: " |+ (heap|."length")`;

        // now update all pointers in reachable objects
        for(var i=reachable.length - 1; i>=0;i--) {
          var c = reachable[i];
          var h = reachable_e[i];
          `withPtrsIdx c h heap (adjustPtr heap gcStart)`;
        }

        // update forward pointers on old heap
        if(incremental) {
          for(var i=forward.length - 1; i>=0;i--) {
            var c = forward[i];
            var h = heap[c];
            `withPtrsIdx c h heap (adjustPtr heap gcStart)`;
          }
        }
        `emitTime startTime "adjust pointers"`;
        `traceGc $ "heap length: " |+ (heap|."length")`;
        // adjust stack and registers
        adjustStack stack sp heap gcStart;
        adjustArgs next heap gcStart;
        `emitTime startTime "adjust stack"`;
        // finally actually move objects, move entry points back into place
        for(var i=0;i<reachable.length;i++) {
          var start = reachable[i];
          var c     = reachable_e[i];
          var size;
          `objSize size start (Just c)`;
          var adj   = start + heap[start];
          heap[adj] = c;
          `traceGc $ "actually moving: " |+ start |+ " -> " |+ adj`;
          for(var j=1;j<size;j++) {
            heap[adj+j] = heap[start+j];
          }
          `debugGc $ checkClosure heap adj`;
        }
        `emitTime startTime "compact heap"`;
        `traceGc $ "heap length: " |+ (heap|."length")`;

//        dumpHeapTo heap htgt;
//        dumpStack stack sp;
        `traceGc $ "func: " |+ (next|."i"|!!1) |+ " gai: " |+ stringify (next|."gai") |+ " regs: " |+ stringify (enumFromTo R1 R8)`;
        var spent = new Date().getTime()-startTime0;
        gcTotal += spent;
        `traceGcTiming $ "sp: " |+ sp |+ "   hp: " |+ hp |+ " -> " |+ htgt |+ " (" |+ spent |+ "ms)"`;
        `traceGcTiming $ "total: " |+ gcTotal |+ "ms)"`;
//        for(var x=htgt;x < heap.length;x++) { heap[x] = -8888888; } // debug, overwrite so problems are caught quickly
        `traceGc $ "heap length: " |+ (heap|."length")`;
        return htgt;
     }

     // walk a stack and follow all of its pointers to the heap
     fun walkStack stack       // array   stack to walk
                   sp          // int     points to top of stack
                   heap        // array   heap array
                   start       // int     only push indices >= start
                   indirect    // array   push indirections here
                   reachable   // array   push reachable (not visited before) items here
                   reachable_e // array   entry functions of reachable objects go hear
                   {
//       while(sp > 0 && typeof stack[sp] !== "function") sp--;
//       dumpStack stack sp;
       var work = [];
       var start0 = start | 0;
       var rl = reachable.length;
       var il = indirect.length;
       `assertGc (isFunction $ stack|!sp) "incomplete stack frame"`
       while(sp > 0) {
         `traceGc $ "walking stack: " |+ sp |+ " (" |+ ((stack|!sp)|."n") |+ ")"`;
         var tag = stack[sp].gtag;
         `assertGc (notUndef tag) "garbage collector tag undefined"`;
         var c;
         if(tag !== 0) { // fast path, info tag contains everything
           var offset;
           if(tag === -1) {    // layout stored inside object
             tag = stack[sp - 1];
             offset = 2;
           } else {            // layout in the tag we have
             offset = 1;
           }
           var bit = (1<<8);
           while(tag >= bit) {
             `traceGc $ "fast path: " |+ bit |+ " (tag: " |+ tag |+ "), offset: " |+ offset`;
             if(tag & bit) {
               c = stack[sp-offset];
              if(typeof c !== "number") {
                log("wrong pointer: " + sp + "-" + offset + "(" + stack[sp].i[1] + ")");
              } else {
//               frames += c;
                 if(c >= start0) {
                   `followPtr work start heap indirect il reachable reachable_e rl c`;
                 }
}
             }
             offset++;
             bit = bit << 1;
           }
           `traceGc $ "adjusting by tag: " |+ tag |+ " (" |+ (stack|!sp)|."i"|!!1 |+ ") - " |+ (tag |& (255::Int))`;
           sp = sp - (tag & 0xff);
         } else {  // slow path, use info list
           `traceGc "warning: slow path walking stack"`;
           var i = stack[sp].gi;
           `traceGc $ "walking stack: " |+ sp |+  " (frame size: " |+ (i|!!0) |+ ")"`;
           for(var j=i.length - 1;j>=1;j--) {
             c = stack[sp-i[j]];
             if(c >= start0) {
               `followPtr work start heap indirect il reachable reachable_e rl c`;
//               work.push(c);
               log("reachable: " + c);
             }
           }
           sp = sp - i[0];
         }
       }
     }

     fun adjustStack stack sp heap start {
//       dumpStack(stack, sp);
       if(typeof stack[sp] !== "function") log("incomplete stack frame");
       var v;
       var idx;
       while(sp > 0) {
         var tag = stack[sp].gtag;
         `assertGc (notUndef tag) "garbage collector tag undefined"`;
         `traceGc $ "adjusting stack object: " |+ (stack|!sp)|."n"`;
         if(tag !== 0) {
           var offset;
           if(tag === -1) {  // layout in object
             tag = stack[sp - 1];
             offset = 2;
           } else {          // layout in tag
             offset = 1;
           }
           var bits = 1 << 8;
           while(tag >= bits) {
             if(tag & bits) {
               idx = sp - offset;
               `traceGc $ "adjusting offset: " |+ offset |+ " at " |+ idx |+ " (value: " |+ (stack |! idx) |+ ")"`;
               v = stack[idx];
               if(typeof v === "number" && v >= start) { // fixme type check not necessary!
                 stack[idx] = v + heap[v];
               }
             }
             offset++;
             bits = bits << 1;
           }
           sp = sp - (tag & 0xff);
         } else {
           var i = stack[sp].gi;
           for(var j=1;j<i.length;j++) {
             var idx = sp-i[j];
             `traceGc $ "adjusting: " |+ idx |+ " (value: " |+ (stack |! idx) |+ ")"`;
             v = stack[idx];
             if(typeof v === "number" && v >= start) {  // fixme type check should not be necessary
               stack[idx] = v + heap[v];
             }
           }
           sp = sp - i[0];
         }
       }
     }

     // some basic heap check at start of gc
     fun heapCheck heap hp {
       if(hp > heap.length) { throw "hp too big"; }
       log("heap length: " + heap.length);
       for(var i=0;i < heap.length;i++) {
         if(heap[i] === undefined) { throw("hole in heap: " + i); }
       }
     }

     // check consistency of heap entry c, that means
     // heap[c].i[0] == heap[c].gi[0] == (heap[c].gtag & 0xff)
     // next (size-1) entries on the heap are not functions
     fun checkC heap c {
       if(!heap[c].i) {
         throw new Error("not an entry at: " + c);
       }
       var size;
       `objSize size c Nothing`;
       // fixme check is wrong since variable size arguments are supported now
       // if(size != heap[c].gi[0] ||
       //    size != (heap[c].gtag & 0xff)) {
       //    throw new Error("inconsistent closure sizes at: " + c);
       // }
       for(var i=1;i<size;i++) {
          if(typeof(heap[c+i]) === "function") {
            throw new Error("invalid closure variable at: " + c);
          }
       }
       // var g = heap[c].gi;
       // for(var i=1;i<g.length - 1;i++) {
       //   if(typeof(heap[c+g[i]]+offset) !== "number") {
       //      throw new Error("invalid closure variable (expected pointer): " + c);
       //   } // fixme add range check, must point to valid heap index
       // }
     }

     fun dumpStack stack sp {
       dumpStackTop stack 0 sp;
     }

     fun dumpStackTop stack start sp {
        for(var i=start;i<=sp;i++) {
           var s = stack[i];
           if(s && s.n) {
             log("stack[" + i + "] = " + s.n);
           } else {
             log("stack[" + i + "] = " + s);
           }
        }
     }

     fun dh {
       dumpHeap heap;
     }

     fun dumpHeap heap {
       log "static heap:";
       dumpHeapFromTo heap 0 hpDyn;
       log "\ndynamic heap:";
       dumpHeapFromTo heap hpDyn hp;
     }

     fun dumpHeapTo heap max {
       log "static heap:";
       dumpHeapFromTo heap 0 hpDyn;
       log "\ndynamic heap:";
       dumpHeapFromTo heap hpDyn max;
     }


     fun dumpHeapFromTo heap start stop {
       var undef = 0;
       for(var i=start;(i < stop || stop < 0);i++) {
         var h = heap[i];
         if(h === undefined || h === 0) { undef++; } else { undef = 0; }
         if(undef >= 10) { break; }
         if(h && h.n) {
            log("heap[" + i + "] = " + h.n + " (" + closureTypeName(h.t) + ")");
         } else {
            log("heap[" + i + "] = " + h);
         }
       }
     }

     // load all arguments that refer to heap objects into the work queue
     fun loadArgs next work {
        `traceGc $ "arguments reachability " |+ (next |. "i" |!! 1) |+ " -> " |+ next |. "gai"`;
        var g = next.gai;
        for(var i=g.length - 1;i>=0;i--) {
          var c = g[i];
          `loadArg work c`;
        }
     }

     fun adjustArgs next heap start {
       var g = next.gai;
       for(var i=g.length - 1; i>=0;i--) {
         var r = g[i];
         `updateArg r heap start`;
       }
     }

     fun setReg regnum val {
       `traceGc $ "setting reg: r" |+ regnum |+ " = " |+ val`;
       `assignReg regnum val`;
     }

     fun getReg regnum {
       `getReg regnum`;
       return -1;
     }

     // sort two arrays in place, use only the first to compare
     fun sortBoth a1 a2 {
       quicksortBoth a1 a2
     }

     // in-place quicksort with insertion sort for small arrays, based on http://jsfiddle.net/u8t2a/61/
     fun quicksortBoth a1 a2 {
       var stack = [];
       var left = 0;
       var right = a1.length - 1;
       var i, j, swap1, swap2, temp1, temp2;
       while(true) {
         if(right - left <= 25){
           for(j=left+1; j<=right; j++) {
             swap1 = a1[j];
             swap2 = a2[j];
             i = j - 1;
             while(i >= left && a1[i] > swap1) {
               a1[i+1] = a1[i];
               a2[i+1] = a2[i--];
             }
             a1[i+1] = swap1;
             a2[i+1] = swap2;
           }
           if(stack.length == 0) break;
           right = stack.pop();
           left = stack.pop();
         } else {
           var median = (left + right) >> 1;
           i = left + 1;
           j = right;
           swap1 = a1[median];
           swap2 = a2[median];
           a1[median] = a1[i];
           a2[median] = a2[i];
           a1[i] = swap1;
           a2[i] = swap2;
           if(a1[left] > a1[right]) {
             swap1 = a1[left];
             swap2 = a2[left];
             a1[left] = a1[right];
             a2[left] = a2[right];
             a1[right] = swap1;
             a2[right] = swap2;
           } if(a1[i] > a1[right]) {
              swap1 = a1[i];
              swap2 = a2[i];
              a1[i] = a1[right];
              a2[i] = a2[right];
              a1[right] = swap1;
              a2[right] = swap2;
            } if(a1[left] > a1[i]) {
              swap1 = a1[left];
              swap2 = a2[left];
              a1[left] = a1[i];
              a2[left] = a2[i];
              a1[i] = swap1;
              a2[i] = swap2;
            }
            temp1 = a1[i];
            temp2 = a2[i];
            while(true){
              do { i++; } while(a1[i] < temp1);
              do { j--; } while(a1[j] > temp1);
              if(j < i) break;
              swap1 = a1[i];
              swap2 = a2[i];
              a1[i] = a1[j];
              a2[i] = a2[j];
              a1[j] = swap1;
              a2[j] = swap2;
            }
            a1[left + 1] = a1[j];
            a2[left + 1] = a2[j];
            a1[j] = temp1;
            a2[j] = temp2;
            if(right - i + 1 >= j - left) {
              stack.push(i);
              stack.push(right);
              right = j - 1;
            } else{
              stack.push(left);
              stack.push(j - 1);
              left = i;
            }
          }
        }
      }

    |]

assignReg :: JExpr -> JExpr -> JStat
assignReg e v = mconcat $ map are [minBound..maxBound]
    where
      are r = [j| if (`e` === `regNum r`) { `r` = `v`; } |]

getReg :: JExpr -> JStat
getReg e = mconcat $ map re [minBound..maxBound]
    where
      re r = [j| if (`e` === `regNum r`) { return `r`; } |]


loadArg :: JExpr -> JExpr -> JStat
loadArg tgt e = SwitchStat e (map mkLoadCase $ enumFrom R1) mempty
  where
    mkLoadCase reg = ( ValExpr . JInt . fromIntegral . regNum $ reg
                     , [j| `tgt`.push(`reg`);
                            `traceGc $ "reachable register: r" |+ (regNum reg) |+ " = " |+ reg`;
                            break;
                         |]
                     )

updateArg :: JExpr -> JExpr -> JExpr -> JStat
updateArg r heap start = SwitchStat r (map mkUpdateCase $ enumFrom R1) mempty
  where
    mkUpdateCase reg = ( ValExpr . JInt . fromIntegral . regNum $ reg
                       , [j| if(`reg` >= `start`) {
                               `reg` += `heap`[`reg`];
                             }
                             break;
                           |]
                       )

{-
  iterate over all pointers in heap object, run some function on them
-}
withPtrs :: JExpr ->           -- ^ heap index of object to follow
            JExpr ->           -- ^ entry point of index (since it might have been overwritten by an offset)
            JExpr ->           -- ^ heap array
           (JExpr -> JStat) -> -- ^ what to do with each pointer (heap index -> some statement)
            JStat
withPtrs idx e heap f = [j| var tag = `e`.gtag;
                            `assertGc (notUndef tag) "garbage collector tag undefined"`;
                            var ptr;
                            if(tag !== 0) {   // tag info
                              var bits = (1<<8);
                              var offset;
                              if(tag === -1) {   // layout stored inside object
                                tag = `heap`[`idx`+1];
                                offset = 2;
                              } else if(tag <= -2) {  // pap object, special care required, reconstruct tag from fun chain
                                offset = 2;
                                var args = (-3) - tag;
                                var skip = 0;
                                var curr = `heap`[`idx`+1];
                                while((`heap`[curr]).t === `Pap`) {
                                  skip +=  (-3) - `heap`[curr].gtag;
                                  curr = `heap`[curr+1];
                                }
                                tag = (`heap`[curr].gtag >> skip) & (((1 << args) - 1) << 8); // fixme check this
                              } else {           // layout stored in the tag we already have
                                offset = 1;
                              }
                              while(tag >= bits) {
                                if(tag & bits) {
                                  ptr = `heap`[`idx`+offset];
                                  `checkGc $ checkPtr ptr offset`;
                                  `f ptr`;
                                 }
                                 offset++;
                                 bits = bits << 1;
                              }
                            } else {          // info list
                              var g = `e`.gi;
                              for(var i=g.length - 1;i >= 1;i--) {
                                ptr = `heap`[`idx` + g[i]];
                                `checkGc $ checkPtr ptr (g |! i)`;
                                `f ptr`;
                              }
                            }
                          |]
    where
      checkPtr ptr offset =
          [j| if(typeof `ptr` !== "number") {
                log("invalid pointer: " + `idx` + " + " + `offset` + " " + `e`.i[1]);
              }
            |]
-- like above, but with pointer index, not its value
withPtrsIdx :: JExpr ->           -- ^ heap index of object to follow
               JExpr ->           -- ^ entry point of index (since it might have been overwritten by an offset)
               JExpr ->           -- ^ heap array
              (JExpr -> JStat) -> -- ^ what to do with each pointer (heap index -> some statement)
               JStat
withPtrsIdx idx e heap f = [j| var tag = `e`.gtag;
                               var pidx;
                               if(tag !== 0) {
                                 var bits = (1<<8);
                                 var offset = 1;
                                 while(tag >= bits) {
                                   if(tag & bits) {
                                     pidx = `idx`+offset;
                                     `f pidx`;
                                   }
                                   offset++;
                                   bits = bits << 1;
                                 }
                               } else {  // slow path
                                 var g = `e`.gi;
                                 for(var i=g.length - 1;i >= 1;i--) {
                                   pidx = `idx`+g[i];
                                  `f pidx`;
                                 }
                               }
                          |]

adjustPtr :: JExpr -> -- ^ heap array
             JExpr -> -- ^ start
             JExpr -> -- ^ index to adjust
             JStat
adjustPtr heap start ptr =
   [j| `traceGc $ "adjusting pointer at: " |+ ptr`;
       var v = `heap`[`ptr`];
       if(v >= `start`) {
         `checkGc (checkAdj v)`;
         `heap`[`ptr`] = v + `heap`[v];
       }
     |]
  where checkAdj v =
            [j| if(typeof(`heap`[`v`]) !== "number" || isNaN(`heap`[`v`])) {
                  throw new Error("Invalid adjustment at: " + `v` + " (referenced by: " + `ptr` + ")");
                 }
              |]

pushWorkPtr :: JExpr -> -- ^ work stack to push to
               JExpr -> -- ^ start (don't push smaller ptrs)
               JExpr -> -- ^ reachable (don't push if reachable[ptr] exists)
               JExpr -> -- ^ the heap object pointer
               JStat
pushWorkPtr work start heap ptr =
  [j| if(`ptr` >= `start` && typeof `heap`[`ptr`] === "function") {
        `work`.push(`ptr`);
      }
    |]

pushForwardPtr :: JExpr -> -- ^ work stack to push to
                  JExpr -> -- ^ start (don't push smaller ptrs)
                  JExpr -> -- ^ the heap object pointer
                  JStat
pushForwardPtr work start ptr =
  [j| `checkGc checkPtr`;
      if(`ptr` >= `start`) {
        `work`.push(`ptr`);
      }
   |]
  where
    checkPtr =
        [j| if(typeof `ptr` !== 'number') {
              log("invalid pointer: " + `ptr`);
            }
          |]

emitTime :: JExpr -> String -> JStat
emitTime start name
    | gcTiming  = [j| var time = new Date().getTime();
                      log("< gc (" + (time-`start`) + "ms) " + `toJExpr name`);
                      `start` = time;
                    |]
    | otherwise = mempty

-- | follow a root pointer, collect everything on the heap reachable by it
--   the follow function replaces the first element of each reached closure with
--   either 1 (indirections) or 0 (everthing else)

followPtr :: JExpr -> -- ^ work array, push newly found pointers on this, may contain existing work
             JExpr -> -- ^ start index, don't follow/mark things below this
             JExpr -> -- ^ heap array
             JExpr -> -- ^ indirections array: indiretions are not marked reachable, but pushed here
             JExpr -> -- ^ length of ind array
             JExpr -> -- ^ reachable array: push indices of reachable closures here
             JExpr -> -- ^ reachable entry points: push entry functions of rechable closures here
             JExpr -> -- ^ length of reachable array
             JExpr -> -- ^ the pointer to start with
             JStat
followPtr work start heap ind ip reach reach_e rp ptr =
  [j|
      while(`ptr` !== undefined) {
        var hptr = `heap`[`ptr`];
        if(`ptr` >= `start`) {
          if(typeof hptr === "function") {
            `checkGc $ checkClosure heap ptr`;
            if(hptr.t === `Ind`) {
              var iptr = `heap`[`ptr`+1];
              `ind`[`ip`++] = `ptr`; // .push(`ptr`);
              `heap`[`ptr`] = 1;
              `ptr` = `iptr`;
            } else {
//              `reach`.push(`ptr`);
//              `reach_e`.push(hptr);
              `reach`[`rp`] = `ptr`;
              `reach_e`[`rp`++] = hptr;
              `heap`[`ptr`] = 0;
              `withPtrs ptr hptr heap (pushWorkPtr work start heap)`;
              `ptr` = `work`.pop();
            }
          } else {   // already reached
            `ptr` = `work`.pop();
          }
        } else {  // not >= start, so old-gen indirection, don't overwrite entry fun
          `withPtrs ptr hptr heap (pushWorkPtr work start heap)`;
          `ptr` = `work`.pop();
        }
      }
    |]

checkClosure heap ptr =
    [j| checkC `heap` `ptr`; |]

objSize :: JExpr -> JExpr -> Maybe JExpr -> JStat
objSize tgt ptr Nothing =
    [j| var cl = heap[`ptr`];
        `objSize tgt ptr (Just cl)`;
      |]
objSize tgt ptr (Just cl) =
    [j| var t = `cl`.gtag;
        if(t >= 0) {          // regular object
          `tgt` = t & 0xff;
        } else if(t === -1) { // layout stored in object
          `tgt` = heap[`ptr`+1] & 0xff;
        } else {              // pap object
          `tgt` = (-1) - t;
        }
     |]

