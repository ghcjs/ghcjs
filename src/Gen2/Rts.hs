{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Gen2.Rts where

import           Language.Javascript.JMacro
import           Language.Javascript.JMacro.Types

import           Gen2.Debug
import           Gen2.GC
import           Gen2.RtsApply
import           Gen2.RtsPrim
import           Gen2.RtsSettings
import           Gen2.RtsTypes
import           Gen2.Utils

import           Data.Bits
import           Data.Char                        (toLower, toUpper)
import qualified Data.List                        as L
import           Data.Monoid

import qualified Data.Text.Lazy                   as TL
import           Gen2.Printer
import           Text.PrettyPrint.Leijen.Text     hiding (pretty, (<>))

import           Encoding

updateThunk :: JStat
updateThunk =
  [j| `push [toJExpr R1, jsv "h$upd_frame"]`;
      `R1`.f = h$blackhole;
      `R1`.d1 = h$currentThread.tid;
      `R1`.d2 = null; // will be filled with waiters array
    |]

-- fixme move somewhere else
declRegs :: JStat
-- fixme prevent holes
declRegs = [j| var !h$regs = []; |]
        <> mconcat (map declReg (enumFromTo R1 R32))
        <> regGettersSetters
    where
      declReg r = (decl . StrI . map toLower . show) r <> [j| `r` = 0; |]

regGettersSetters :: JStat
regGettersSetters =
  [j| fun h$getReg n {
        `SwitchStat n getRegCases mempty`;
      }
      fun h$setReg n v {
        `SwitchStat n (setRegCases v) mempty`;
      }
    |]
  where
    getRegCases =
      map (\r -> (toJExpr (regNum r), [j| return  `r`; |])) (enumFrom R1)
    setRegCases v =
      map (\r -> (toJExpr (regNum r), [j| `r` = `v`; return; |])) (enumFrom R1)

declRets :: JStat
declRets = mconcat $ map (decl . StrI . ("h$"++) . map toLower . show) (enumFrom Ret1)

trace :: ToJExpr a => a -> JStat
trace e = [j| log(`e`);  |]

closureTypes :: JStat
closureTypes = mconcat (map mkClosureType (enumFromTo minBound maxBound)) <> closureTypeName
  where
    mkClosureType :: CType -> JStat
    mkClosureType c = let s = StrI $ "h$" ++ map toUpper (show c) ++ "_CLOSURE"
                      in  decl s <> [j| `iex s` = `c` |]
    closureTypeName :: JStat
    closureTypeName = [j| fun h$closureTypeName c {
                            `map (ifCT c) [minBound..maxBound]`;
                            return "InvalidClosureType";
                          }
                        |]
    ifCT :: JExpr -> CType -> JStat
    ifCT arg ct = [j| if(`arg` === `ct`) { return `show ct`; } |]

hsCall :: JExpr -> JStat
hsCall c = [j| `jstatIf rtsTraceCalls $ logCall c`;
               `jstatIf rtsTraceStack logStack`;
               `c` = `ApplExpr c []`;
             |]

logCall :: JExpr -> JStat
logCall c = [j| h$logCall(`c`); |]

logStack :: JStat
logStack = [j| h$logStack(); |]


rts = jsSaturate (Just "RTS") rts'
rtsDebug = renderJs (addDebug $ jsSaturate (Just "RTS") rts')

rtsStr :: String
rtsStr = TL.unpack . displayT . renderPretty 0.8 150 . pretty $ rts

rts' :: JStat
rts' = [j|

// make logging work in browser, node, v8 and spidermonkey, browser also logs to
// <div id="output"> if jquery is detected

var h$glbl;
fun h$getGlbl { h$glbl = this; }
h$getGlbl();
fun log {
  if(h$glbl) {
    if(h$glbl.console && h$glbl.console.log) {
      h$glbl.console.log.apply(h$glbl.console,arguments);
    } else {
      h$glbl.print.apply(this,arguments);
    }
  } else {
    print.apply(this, arguments);
  }
  // if we have jquery, add to <div id='output'> element
  if(typeof($) !== 'undefined') {
    var x = '';
    for(var i=0;i<arguments.length;i++) { x = x + arguments[i]; }
    var xd = $("<div></div>");
    xd.text(x);
    $('#output').append(xd);
  }
}

var !h$stack = null; // [];                // thread-local
var !h$initStatic = [];           // we need delayed initialization for static objects, push functions here
                                  // to be initialized just before haskell runs

var !h$sp  = 0;                   // stack pointer

var !h$staticThunks    = {};      // funcName -> heapidx map for srefs
var !h$staticThunksArr = [];      // indices of updatable thunks in static heap
var !h$currentThread = null;

// stg registers
`declRegs`;
`declRets`;

fun h$blackhole { throw "<<loop>>"; return 0; }
`ClosureInfo (jsv "h$blackhole") [] "blackhole" (CILayoutPtrs 2 []) CIBlackhole CINoStatic`;

fun h$done o {
  h$finishThread(h$currentThread);
  return h$reschedule;
}
`ClosureInfo (jsv "h$done") [PtrV] "done" (CILayoutPtrs 1 []) (CIFun 0 0) CINoStatic`;

fun h$doneMain {
  if(typeof process !== 'undefined' && process.exit) {
    process.exit(0);
  } else if(typeof quit !== 'undefined') {
    quit();
  }
  h$finishThread(h$currentThread);
  return h$reschedule;
}
`ClosureInfo (jsv "h$doneMain") [PtrV] "doneMain" (CILayoutPtrs 1 []) (CIFun 0 0) CINoStatic`;

// many primops return bool, and we can cheaply convert javascript bool to 0,1 with |0
// so this is where we store our Bool constructors, hackily
// note: |0 hack removed because it's slow in v8, fixed positions still useful: x?1:0
fun h$false_e { return `Stack`[`Sp`]; }
`ClosureInfo (jsv "h$false_e") [] "GHC.Types.False" (CILayoutFixed 1 []) (CICon 1) CINoStatic`;

fun h$true_e { return `Stack`[`Sp`]; }
`ClosureInfo (jsv "h$true_e") [] "GHC.Types.True" (CILayoutFixed 1 []) (CICon 2) CINoStatic`;

fun h$con_e { return `Stack`[`Sp`]; };
var !h$f = { f: h$false_e, d1: null, d2: null };
var !h$t = { f: h$true_e, d1: null, d2: null };

fun h$catch a handler {
  `preamble`;
  `adjSp 3`;
  `Stack`[`Sp` - 2] = h$currentThread.mask;
  `Stack`[`Sp` - 1] = handler;
  `Stack`[`Sp`] = h$catch_e;
  `R1` = a;
  return h$ap_1_0_fast();
}

fun h$catch_e {
  `preamble`;
  `adjSpN 3`;
  return `Stack`[`Sp`];
}
`ClosureInfo (jsv "h$catch_e") [] "exception handler" (CILayoutFixed 3 [PtrV,PtrV]) (CIFun 0 0) CINoStatic`;


// function application to one argument
fun h$ap1_e {
  var c = `R1`;
  `R1` = c.d1;
  `R2` = c.d2;
  return h$ap_1_1_fast();
}
`ClosureInfo (jsv "h$ap1_e") [] "apply1" (CILayoutFixed 2 [PtrV, PtrV]) CIThunk CINoStatic`;

// select first field
fun h$select1_e {
  var t = `R1`.d1;
  `adjSp 3`;
  `Stack`[`Sp`-2] = `R1`;
  `Stack`[`Sp`-1] = h$upd_frame;
  `Stack`[`Sp`] = h$select1_ret;
  `R1`.f = h$blackhole;
  `R1`.d1 = h$currentThread.tid;
  `R1`.d2 = null;
  `R1` = t;
  return h$ap_0_0_fast();
}
`ClosureInfo (jsv "h$select1_e") [] "select1" (CILayoutFixed 1 [PtrV]) CIThunk CINoStatic`;

fun h$select1_ret {
  `R1` = `R1`.d1;
  `adjSpN 1`;
  return h$ap_0_0_fast();
}
`ClosureInfo (jsv "h$select1_ret") [] "select1ret" (CILayoutFixed 1 []) (CIFun 0 0) CINoStatic`;

// select second field of a two-field constructor
fun h$select2_e {
  var t = `R1`.d1;
  `adjSp 3`;
  `Stack`[`Sp`-2] = `R1`;
  `Stack`[`Sp`-1] = h$upd_frame;
  `Stack`[`Sp`] = h$select2_ret;
  `R1`.f = h$blackhole;
  `R1`.d1 = h$currentThread.tid;
  `R1`.d2 = null;
  `R1` = t;
  return h$ap_0_0_fast();
}
`ClosureInfo (jsv "h$select2_e") [] "select2" (CILayoutFixed 1 [PtrV]) CIThunk CINoStatic`;

fun h$select2_ret {
  `R1` = `R1`.d2;
  `adjSpN 1`;
  return h$ap_0_0_fast();
}
`ClosureInfo (jsv "h$select2_ret") [] "select2ret" (CILayoutFixed 1 []) (CIFun 0 0) CINoStatic`;

// throw an exception: walk the thread's stack until you find a handler
fun h$throw e async {
  `preamble`;
  // log("throwing exception: " + async);
  // h$dumpStackTop(`Stack`,0,`Sp`);
  var origSp = `Sp`;
  var lastBh = null; // position of last blackhole frame
  while(`Sp` > 0) {
    // log("unwinding frame: " + `Sp`);
    var f = `Stack`[`Sp`];
    if(f === null || f === undefined) {
      throw("panic: invalid object while unwinding stack");
    }
    if(f === h$catch_e) break;
    if(f === h$upd_frame) {
      var t = `Stack`[`Sp`-1];
      // wake up threads blocked on blackhole
      var waiters = t.d2;
      if(waiters !== null) {
        for(var i=0;i<waiters.length;i++) {
          h$wakeupThread(waiters[i]);
        }
      }
      if(async) {
        // convert blackhole back to thunk
        if(lastBh === null) {
          h$makeResumable(t,`Sp`+1,origSp,[]); // [`R1`,h$return]);
        } else {
          h$makeResumable(t,`Sp`+1,lastBh-2,[h$ap_0_0,`Stack`[lastBh-1],h$return]);
        }
        lastBh = `Sp`;
      } else {
        // just raise the exception in the thunk
        t.f = h$raise_e;
        t.d1 = e;
        t.d2 = null;
      }
    }
    var size;
    if(f === h$ap_gen) { // h$ap_gen is special
      size = ((`Stack`[`Sp` - 1] >> 8) + 2);
    } else {
      var tag = f.gtag;
      if(tag < 0) { // dynamic size
        size = `Stack`[`Sp`-1];
      } else {
        size = tag & 0xff;
      }
    }
    `Sp` = `Sp` - size;
  }
  //log("unwound stack to: " + `Sp`);
  //h$dumpStackTop(`Stack`,0,origSp);
  if(`Sp` > 0) {
    var maskStatus = `Stack`[`Sp` - 2];
    var handler = `Stack`[`Sp` - 1];
    h$currentThread.mask = maskStatus;
    `R1` = handler;
    `R2` = e;
    if(`Sp` > 3) { // don't pop the top-level handler
      `adjSpN 3`;
    }
    return h$ap_2_1_fast();
  } else {
    throw "unhandled exception in haskell thread";
  }
}

// a thunk that just raises a synchronous exception
fun h$raise_e {
  return h$throw(`R1`.d1, false);
}
`ClosureInfo (jsv "h$raise_e") [PtrV] "h$raise_e" (CILayoutFixed 1 []) CIThunk CINoStatic`;

// a thunk that just raises an asynchronous exception
fun h$raiseAsync_e {
  return h$throw(`R1`.d1, true);
}
`ClosureInfo (jsv "h$raiseAsync_e") [PtrV] "h$raiseAsync_e" (CILayoutFixed 1 []) CIThunk CINoStatic`;

// a stack frame that raises an exception, this is pushed by
// the scheduler when raising an async exception
fun h$raiseAsync_frame {
  var ex = `Stack`[`Sp`-1];
  `adjSpN 2`;
  return h$throw(ex,true);
}
`ClosureInfo (jsv "h$raiseAsync_frame") [] "h$raiseAsync_frame" (CILayoutFixed 2 []) (CIFun 0 0) CINoStatic`;

// reduce result if it's a thunk, follow if it's an ind
// add this to the stack if you want the outermost result
// to always be reduced to whnf, and not an ind
fun h$reduce {
  `preamble`;
  if(`isThunk (toJExpr R1)`) {
    return `R1`.f;
  } else {
    `adjSpN 1`;
    return `Stack`[`Sp`];
  }
}
`ClosureInfo (jsv "h$reduce") [PtrV] "h$reduce" (CILayoutFixed 1 []) (CIFun 0 0) CINoStatic`;

var h$gccheckcnt = 0;
fun h$gc_check next {
//  log("gc_check: todo");
  if(++h$gccheckcnt > 1000) {
    for(var i=`Sp`+1;i<`Stack`.length;i++) {
      `Stack`[i] = null;
    }
    h$gccheckcnt = 0;
  }
  return 0;
}

// set heap/stack object information
fun h$setObjInfo o typ name fields a gcinfo regs srefs {
  o.t    = typ;
  o.i    = fields;
  o.n    = name;
  o.a    = a;
  o.gai  = regs;        // active registers with ptrs
  o.s    = srefs;
  if(`isArray gcinfo`) { // info doesn't fit in int tag
    o.gtag = gcinfo.length; // fixme this is wrong for multi-elem0;
    o.gi   = gcinfo;
  } else {
    o.gtag = gcinfo;
    o.gi   = [];
  }
}

// allocate function on heap
fun h$static_fun f arity name gai {
  return { f: f, d1: null, d2: null }
}

fun h$static_thunk f {
  // fixme push stuff to restore stuff here
  var h = { f: f, d1: null, d2: null };
  return h;
}

// print a closure
fun h$printcl i {
  var cl = i.f;
  var d  = i.d1;
  var r = "";
  switch(cl.t) {
    case `Thunk`:
      r += "thunk";
      break;
    case `Con`:
      r += "con[" + cl.a + "]";
      break;
    case `Pap`:
      r += "pap[" + cl.a + "]";
      break;
    case `Fun`:
      r += "fun[" + cl.a + "]";
      break;
    default:
      r += "unknown closure type";
      break;
  }
  r += " :: " + cl.n + " ->";
  var idx = 1;
  // fixme update for single field data
  for(var i=0;i<cl.i.length;i++) {
    r += " ";
    switch(cl.i[i]) {
      case `PtrV`:
        r += "[ Ptr :: " + d["d"+idx].f.n + "]";
        idx++;
        break;
      case `VoidV`:
        r += "void";
        break;
      case `DoubleV`:
        r += "(" + d["d"+idx] + " :: double)";
        idx++;
        break;
      case `IntV`:
        r += "(" + d["d"+idx] + " :: int)";
        idx++;
        break;
      case `LongV`:
        r += "(" + d["d"+idx] + "," + d["d"+(idx+1)] + " :: long)";
        idx+=2;
        break;
      case `AddrV`:
        r += "(" + d["d"+idx].length + "," + d["d"+(idx+1)] + " :: ptr)";
        idx+=2;
        break;
      default:
        r += "unknown field: " + cl.i[i];
    }
  }
  log(r);
}
/*
fun h$static_con0 f {
  if(hpS+1 >= hpDyn) run_gc();
  var h = hpS;
  heap[hpS++] = f;
  return h;
}

fun h$static_con f xs {
  if(hpS+1+xs.length >= hpDyn) run_gc();
  var h = hpS;
  var n = xs.length;
  heap[h] = f;
  for(var i=0;i<n;i++) {
    heap[hpS+i+1] = xs[i];
  }
  hpS += n+1;
  return h;
}

fun h$alloc_static n {
  if(hpS+n >= hpDyn) run_gc();
  var h = hpS;
  hpS += n;
  return h;
}
*/
fun h$init_closure c xs {
  switch(xs.length) {
    case 0:
      c.d1 = null; c.d2 = null;
      return c;
    case 1:
      c.d1 = xs[0]; c.d2 = null;
      return c;
    case 2:
      c.d1 = xs[0]; c.d2 = xs[1];
      return c;
    case 3:
      c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2] };
      return c;
    case 4:
      c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3] };
      return c;
    case 5:
      c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4] };
      return c;
    case 6:
      c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5] };
      return c;
    case 7:
      c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5], d6: xs[6] };
      return c;
    default:
      c.d1 = xs[0]; c.d2 = { d1: xs[1], d2: xs[2], d3: xs[3], d4: xs[4], d5: xs[5], d6: xs[6] };
      // fixme does closure compiler bite us here?
      for(var i=7;i<xs.length;i++) {
        c.d2["d"+i] = xs[i];
      }
      return c;
  }
}

fun h$run_init_static {
  if(h$initStatic.length == 0) return;
  for(var i=h$initStatic.length - 1;i>=0;i--) {
    h$initStatic[i]();
  }
  h$initStatic = [];
}

// print function to be called and first few registers
fun h$logCall c {
  var f = c;
  if(c && c.n) {
    f = c.n;
  } else {
    f = h$collectProps c;
  }
  log(h$threadString(h$currentThread) + "  trampoline calling: " + f + "    " + JSON.stringify([h$printReg `R1`, h$printReg `R2`, h$printReg `R3`, h$printReg `R4`, h$printReg `R5`]));
  h$checkStack();
}

fun h$collectProps o {
  var props = [];
  for(var p in o) { props.push(p); }
  return("{"+props.join(",")+"}");
}

fun h$checkStack {
  var idx = `Sp`;
  while(idx >= 0) {
    var f = `Stack`[idx];
    var size, offset;
    if(typeof(f) === 'function') {
      if(f === h$ap_gen) {
        size = (`Stack`[idx - 1] >> 8) + 2;
        offset = 2;
      } else {
        var tag = `Stack`[idx].gtag;
        if(tag <= 0) {
          size = `Stack`[idx-1];
          offset = 2;
        } else {
          size = tag & 0xff;
          offset = 1;
        }
      }
      if(size < 1) throw("invalid stack frame size at: stack[" + idx + "], frame: " +`Stack`[idx].n);
//        log("checking frame: " + `Stack`[idx].n + " size " + size);
      if(f !== h$returnf && f !== h$restoreThread) {
        for(var i=0;i<size-offset;i++) {
          if(typeof `Stack`[idx-offset-i] === 'function') {
            h$dumpStackTop `Stack` 0 `Sp`;
            throw("unexpected function in frame at: " + idx + " " + `Stack`[idx].n);
          }
        }
      }
      idx = idx - size;
    } else {
      h$dumpStackTop `Stack` 0 `Sp`;
      throw("invalid stack object at: " + idx);
    }
  }
}

fun h$printReg r {
  if(r === null) {
    return "null";
  } else if(typeof r === 'object' && r.hasOwnProperty('f') && r.hasOwnProperty('d1') && r.hasOwnProperty('d2')) {
    if(r.f.t === `Blackhole` && r.x) {
      return ("blackhole: -> " + h$printReg({ f: r.x.x1, d: r.d1.x2 }) + ")");
    } else {
      return (r.f.n + " (" + h$closureTypeName(r.f.t) + ", " + r.f.a + ")");
    }
  } else if(typeof r === 'object') {
    var res = h$collectProps(r);
    if(res.length > 40) {
      return (res.substr(0,40)+"...");
    } else {
      return res;
    }
  } else {
    var xs = new String(r) + "";
    if(xs.length > 40) {
      return xs.substr(0,40)+"...";
    } else {
      return xs;
    }
  }
}

// print top stack frame
fun h$logStack {
  if(typeof `Stack`[`Sp`] === 'undefined') {
    log("warning: invalid stack frame");
    return;
  }
  var size = 0;
  var gt = `Stack`[`Sp`].gtag;
  if(gt === -1) {
    size = `Stack`[`Sp` - 1] & 0xff;
  } else {
    size = gt & 0xff;
  }
  h$dumpStackTop `Stack` (`Sp`-size - 2) `Sp`;
  for(var i=Math.max(0,`Sp`-size+1); i <= `Sp`; i++) {
    if(typeof `Stack`[i] === 'undefined') {
      throw "undefined on stack";
    }
  }
}

`rtsApply`;
`rtsPrim`;
`closureTypes`;
`garbageCollector`;

fun h$runio_e {
  `preamble`;
  `R1` = `R1`.d1;
  `Stack`[++`Sp`] = h$ap_1_0;
  return h$ap_1_0;
}
`ClosureInfo (jsv "h$runio_e") [PtrV] "runio" (CILayoutFixed 2 [PtrV]) CIThunk CINoStatic`;

fun h$runio c {
  return { f: h$runio_e, d1: c, d2: null };
}

fun h$flushStdout_e {
  `R1` = h$baseZCGHCziIOziHandlezihFlush;
  `R2` = h$baseZCGHCziIOziHandleziFDzistdout;
  return h$ap_1_1_fast();
}
`ClosureInfo (jsv "h$flushStdout_e") [] "flushStdout" (CILayoutFixed 1 []) CIThunk CINoStatic`;
var !h$flushStdout = h$static_thunk(h$flushStdout_e);

var h$start = new Date();
fun h$dumpRes cl {
   h$printcl cl;
   var end = new Date();
   log("elapsed time: " + (end.getTime()-h$start.getTime()) + "ms");
}

// fixme move somewhere else

fun h$ascii s {
    var res = [];
    for(var i=0;i<s.length;i++) {
      res.push(s.charCodeAt(i));
    }
    res.push(0);
    return res;
}

fun h$dumpStackTop stack start sp {
        for(var i=start;i<=sp;i++) {
           var s = stack[i];
           if(s && s.n) {
             log("stack[" + i + "] = " + s.n);
           } else {
             if(s === null) {
               log("stack[" + i + "] = null WARNING DANGER");
             } else if(typeof s === 'object' && s !== null && s.hasOwnProperty("f") && s.hasOwnProperty("d1") && s.hasOwnProperty("d2")) {
               if(s.d1 === undefined) { log("WARNING: stack[" + i + "] d1 undefined"); }
               if(s.d2 === undefined) { log("WARNING: stack[" + i + "] d2 undefined"); }
               if(s.f.t === `Blackhole` && s.d1 && s.d1.x1 && s.d1.x1.n) {
                 log("stack[" + i + "] = blackhole -> " + s.d1.x1.n);
               } else {
                 log("stack[" + i + "] = -> " + s.f.n + " (" + h$closureTypeName(s.f.t) + ", a: " + s.f.a + ")");
               }
             } else if(h$isInstanceOf(s,h$MVar)) {
               var val = s.val ===
                 null ? " empty"
                      : " value -> " + s.val.f.n + " (" + h$closureTypeName(s.val.f.t) + ", a: " + s.val.f.a + ")";
               log("stack[" + i + "] = MVar " + val);
             } else if(h$isInstanceOf(s,h$MutVar)) {
               log("stack[" + i + "] = IORef -> " + s.val.f.n + " (" + h$closureTypeName(s.val.f.t) + ", a: " + s.val.f.a + ")");
             } else if(typeof s === 'object') {
               log("stack[" + i + "] = " + h$collectProps(s).substring(0,50));
             } else {
               log("stack[" + i + "] = " + (""+s).substring(0,50));
             }
          }
        }
     }

// check that a haskell heap object is what we expect:
// f is a haskell entry function
// d exists, but might be null, if it isn't, warn for any undefined/null fields or fields with unfamiliar names
fun h$checkObj obj {
  if(!obj.hasOwnProperty("f") || obj.f === null || obj.f === undefined || !obj.f.n || obj.f.a === undefined || typeof obj.f !== 'function') {
    log("h$checkObj: WARNING, something wrong with f:");
    log((""+obj).substring(0,200));
  }
  if(!obj.hasOwnProperty("d1") || obj.d1 === undefined) {
    log("h$checkObj: WARNING, something wrong with d1:");
    log((""+obj).substring(0,200));
  } else if(!obj.hasOwnProperty("d2") || obj.d2 === undefined) {
    log("h$checkObj: WARNING, something wrong with d2:");
    log((""+obj).substring(0,200));
  } else if(obj.d2 !== null && typeof obj.d2 === 'object') {
    var d = obj.d2;
    for(var p in d) {
      if(d.hasOwnProperty(p)) {
//        if(p.substring(0,1) != "d") {
//          log("h$checkObj: WARNING, unexpected field name: " + p);
//          log((""+obj).substring(0,200));
//        }
        if(d[p] === undefined) {
          log("h$checkObj: WARNING, undefined field detected: " + p);
          log((""+obj).substring(0,200));
        }
//        if(d[p] === null) {
//          log("h$checkObj: WARNING, null field detected: " + p);
//          log((""+obj).substring(0,200));
//        }
      }
    }
  }
}

fun h$traceForeign f as {
  if(`not rtsTraceForeign`) { return; }
  var bs = [];
  for(var i=0;i<as.length;i++) {
    var ai = as[i];
    if(ai === null) {
      bs.push("null");
    } else if(typeof ai === 'object') {
      var astr = ai.toString();
      if(astr.length > 40) {
        bs.push(astr.substring(0,40)+"...");
      } else {
        bs.push(astr);
      }
    } else {
      bs.push(""+ai);
    }
  }
  log("ffi: " + f + "(" + bs.join(",") + ")");
}

// the scheduler pushes this frame when suspending a thread that
// has not called h$reschedule explicitly
fun h$restoreThread {
  var f         = `Stack`[`Sp`-2];
  var frameSize = `Stack`[`Sp`-1];
//  log("restoreThread " + h$currentThread.tid + " sp: " + h$sp + " frame size: " + frameSize);
  var nregs = frameSize - 3;
  for(var i=1;i<=nregs;i++) {
    h$setReg(i, `Stack`[`Sp`-2-i]);
  }
  `Sp` = `Sp` - frameSize;
  return f;
}
`ClosureInfo (jsv "h$restoreThread") [] "restoreThread" CILayoutVariable (CIFun 0 0) CINoStatic`;

// return a closure in the stack frame to the next thing on the stack
fun h$return {
  `R1` = `Stack`[`Sp`-1];
//  log("h$return, returning: " + `R1`.f.n);
  `adjSpN 2`;
  return `Stack`[`Sp`];
}
`ClosureInfo (jsv "h$return") [] "return" (CILayoutFixed 2 []) (CIFun 0 0) CINoStatic`;

// return a function in the stack frame for the next call
fun h$returnf {
  var r = `Stack`[`Sp`-1];
//  log("h$returnf, returning: " + r.n);
  `adjSpN 2`;
  return r;
}
`ClosureInfo (jsv "h$returnf") [] "returnf" (CILayoutFixed 2 []) (CIFun 0 0) CINoStatic`;

// return this function when the scheduler needs to come into action
// (yield, delay etc), returning thread needs to push all relevant
// registers to stack frame, thread will be resumed by calling the stack top
fun h$reschedule {
  return h$reschedule;
}
`ClosureInfo (jsv "h$reschedule") [] "reschedule" (CILayoutFixed 0 []) CIThunk CINoStatic`;

// carefully suspend the current thread, looking at the
// function that would be called next
fun h$suspendCurrentThread next {
   var nregs;
  // pap arity
  if(next.t === `Pap`) {
    var pa;
    `papArity pa (toJExpr R1)`;
    nregs = (pa >> 8) + 1;
  } else if(next.t === `Fun`) {
    // for normal functions, the number active registers is in the .a proprty
    nregs = (next.a >> 8) + 1;
  } else {
    nregs = 1;  // Thunk, Con, Blackhole only have R1
  }
  `Sp` = `Sp`+nregs+3;
  for(var i=1;i<=nregs;i++) {
    `Stack`[`Sp`-2-i] = h$getReg(i);
  }
  `Stack`[`Sp`-2] = next;
  `Stack`[`Sp`-1] = nregs+3;
  `Stack`[`Sp`]   = h$restoreThread;
  h$currentThread.sp = `Sp`;
}

// resume an interrupted computation, the stack
// we need to push is in d1, restore frame should
// be there
fun h$resume_e {
  //log("resuming computation");
  //h$logStack();
  var s = `R1`.d1;
  `updateThunk`;
  for(var i=0;i<s.length;i++) {
    `Stack`[`Sp`+1+i] = s[i];
  }
  `Sp`=`Sp`+s.length;
  //h$logStack();
  return `Stack`[`Sp`];
}
`ClosureInfo (jsv "h$resume_e") [] "resume" (CILayoutFixed 0 []) CIThunk CINoStatic`;

fun h$unmaskFrame {
  h$currentThread.mask = 0;
  `adjSpN 1`;
  // back to scheduler to give us async exception if pending
  if(h$currentThread.excep.length > 0) {
    `push [toJExpr R1, jsv "h$return"]`;
    return h$reschedule;
  } else {
    return `Stack`[`Sp`];
  }
}
`ClosureInfo (jsv "h$unmaskFrame") [] "unmask" (CILayoutFixed 1 []) (CIFun 0 0) CINoStatic`;

fun h$maskFrame {
  h$currentThread.mask = 2;
  `adjSpN 1`;
  return `Stack`[`Sp`];
}
`ClosureInfo (jsv "h$maskFrame") [] "mask" (CILayoutFixed 1 []) (CIFun 0 0) CINoStatic`;

|]

