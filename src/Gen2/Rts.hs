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
    mkClosureType c = let s = StrI $ map toUpper (show c) ++ "_CLOSURE"
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

var !h$stack = [h$done]; // [];                // thread-local
for(var i=1;i<500;i++) { h$stack[i] = 0; }
var !h$initStatic = [];           // we need delayed initialization for static objects, push functions here
                                  // to be initialized just before haskell runs

var !h$sp  = 0;                   // stack pointer
var !h$mask = 0;                  // mask async exceptions

var !h$staticThunks    = {};      // funcName -> heapidx map for srefs
var !h$staticThunksArr = [];      // indices of updatable thunks in static heap
var !h$threadId = 1;

// stg registers
`declRegs`;
`declRets`;

fun h$blackhole { throw "<<loop>>"; return 0; }
`ClosureInfo (jsv "h$blackhole") [] "blackhole" (CILayoutPtrs 2 []) CIBlackhole CINoStatic`;

// really done now
fun h$done o { return h$done; }
`ClosureInfo (jsv "h$done") [PtrV] "done" (CILayoutPtrs 2 []) (CIFun 0 0) CINoStatic`;

// many primops return bool, and we can cheaply convert javascript bool to 0,1 with |0
// so this is where we store our Bool constructors, hackily
// note: |0 hack removed because it's slow in v8, fixed positions still useful: x?1:0
fun h$false_e { return stack[sp]; }
`ClosureInfo (jsv "h$false_e") [] "GHC.Types.False" (CILayoutFixed 1 []) (CICon 1) CINoStatic`;

fun h$true_e { return stack[sp]; }
`ClosureInfo (jsv "h$true_e") [] "GHC.Types.True" (CILayoutFixed 1 []) (CICon 2) CINoStatic`;

fun h$con_e { return `Stack`[`Sp`]; };
var !h$f = { f: h$false_e, d: null };
var !h$t = { f: h$true_e, d: null };

fun h$catch a handler {
  `preamble`;
  `adjSp 2`;
  `Stack`[`Sp` - 1] = handler;
  `Stack`[`Sp`] = h$catch_e;
  `R1` = a;
  return h$ap_1_0_fast();
}

fun h$catch_e {
  `preamble`;
  `adjSpN 2`;
  return `Stack`[`Sp`];
}
`ClosureInfo (jsv "h$catch_e") [] "exception handler" (CILayoutFixed 2 [PtrV]) (CIFun 0 0) CINoStatic`;


// throw an exception: walk the thread's stack until you find a handler
fun h$throw e {
  `preamble`;
  while(`Sp` > 0) {
    var f = `Stack`[`Sp`];
    if(f === h$catch_e) break;
    if(f === h$upd_frame) { // unclaim black hole
      `Stack`[`Sp`-1].f = `Stack`[`Sp`-3];
      `Stack`[`Sp`-1].d = `Stack`[`Sp`-2];
    }
    var tag = f.gtag;
    if(tag <= 0) { // generic apply frame
        tag = (stack[`Sp` - 1] >> 8) + 2;
        offset = 2;
    }
    var size = tag & 0xff;
    `Sp` = `Sp` - size;
  }
  if(`Sp` > 0) {
    var handler = `Stack`[`Sp` - 1];
    `R1` = handler;
    `R2` = e;
    if(`Sp` > 3) { // don't pop the top-level handler
      `adjSpN 2`;
    }
    return h$ap_2_1_fast();
  } else {
    throw "unhandled exception in haskell thread";
  }
}

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

fun h$gc_check next {
//  log("gc_check: todo");
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
    o.gtag = 0;
    o.gi   = gcinfo;
  } else {
    o.gtag = gcinfo;
    o.gi   = [];
  }
}

// allocate function on heap
fun h$static_fun f arity name gai {
  return { f: f, d: null }
}

// allocate static thunk on heap (after setting closure info)
// we need two positions for static thunks, to have enough room for the update frame
// third position is for storing the original fun again, to be able to revert it
fun h$static_thunk f {
  // fixme push stuff to restore stuff here
  var h = { f: f, d: { } };
   
  return h;
}

// print a closure
fun h$printcl i {
  var cl = i.f;
  var d  = i.d;
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
      c.d = { };
      return c;
    case 1:
      c.d = { d1: xs[0] };
      return c;
    case 2:
      c.d = { d1: xs[0], d2: xs[1] };
      return c;
    case 3:
      c.d = { d1: xs[0], d2: xs[1], d3: xs[2] };
      return c;
    case 4:
      c.d = { d1: xs[0], d2: xs[1], d3: xs[2], d4: xs[3] };
      return c;
    case 5:
      c.d = { d1: xs[0], d2: xs[1], d3: xs[2], d4: xs[3], d5: xs[4] };
      return c;
    case 6:
      c.d = { d1: xs[0], d2: xs[1], d3: xs[2], d4: xs[3], d5: xs[4], d6: xs[5] };
      return c;
    case 7:
      c.d = { d1: xs[0], d2: xs[1], d3: xs[2], d4: xs[3], d5: xs[4], d6: xs[5], d7: xs[6] };
      return c;
    default:
      c.d = { d1: xs[0], d2: xs[1], d3: xs[2], d4: xs[3], d5: xs[4], d6: xs[5], d7: xs[6] };
      // fixme does closure compiler bite us here?
      for(var i=7;i<xs.length;i++) {
        c.d["d"+(i+1)] = xs[i];
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
  log("trampoline calling: " + f + "    " + JSON.stringify([h$printReg `R1`, h$printReg `R2`, h$printReg `R3`, h$printReg `R4`, h$printReg `R5`]));
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
    if(typeof(f) === 'function') {
      var tag = `Stack`[idx].gtag;
      var offset = 1;
      if(tag <= 0) { // generic apply frame
        tag = (stack[idx - 1] >> 8) + 2;
        offset = 2;
      }
      var size = tag & 0xFF;
      if(size < 1) throw("invalid stack frame size at: stack[" + idx + "], frame: " +`Stack`[idx].n);
//        log("checking frame: " + `Stack`[idx].n + " size " + size);
      if(f !== h$upd_frame) {
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
  if(typeof r === 'object' && r.hasOwnProperty('f') && r.hasOwnProperty('d')) {
    if(r.f.t === `Blackhole` && r.d.x1 && r.d.x1.n) {
      return ("blackhole: -> " + h$printReg({ f: r.d.x1, d: r.d.x2 }) + ")");
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

fun h$run cl cb {
//  `trace "runhs"`;
  h$run_init_static();
  h$stack[0] = h$done;
  h$stack[1] = h$baseZCGHCziConcziSynczireportError;
  h$stack[2] = h$catch_e;
  h$stack[3] = h$reduce;
  h$sp = 3;
  `R1` = cl;
  var c = cl.f;
  while(c !== h$done) {
    `replicate 10 (hsCall c)`;
    `hsCall c`;
    h$gc_check(c);
  }
  cb(`R1`);
}

`rtsApply`;
`rtsPrim`;
`closureTypes`;
`garbageCollector`;

fun h$runio_e {
  `preamble`;
  `R1` = `R1`.d.d1;
  `Stack`[++`Sp`] = h$ap_1_0;
  return h$ap_1_0;
}
`ClosureInfo (jsv "h$runio_e") [PtrV] "runio" (CILayoutFixed 2 [PtrV]) CIThunk CINoStatic`;

fun h$runio c {
  return { f: h$runio_e, d: { d1: c } };
}

fun h$flushStdout_e {
  `R1` = h$baseZCGHCziIOziHandlezihFlush;
  `R2` = h$baseZCGHCziIOziHandleziFDzistdout;
  return h$ap_1_fast();
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
             if(typeof s === 'object' && s.hasOwnProperty("f") && s.hasOwnProperty("d")) {
               if(s.f.t === `Blackhole` && s.d.x1 && s.d.x1.n) {
                 log("stack[" + i + "] = blackhole -> " + s.d.x1.n);
               } else {
                 log("stack[" + i + "] = -> " + s.f.n + " (" + h$closureTypeName(s.f.t) + ", a: " + s.f.a + ")");
               }
             } else {
               log("stack[" + i + "] = " + s);
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
    log(obj);
  }
  if(!obj.hasOwnProperty("d") || obj.d === undefined || obj.d === null) {
    log("h$checkObj: WARNING, something wrong with d:");
    log(obj);
  } else {
    var d = obj.d;
    for(var p in obj.d) {
      if(d.hasOwnProperty(p)) {
        if(p.substring(0,1) != "d") {
          log("h$checkObj: WARNING, unexpected field name: " + p);
          log(obj);
        }
        if(d[p] === undefined) {
          log("h$checkObj: WARNING, undefined field detected: " + p);
          log(obj);
        }
//        if(d[p] === null) {
//          log("h$checkObj: WARNING, null field detected: " + p);
//          log(obj);
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
    if(typeof ai === 'object') {
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

|]

