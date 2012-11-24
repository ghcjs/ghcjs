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
{-  slower than globals
declRegs = [j|
    fun Regs {
      `map initReg (enumFrom R1)`;
    }
    var !r = new Regs();
  |]
    where
      initReg r = AssignStat (SelExpr (jsv "this") (StrI $ regName r)) [je| 0 |]
-}
-- fixme prevent holes
declRegs = [j| var !regs = []; |] <> mconcat (map declReg (enumFromTo R1 R32))
    where
      declReg r = (decl . StrI . map toLower . show) r <> [j| `r` = 0; |]

declRets :: JStat
declRets = mconcat $ map (decl . StrI . map toLower . show) (enumFrom Ret1)

trace :: ToJExpr a => a -> JStat
trace e = [j| log(`e`);  |]

-- generate function thingie
{-
cheatFun :: String -> Int -> [VarType] -> JStat
cheatFun f tag free =
    let fe   = ValExpr . JVar . StrI $ f
        fu   = ValExpr $ JFunc [] [j| `preamble`; return `Stack`[`Sp`]; |]
--        info = ValExpr . JList $ [toJExpr (nfree+1), toJExpr (zDecodeString f)] ++ replicate nfree (toJExpr (2::Int))
{-        objInfo =
          "t" .= Con <>
          "a" .= tag <>
          "i" .= info <>
          gcInfo (nfree+1) [] <>
          "gai" .= ([]::[Int]) -}
    in [j| `decl (StrI f)`;
           `fe` = `fu`;
           `ClosureInfo fe [] (zDecodeString f) (fixedLayout free) (CICon tag) CINoStatic`;
         |]
-}

closureTypes :: JStat
closureTypes = mconcat (map mkClosureType (enumFromTo minBound maxBound)) <> closureTypeName
  where
    mkClosureType :: CType -> JStat
    mkClosureType c = let s = StrI $ map toUpper (show c) ++ "_CLOSURE"
                      in  decl s <> [j| `iex s` = `c` |]
    closureTypeName :: JStat
    closureTypeName = [j| fun closureTypeName c {
                            `map (ifCT c) [minBound..maxBound]`;
                            return "InvalidClosureType";
                          }
                        |]
    ifCT :: JExpr -> CType -> JStat
    ifCT arg ct = [j| if(`arg` === `ct`) { return `show ct`; } |]

-- add some basic stuff locally, to make code run without base, fixme remove this once base builds
{-
coreTypes :: JStat
coreTypes = mconcat $ map (\(x,fr) -> cheatFun x 1 fr)
            [ ("$hs_GHCziTypesziIzh_e", [IntV])
            , ("$hs_GHCziPrimziZLzhz2cUzhZR_e", [])
            , ("$hs_GHCziTypesziIzh_con_e", [IntV])
            , ("$hs_GHCziPrimziZLzhz2cUzhZR_con_e", [])
            ]
-}

hsCall :: JExpr -> JStat
hsCall c = [j| `jstatIf rtsTraceCalls $ logCall c`;
               `jstatIf rtsTraceStack logStack`;
               `c` = `ApplExpr c []`;
             |]

logCall :: JExpr -> JStat
logCall c = [j| logCall(`c`); |]

logStack :: JStat
logStack = [j| logStack(); |]


rts = jsSaturate (Just "RTS") rts'
rtsDebug = renderJs (addDebug $ jsSaturate (Just "RTS") rts')

rtsStr :: String
rtsStr = TL.unpack . displayT . renderPretty 0.8 150 . pretty $ rts

rts' :: JStat
rts' = [j|

// make logging work in browser, node, v8 and spidermonkey, browser also logs to
// <div id="output"> if jquery is detected
var glbl;
fun getGlbl { glbl = this; }
getGlbl();
fun log {
  if(glbl) {
    if(glbl.console && glbl.console.log) {
      glbl.console.log.apply(glbl.console,arguments);
    } else {
      glbl.print.apply(this,arguments);
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

var !stack = [done]; // [];                // thread-local
for(var i=1;i<500;i++) { stack[i] = 0; }
var !heap  = [done]; // [];                // global
for(var i=1;i<300000;i++) { heap[i] = 0; }
var !hpDyn = 10000;             // for h < hpDyn, heap[h] is static, otherwise dynamic
var !hpS = 2;                   // static heap pointer (first two places reserved, see below)
var !hp  = hpDyn;               // dynamic heap pointer
var !hpOld = hpDyn;             // for h < hpOld, heap[h] belongs to the old generation
var !hpForward = [];            // list closures from old generation that point to the new
// var !staticForward = [];        // static closures pointing to dynamic heap
var !initStatic = [];           // we need delayed initialization for static objects, push functions here
                                // to be initialized just before haskell runs

var !sp  = 0;                   // stack pointer
var !mask = 0;                  // mask async exceptions

// gc tuning
var !staticFree = 2000;         // when garbage collecting, try to keep this many free indices for static
var !allocArea  = 500000;       // allocate this many indices before running minor gc
var !hpLim = hpDyn + allocArea; // collect garbage if we go over this limit
var !gcInc = 10;                // run full gc after this many incrementals

var !gcIncCurrent = gcInc;

var !staticThunks    = {};      // funcName -> heapidx map for srefs
var !staticThunksArr = [];      // indices of updatable thunks in static heap

// stg registers
`declRegs`;
`declRets`;

// mutable variables are just datacon-like things that contain one ptr
fun mutvar { `R1` = heap[r1+1]; return stack[sp]; }
`ClosureInfo (jsv "mutvar") [] "mutable cell" (CILayoutPtrs 2 [0]) (CIFun 0 0) CINoStatic`;

// ptr arrays are regular js arrays

// black hole size 2, we need enough space for the ind frame
fun blackhole { throw "<<loop>>"; return 0; }
`ClosureInfo (jsv "blackhole") [] "blackhole" (CILayoutPtrs 2 []) CIBlackhole CINoStatic`;

// really done now
fun done o { return done; }
`ClosureInfo (jsv "done") [R1] "done" (CILayoutPtrs 2 []) (CIFun 0 0) CINoStatic`;

// many primops return bool, and we can cheaply convert javascript bool to 0,1 with |0
// so this is where we store our Bool constructors, hackily
// note: |0 hack removed because it's slow in v8, fixed positions still useful: x?1:0
fun false_e { return stack[sp]; }
`ClosureInfo (jsv "false_e") [] "GHC.Types.False" (CILayoutFixed 1 []) (CICon 1) CINoStatic`;

fun true_e { return stack[sp]; }
`ClosureInfo (jsv "true_e") [] "GHC.Types.True" (CILayoutFixed 1 []) (CICon 2) CINoStatic`;

heap[0] = false_e;
var !$hs_GHCziTypesziFalse = 0;

heap[1] = true_e;
var !$hs_GHCziTypesziTrue = 1;

fun stg_catch a handler {
  `preamble`;
  `adjSp 2`;
  `Stack`[`Sp` - 1] = handler;
  `Stack`[`Sp`] = stg_catch_e;
  `R1` = a;
  return stg_ap_v_fast();
}

fun stg_catch_e {
  `preamble`;
  `adjSpN 2`;
  return `Stack`[`Sp`];
}
`ClosureInfo (jsv "stg_catch_e") [] "exception handler" (CILayoutFixed 2 [PtrV]) (CIFun 0 0) CINoStatic`;


// throw an exception: walk the thread's stack until you find a handler
fun stg_throw e {
  while(sp > 0) {
    var f = stack[sp];
    if(f === stg_catch_e) break;
    var size = f.gtag & 0xff;
    sp = sp - size;
  }
  if(sp > 0) {
    var handler = stack[sp - 1];
    `R1` = handler;
    `R2` = e;
    `adjSpN 2`;
    return stg_ap_pv_fast();
  } else {
    throw "unhandled exception in haskell thread";
  }
}

// reduce result if it's a thunk, follow if it's an ind
// add this to the stack if you want the outermost result
// to always be reduced to whnf, and not an ind
fun reduce {
  `preamble`;
  if(typeof(`Heap`[`R1`].t) !== "undefined" && heap[`R1`].t === `Thunk`) {
    do {
      switch(`Heap`[`R1`].t) {
        case `Thunk`:
          return `Heap`[`R1`];
        case `Ind`:
          `R1` = `Heap`[`R1`+1];
          continue;
        default:
          `adjSpN 1`;
          return `Stack`[`Sp`];
      }
    } while(true);
  } else { // unboxed
    `adjSpN 1`;
    return `Stack`[`Sp`];
  }
}
`ClosureInfo (jsv "reduce") [R1] "reduce" (CILayoutFixed 1 []) (CIFun 0 0) CINoStatic`;

fun gc_check next {
   if(hp > hpLim) {
      if(gcIncCurrent <= 0) {  // full collection, fixme, activate generational later
        hp = gc(heap, hp, stack, sp, next, hpDyn, hpOld, hpForward, staticThunksArr, false);
        hpForward = [];
        gcIncCurrent = gcInc;
      } else {                 // incremental collection
        hp = gc(heap, hp, stack, sp, next, hpDyn, hpOld, hpForward, staticThunksArr, true);
        hpForward = [];
        gcIncCurrent--;
      }
      hpOld = hp;  // fixme do we promote everything to old gen immediately?
      hpLim = hp + allocArea;
//      for(var x=heap.length;x<hpLim+1000;x++) { heap[x] = 0; } // force alloc of heap
   }
}

// set heap/stack object information
fun _setObjInfo o typ name fields a gcinfo regs srefs {
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
fun static_fun f arity name gai {
  if(hpS+1 >= hpDyn) run_gc();
  var h = hpS;
  heap[hpS++] = f;
  return h;
}

// allocate static thunk on heap (after setting closure info)
// we need two positions for static thunks, to have enough room for the update frame
// third position is for storing the original fun again, to be able to revert it
fun static_thunk f {
  if((hpS+3) >= hpDyn) run_gc();
  var h = hpS;
  heap[hpS] = f;
  heap[hpS+1] = 0;
  heap[hpS+2] = f;
  hpS += 3;
  staticThunks[f.n] = h;
  staticThunksArr.push(h);
  return h;
}

// print a closure
fun printcl i {
  var cl = heap[i];
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
  var idx = i+1;
  for(var i=0;i<cl.i.length;i++) {
    r += " ";
    switch(cl.i[i]) {
      case `PtrV`:
        r += "[" + heap[idx] + " :: " + heap[heap[idx]].i[1] + "]";
        idx++;
        break;
      case `VoidV`:
        r += "void";
        break;
      case `DoubleV`:
        r += "(" + heap[idx] + " :: double)";
        idx++;
        break;
      case `IntV`:
        r += "(" + heap[idx] + " :: int)";
        idx++;
        break;
      case `LongV`:
        r += "(" + heap[idx] + "," + heap[idx+1] + " :: long)";
        idx+=2;
        break;
      case `AddrV`:
        r += "(" + heap[idx].length + "," + heap[idx+1] + " :: ptr)";
        idx+=2;
        break;
      default:
        r += "unknown field: " + cl.i[i];
    }
  }
  log(r);
}

fun static_con0 f {
  if(hpS+1 >= hpDyn) run_gc();
  var h = hpS;
  heap[hpS++] = f;
  return h;
}

fun static_con f xs {
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

fun alloc_static n {
  if(hpS+n >= hpDyn) run_gc();
  var h = hpS;
  hpS += n;
  return h;
}

fun init_closure idx f xs {
  heap[idx] = f;
  for(var i=xs.length - 1;i>=0;i--) {
    heap[idx+i+1] = xs[i];
  }
}

fun run_init_static {
  if(initStatic.length == 0) return;
  for(var i=initStatic.length - 1;i>=0;i--) {
    initStatic[i]();
  }
  initStatic = [];
}

// print function to be called and first few registers
fun logCall c {
  var f = c;
  if(c && c.n) f = c.n;
//  log("trampoline calling: " + f + "    " + JSON.stringify([r1,r2,r3,r4,r5,r6,r7,r8]) + "  hp: " + hp + "(l: " + heap.length + ")");
  log("trampoline calling: " + f + "    " + JSON.stringify([printReg r1, printReg r2, printReg r3, printReg r4, printReg r5]) + "  hp: " + hp + "(l: " + heap.length + ")");
  checkDynHeap();
  checkStack();
}

// check that all pointers on the dyn heap indeed refer to some valid pointer target
fun checkDynHeap {
  var idx = hpDyn;
  while(idx < hp) {
    if(typeof(heap[idx]) === 'function') {
      // updated objects are probably not the same size anymore
      while(heap[idx].t === `Blackhole` || heap[idx].t === `Ind`) {
        while(++idx < hp) {
          if(typeof(heap[idx]) === 'function') {
            break;
          }
        }
        if(idx >= hp) return;
      }
      var g = heap[idx].gtag;
      var offset = 1;
      if(g < 0) {
        if(heap[idx].t === `Pap`) { // skip pap objects for now
          g = 0 - (g+1); // ignore object, just size
          log("pap size: " + g);
        } else {
          g = heap[idx+1];
          offset = 2;
        }
      }
      var size = g & 0xff;
      // if(heap[idx].t === `Thunk`) { // thunks on the dynheap are at least size 2
        size = Math.max(size, 2);
      // }
      var ptrs = g >> 8;
      while(ptrs) {
        if(ptrs&1) {
          var ptr = heap[idx+offset];
          if(typeof heap[ptr] !== 'function') {
            dh();
            throw("expected pointer at: " + (idx+offset) + " inspecting: " + heap[idx].n);
          }
        }
        ptrs = ptrs >> 1;
        offset++;
      }
      idx += size;
    } else {
      dh();
      throw("invalid heap object at: " + idx);
    }
  }
}

fun checkStack {
  var idx = sp;
  while(idx >= 0) {
    var f = stack[idx];
    if(typeof(f) === 'function') {
      var tag = stack[idx].gtag;
      var offset = 1;
      if(tag <= 0) {
        tag = stack[idx-1];
        offset = 2;
      }
      var size = tag & 0xff;
      var ptrs = tag >> 8;
      while(ptrs) {
        if(ptrs&1) {
          var ptr = stack[idx-offset];
          if(typeof heap[ptr] !== 'function') {
            dumpStack stack sp;
            dh();
            throw("expected pointer at: stack[" + (idx-offset) + "] inspecting " + stack[idx].n);
          }
        }
        ptrs = ptrs >> 1;
        offset++;
      }
      idx = idx - size;
    } else {
      dumpStack stack sp;
      dh();
      throw("invalid stack object at: " + idx);
    }
  }
}

fun printReg r {
  if(r > 0 && r <= hp && heap[r].n) {
    return (r + ":" + heap[r].n);
  } else {
    return r;
  }
}

// print top stack frame
fun logStack {
  if(typeof stack[sp] === 'undefined') {
    log("warning: invalid stack frame");
    return;
  }
  var size = 0;
  var gt = stack[sp].gtag;
  if(gt === -1) {
    size = stack[sp-1] & 0xff;
  } else {
    size = gt & 0xff;
  }
//  dumpStackTop stack (sp-size+1) sp;
  dumpStackTop stack (sp-size - 2) sp;
  for(var i=Math.max(0,sp-size+1); i <= sp; i++) {
    if(typeof stack[i] === 'undefined') {
      throw "undefined on stack";
    }
  }
}

fun runhs t cb {
  `trace "runhs"`;
  run_init_static();
  stack[0] = done;
  stack[1] = reduce;
  sp = 1;
  var c = heap[t];
  `R1` = t;
  while(c !== done) {
    `replicate 10 (hsCall c)`;
    `hsCall c`;
    gc_check(c);
  }
  while(`R1`.t === `Ind`) { `R1` = heap[`R1`+1]; }
  cb(`R1`);
}

`rtsApply`;
`rtsPrim`;
`closureTypes`;
`garbageCollector`;

fun runio_e {
  `preamble`;
  `R1` = `Heap`[`R1`+1];
  stack[++sp] = stg_ap_v;
  return stg_ap_v;
}
`ClosureInfo (jsv "runio_e") [R1] "runio" (CILayoutFixed 2 [PtrV]) CIThunk CINoStatic`;

fun runio c {
  var h = hp;
  heap[h] = runio_e;
  heap[h+1] = c;
  hp+=2;
  return h;
}

var start = new Date();
fun dumpRes cl {
   printcl cl;
   var end = new Date();
   log("elapsed time: " + (end.getTime()-start.getTime()) + "ms");
}

// fixme move somewhere else

fun ascii s {
    var res = [];
    for(var i=0;i<s.length;i++) {
      res.push(s.charCodeAt(i));
    }
    res.push(0);
    return res;
}

fun localeEncoding {
   ret1 = 0; // offset 0
   return encodeUtf8("UTF-8");
}

fun u_towupper {
  return 0;
}



|]

