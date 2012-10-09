{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Gen2.Rts where

import Language.Javascript.JMacro
import Language.Javascript.JMacro.Types

import Gen2.Utils
import Gen2.RtsPrim
import Gen2.RtsApply
import Gen2.RtsTypes
import Gen2.GC
import Gen2.Debug
import Gen2.RtsSettings

import Data.Monoid
import qualified Data.List as L
import Data.Bits
import Data.Char (toLower, toUpper)

import Encoding

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

declRegs = mconcat (map declReg (enumFrom R1))
    where
      declReg r = (decl . StrI . map toLower . show) r <> [j| `r` = 0; |]

trace :: ToJExpr a => a -> JStat
trace e = [j| log(`e`);  |]

-- generate function thingie
cheatFun :: String -> Int -> Int -> JStat
cheatFun f tag nfree =
    let fe   = ValExpr . JVar . StrI $ f
        fu   = ValExpr $ JFunc [] [j| return stack[sp]; |]
        info = ValExpr . JList $ [toJExpr (nfree+1), toJExpr (zDecodeString f)] ++ replicate nfree (toJExpr (2::Int))
        objInfo =
          "t" .= Con <>
          "a" .= tag <>
          "i" .= info <>
          gcInfo (nfree+1) [] <>
          "gai" .= ([]::[Int])
    in [j| `decl (StrI f)`;
           `fe` = `fu`;
           `setObjInfo fe objInfo`;
         |]

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
coreTypes :: JStat
coreTypes = mconcat $ map (\x -> cheatFun x 1 1)
            [ "$hs_GHCziTypesziIzh_e"
            , "$hs_GHCziPrimziZLzhz2cUzhZR_e"
            , "$hs_GHCziTypesziIzh_con_e"
            , "$hs_GHCziPrimziZLzhz2cUzhZR_con_e"
            ]

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
var !staticForward = [];        // static closures pointing to dynamic heap
var !initStatic = [];           // we need delayed initialization for static objects, push functions here
                                // to be initialized just before haskell runs

var !sp  = 0;                   // stack pointer

// gc tuning
var !staticFree = 2000;         // when garbage collecting, try to keep this many free indices for static
var !allocArea  = 250000;       // allocate this many indices before running minor gc
var !hpLim = hpDyn + allocArea; // collect garbage if we go over this limit
var !gcInc = 10;                // run full gc after this many incrementals

var !gcIncCurrent = gcInc;

// stg registers
`declRegs`;

// black hole size 2, we need enough space for the ind frame
fun blackhole { throw "<<loop>>"; return 0; }
`setObjInfo (jsv "blackhole") $
  "i"   .= [ji 2, jstr "blackhole"] <>
  gcInfo 2 [] <>
  "gai" .= ([]::[Int]) <>
  "a"   .= ji 0 <>
  "t"   .= Blackhole
`;


// really done now
fun done o { return done; }
`setObjInfo (jsv "done") $
  "i"   .= [ji 1, jstr "done"] <>
  gcInfo 1 [] <>
  "gai" .= ([1]::[Int]) <>
  "a"   .= ji 0 <>
  "t"   .= Fun
`;

// many primops return bool, and we can cheaply convert javascript bool to 0,1 with |0
// so this is where we store our Bool constructors, hackily
// note: |0 hack removed because it's slow in v8, fixed positions still useful: x?1:0
fun false_e { return stack[sp]; }
`setObjInfo (jsv "false_e") $
  "i"    .= [ji 1, jstr "GHC.Types.False"] <>
  "gtag" .= ji 1 <>
  "gai"  .= ([]::[Int]) <>
  "gi"   .= [ji 1] <>
  "a"    .= ji 1 <>
  "t"    .= Con
`;

fun true_e { return stack[sp]; }
`setObjInfo (jsv "true_e") $
  "i"    .= [ji 1, jstr "GHC.Types.False"] <>
  "gtag" .= ji 1 <>
  "gai"  .= ([]::[Int]) <>
  "gi"   .= [ji 1] <>
  "a"    .= ji 2 <>
  "t"    .= Con
`;

heap[0] = false_e;
var !$hs_GHCziTypesziFalse = 0;

heap[1] = true_e;
var !$hs_GHCziTypesziTrue = 1;

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
`setObjInfo (jsv "reduce") $
  "i"   .= [ji 1, jstr "reduce"] <>
  "gai" .= [ji 1] <>  -- is this right?
  gcInfo 1 [] <>
  "a"   .= ji 0 <>
  "t"   .= Fun
`;

fun gc_check next {
   if(hp > hpLim) {
      if(gcIncCurrent <= 0) {  // full collection, fixme, activate generational later
        hp = gc(heap, hp, stack, sp, next, hpDyn, hpOld, true);
        hpForward = [];
        gcIncCurrent = gcInc;
      } else {                 // incremental collection
        hp = gc(heap, hp, stack, sp, next, hpDyn, hpOld, false);
        gcIncCurrent--;
      }
      hpOld = hp;
      hpLim = hp + allocArea;
      for(var x=heap.length;x<hpLim+1000;i++) { heap[0] = 0; } // force alloc of heap
   }
}

fun static_fun f arity name gai {
  if(hpS+1 >= hpDyn) run_gc();
  var h = hpS;
  heap[hpS++] = f;
  `setObjInfo f $
    "a"   .= arity <>
    "t"   .= Fun <>
    "i"   .= [ji 1, name] <>
    gcInfo 1 [] <>
    "gai" .= gai
  `;
  return h;
}

// set heap/stack object information
fun _objInfo o o0 {
    `assertRts (isNumber (o0|."t"))    "object type must be a number"`;
    `assertRts (isNumber (o0|."gtag")) "gtag must be a number"`;
    `assertRts (isNumber (o0|."a"))    "constructor/arity tag must be a number"`;
    `assertRts (isArray  (o0|."i"))    "object information must be an array"`;
    `assertRts (isArray  (o0|."gi"))   "gc information list must be an array"`;
    `assertRts (isArray  (o0|."gai"))  "argument type list must be an array"`;
    o.i   = o0.i;
    o.gi  = o0.gi;
    o.gai = o0.gai;
    o.a   = o0.a;
    o.t   = o0.t;
    o.gtag = o0.gtag;
}

// we need two positions for static thunks, to have enough room for the update frame
fun static_thunk f name {
  if((hpS+2) >= hpDyn) run_gc();
  var h = hpS;
  heap[hpS] = f;
  hpS += 2;
  `setObjInfo f $
    "t"   .= Thunk <>
    "i"   .= [ji 2, name] <>
     gcInfo 2 [] <>
    "a"   .= ji 0 <>
    "gai" .= [ji 1]
  `;
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
  r += " :: " + cl.i[1] + " ->";
  var idx = i+1;
  for(var i=2;i<cl.i.length;i++) {
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
      case `ArrV`:
        r += "(" + heap[idx].length + "," + heap[idx+1] + " :: ptr)";
        idx+=2;
        break;
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
  if(c.i) f = c.i[1];
  log("trampoline calling: " + f + "    " + JSON.stringify([r1,r2,r3,r4,r5,r6,r7,r8]) + "  hp: " + hp + "(l: " + heap.length + ")");
}

// print top stack frame
fun logStack {
  var size = 0;
  var gt = stack[sp].gtag;
  if(gt === -1) {
    size = stack[sp-1] & 0xff;
  } else {
    size = gt & 0xff;
  }
  dumpStackTop stack (sp-size+1) sp;
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
`coreTypes`;

fun runio_e {
  `preamble`;
  `R1` = `Heap`[`R1`+1];
  return stg_ap_v_fast();
}
`setObjInfo (jsv "runio_e") $
   "t"   .= Thunk <>
   "i"   .= [ji 2, jstr "runio", 0] <>
    gcInfo 1 [] <>
   "a"   .= ji 0 <>
   "gai" .= [ji 0]
`;

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

|]

