{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

module Gen2.Rts where

import           DynFlags

import           Control.Lens                     hiding ((||=))

import           Data.Array
import           Data.Bits
import           Data.Char                        (toLower, toUpper)
import qualified Data.Map                         as M
import           Data.Monoid
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL

import           Text.PrettyPrint.Leijen.Text     hiding (pretty, (<>))

import           Compiler.JMacro

import           Gen2.ClosureInfo
import           Gen2.Profiling
import           Gen2.Printer
import           Gen2.RtsApply
import           Gen2.RtsTypes
import           Gen2.Utils

garbageCollector :: JStat
garbageCollector =
  [j| fun h$resetRegisters {
        `mconcat $ map resetRegister [minBound..maxBound]`;
      }

      fun h$resetResultVars {
        `mconcat $ map resetResultVar [minBound..maxBound]`;
      }
    |]

resetRegister :: StgReg -> JStat
resetRegister r = [j| `r` = null; |]

resetResultVar :: StgRet -> JStat
resetResultVar r = [j| `r` = null; |]

{-
          use h$c1, h$c2, h$c3, ... h$c24 instead of making objects manually
  so layouts and fields can be changed more easily
 -}
closureConstructors :: CgSettings -> JStat
closureConstructors s =
     declClsConstr "h$c"  ["f"] [jsv "f", jnull, jnull, ji 0]
  <> declClsConstr "h$c0" ["f"] [jsv "f", jnull, jnull, ji 0] -- FIXME: same as h$c, maybe remove one of them?
  <> declClsConstr "h$c1" ["f", "x1"] [jsv "f", jsv "x1", jnull, ji 0]
  <> declClsConstr "h$c2" ["f", "x1", "x2"] [jsv "f", jsv "x1", jsv "x2", ji 0]
  <> mconcat (map mkClosureCon [3..24])
  <> mconcat (map mkDataFill [1..24])
  where
    prof = csProf s
    addCCArg as = map TxtI $ as ++ if prof then ["cc"] else []
    addCCArg' as = as ++ if prof then [TxtI "cc"] else []
    addCCField fs = jhFromList $ fs ++ if prof then [("cc", jsv "cc")] else []

    declClsConstr i as fs = TxtI i ||= jfun (addCCArg as)
            [j| `checkC`;
                var x = `addCCField $ zip ["f", "d1", "d2", "m"] fs`;
                `traceAlloc x`;
                return x;
              |]

    traceAlloc x | csTraceRts s = [j| h$traceAlloc(`x`); |]
                 | otherwise    = mempty

    -- only JSVal can typically contain undefined or null
    -- although it's possible (and legal) to make other Haskell types
    -- to contain JS refs directly
    -- this can cause false positives here
    checkC :: JStat
    checkC {- | csAssertRts s =
                     [j|
                         if(arguments[0] !== h$ghcjszmprimZCGHCJSziPrimziJSVal_con_e) {
                           for(var i=1;i<arguments.length;i++) {
                             if(arguments[i] === null || arguments[i] === undefined) {
                               var msg = "warning: undefined or null in argument: " 
                                      + i + " allocating closure: " + arguments[0].n;
                               h$log(msg);
                               if(console && console.trace) { console.trace(msg); }
                             }
                           }
                         }
                       |] -}
           | otherwise = mempty

    -- h$d is never used for JSVal (since it's only for constructors with
    -- at least three fields, so we always warn here
    checkD | csAssertRts s =
                     [j| for(var i=0;i<arguments.length;i++) {
                           if(arguments[i] === null || arguments[i] === undefined) {
                             var msg = "warning: undefined or null in argument: " + i + " allocating fields";
                             if(console && console.trace) { console.trace(msg); }
                           }
                         }
                       |]
           | otherwise = mempty

    mkClosureCon :: Int -> JStat
    mkClosureCon n = let funName = TxtI $ T.pack ("h$c" ++ show n)
                         vals   = TxtI "f" : addCCArg' (map (TxtI . T.pack . ('x':) . show) [(1::Int)..n])
                         fun    = JFunc vals funBod
                         funBod =
                           [j| `checkC`;
                               var x = `addCCField [("f", jsv "f"), ("d1", jsv "x1"),
                                                   ("d2", toJExpr obj), ("m", ji 0)]`;
                               `traceAlloc x`;
                               return x;
                             |]
                         obj    = JHash . M.fromList . zip
                                    (map (T.pack . ('d':) . show) [(1::Int)..]) $
                                    (map (toJExpr . TxtI . T.pack . ('x':) . show) [2..n])
                     in decl funName <> [j| `funName` = `fun` |]

    mkDataFill :: Int -> JStat
    mkDataFill n = let funName = TxtI $ T.pack ("h$d" ++ show n)
                       ds      = map (T.pack . ('d':) . show) [(1::Int)..n]
                       obj     = JHash . M.fromList . zip ds $ map (toJExpr . TxtI) ds
                       fun     = JFunc (map TxtI ds) [j| `checkD`; return `obj` |]
                   in decl funName <> [j| `funName` = `fun` |]

stackManip :: JStat
stackManip = mconcat (map mkPush [1..32]) <>
             mconcat (map mkPpush [1..255])
  where
    mkPush :: Int -> JStat
    mkPush n = let funName = TxtI $ T.pack ("h$p" ++ show n)
                   as      = map (TxtI . T.pack . ('x':) . show) [1..n]
                   fun     = JFunc as $ [j| `Sp` = `Sp` + `n`; |] <>
                             mconcat (zipWith (\i a -> [j| `Stack`[`Sp`- `n-i`] = `a`; |]) [1..] as)
               in decl funName <> [j| `funName` = `fun`; |]

    -- | partial pushes, based on bitmap, increases Sp by highest bit
    mkPpush :: Integer -> JStat
    mkPpush sig | sig .&. (sig+1) == 0 = mempty -- already handled by h$p
    mkPpush sig = let funName = TxtI $ T.pack ("h$pp" ++ show sig)
                      bits    = bitsIdx sig
                      n       = length bits
                      h       = last bits
                      args    = map (TxtI . T.pack . ('x':) . show) [1..n]
                      fun     = JFunc args $ [j| `Sp` = `Sp` + `h+1` |] <>
                                mconcat (zipWith (\b a -> [j| `Stack`[`Sp`-`h-b`] = `a` |]) bits args)
                   in decl funName <> [j| `funName` = `fun`; |]

bitsIdx :: Integer -> [Int]
bitsIdx n | n < 0 = error "bitsIdx: negative"
          | otherwise = go n 0
  where
    go 0 _ = []
    go m b | testBit m b = b : go (clearBit m b) (b+1)
           | otherwise   = go (clearBit m b) (b+1)

bhStats :: CgSettings -> Bool -> JStat
bhStats s pushUpd =
  let u = if pushUpd then push' s [toJExpr R1, jsv "h$upd_frame"] else mempty
  in  u <> [j|  `R1`.f  = h$blackhole;
                `R1`.d1 = h$currentThread;
                `R1`.d2 = null; // will be filled with waiters array
             |]

bhLneStats :: CgSettings -> JExpr -> JExpr -> JStat
bhLneStats _s p frameSize =
  [j| var v = `Stack`[`p`];
      if(v) {
        `Sp` -= `frameSize`;
        if(v === h$blackhole) {
          return h$throw(h$baseZCControlziExceptionziBasezinonTermination, false);
        } else {
          `R1` = v;
          `Sp` -= `frameSize`;
          return `Stack`[`Sp`];
        }
      } else {
        `Stack`[`p`] = h$blackhole;
        return null;
      }
    |]


updateThunk' :: CgSettings -> JStat
updateThunk' settings =
  if csInlineBlackhole settings
    then bhStats settings True
    else [j| h$bh(); |]

updateThunk :: C
updateThunk = do
  settings <- use gsSettings
  adjPushStack 2 -- update frame size
  return $ (updateThunk' settings)

{-
  Overwrite a single entry object with a special thunk that behaves like a black hole
  (throws a JS exception when entered) but pretends to be a thunk. Useful for making
  sure that the object is not accidentally entered multiple times
 -}
bhSingleEntry :: CgSettings -> JStat
bhSingleEntry _settings = [j| `R1`.f  = h$blackholeTrap;
                              `R1`.d1 = undefined;
                              `R1`.d2 = undefined;
                            |]

-- fixme move somewhere else
declRegs :: JStat
-- fixme prevent holes
declRegs = [j| var !h$regs = []; |]
        <> mconcat (map declReg (enumFromTo R1 R32))
        <> regGettersSetters
        <> loadRegs
    where
      declReg r = (decl . TxtI . T.pack . ("h$"++) . map toLower . show) r <> [j| `r` = 0; |]

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

loadRegs :: JStat
loadRegs = mconcat $ map mkLoad [1..32]
  where
    mkLoad :: Int -> JStat
    mkLoad n = let args   = map (TxtI . T.pack . ("x"++) . show) [1..n]
                   assign = zipWith (\a r -> [j| `r` = `a`; |]) 
                              args (reverse $ take n (enumFrom R1))
                   fname  = TxtI $ T.pack ("h$l" ++ show n)
                   fun    = JFunc args (mconcat assign)
               in decl fname <> [j| `fname` = `fun`; |]

-- assign registers R1 ... Rn
-- assigns Rn first
assignRegs :: CgSettings -> [JExpr] -> JStat
assignRegs _ [] = mempty
assignRegs s xs
  | l <= 32 && not (csInlineLoadRegs s)
      = ApplStat (ValExpr (JVar $ assignRegs'!l)) (reverse xs)
  | otherwise = mconcat . reverse $
      zipWith (\r e -> [j| `r` = `e` |]) (take l $ enumFrom R1) xs
  where
    l = length xs

assignRegs' :: Array Int Ident
assignRegs' = listArray (1,32) (map (TxtI . T.pack . ("h$l"++) . show) [(1::Int)..32])

declRets :: JStat
declRets = mconcat $ map (decl . TxtI . T.pack . ("h$"++) . map toLower . show) (enumFrom Ret1)

trace :: ToJExpr a => a -> JStat
trace e = [j| h$log(`e`);  |]

closureTypes :: JStat
closureTypes = mconcat (map mkClosureType (enumFromTo minBound maxBound)) <> closureTypeName
  where
    mkClosureType :: CType -> JStat
    mkClosureType c = let s = TxtI . T.pack $ "h$" ++ map toUpper (show c) ++ "_CLOSURE"
                      in  decl s <> [j| `iex s` = `c` |]
    closureTypeName :: JStat
    closureTypeName = [j| fun h$closureTypeName c {
                            `map (ifCT c) [minBound..maxBound]`;
                            return "InvalidClosureType";
                          }
                        |]
    ifCT :: JExpr -> CType -> JStat
    ifCT arg ct = [j| if(`arg` === `ct`) { return `show ct`; } |]
{-
hsCall :: JExpr -> JStat
hsCall c = [j| `jstatIf rtsTraceCalls $ logCall c`;
               `jstatIf rtsTraceStack logStack`;
               `c` = `ApplExpr c []`;
             |]

logCall :: JExpr -> JStat
logCall c = [j| h$logCall(`c`); |]

logStack :: JStat
logStack = [j| h$logStack(); |]
-}

rtsDeclsText :: TL.Text
rtsDeclsText = (displayT . renderPretty 0.8 150 . pretty $ rtsDecls) <> "\n"

rtsDecls :: JStat
rtsDecls = jsSaturate (Just "h$RTSD") [j|

            var !h$currentThread   = null;      // thread state object for current thread
var !h$stack           = null;      // stack for the current thread
var !h$sp              = 0;         // stack pointer for the current thread

var !h$initStatic      = [];        // we need delayed initialization for static objects, push functions here
                                    // to be initialized just before haskell runs
var !h$staticThunks    = {};        // funcName -> heapidx map for srefs
var !h$staticThunksArr = [];        // indices of updatable thunks in static heap

// stg registers
`declRegs`;
`declRets`;

|]


-- rtsDebug = renderJs (addDebug $ jsSaturate (Just "h$RTS") rts')

rtsText :: DynFlags -> CgSettings -> TL.Text
rtsText dflags = (<>"\n") . displayT . renderPretty 0.8 150 . pretty . rts dflags

rts :: DynFlags -> CgSettings -> JStat
rts dflags s = jsSaturate (Just "h$RTS") (rts' dflags s)

rts' :: DynFlags -> CgSettings -> JStat
rts' dflags s = [j|

// use these things instead of building objects manually
`closureConstructors s`;

`garbageCollector`;
`stackManip`;

fun h$bh { `bhStats s True`; }
fun h$bh_lne x frameSize { `bhLneStats s x frameSize`; }

fun h$blackhole { throw "oops: entered black hole"; return 0; }
`ClosureInfo "h$blackhole" (CIRegs 0 []) "blackhole" (CILayoutUnknown 2) CIBlackhole noStatic`;

// a black hole that pretends to be a thunk. when compiled with assertions we overwrite
// single entry thunks/functions with this, to trap multiple entry attempts
fun h$blackholeTrap { throw "oops: entered multiple times"; return 0; }
`ClosureInfo "h$blackholeTrap" (CIRegs 0 []) "blackhole" (CILayoutUnknown 2) CIThunk noStatic`;

fun h$done o {
  h$finishThread(h$currentThread);
  return h$reschedule;
}
`ClosureInfo "h$done" (CIRegs 0 [PtrV]) "done" (CILayoutUnknown 0) CIStackFrame noStatic`;

fun h$doneMain_e {
  return h$doneMain();
}
`ClosureInfo "h$doneMain" (CIRegs 0 [PtrV]) "doneMain" (CILayoutUnknown 0) CIStackFrame noStatic`;

// many primops return bool, and we can cheaply convert javascript bool to 0,1 with |0
// so this is where we store our Bool constructors, hackily
// note: |0 hack removed because it's slow in v8, fixed positions still useful: x?1:0
fun h$false_e { return `Stack`[`Sp`]; }
`ClosureInfo "h$false_e" (CIRegs 0 [PtrV]) "GHC.Types.False" (CILayoutFixed 0 []) (CICon 1) noStatic`;

fun h$true_e { return `Stack`[`Sp`]; }
`ClosureInfo "h$true_e" (CIRegs 0 [PtrV]) "GHC.Types.True" (CILayoutFixed 0 []) (CICon 2) noStatic`;

fun h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e { return `Stack`[`Sp`]; }
`ClosureInfo "h$integerzmgmpZCGHCziIntegerziTypeziSzh_con_e" (CIRegs 0 [PtrV]) "GHC.Integer.Type.S#" (CILayoutFixed 1 [IntV]) (CICon 1) noStatic`;
fun h$integerzmgmpZCGHCziIntegerziTypeziJzh_con_e { return `Stack`[`Sp`]; }
`ClosureInfo "h$integerzmgmpZCGHCziIntegerziTypeziJzh_con_e" (CIRegs 0 [PtrV]) "GHC.Integer.Type.J#" (CILayoutFixed 2 [IntV, ObjV]) (CICon 2) noStatic`;

// generic data constructor with 1 non-heapobj field
fun h$data1_e { return `Stack`[`Sp`]; }
`ClosureInfo "h$data1_e" (CIRegs 0 [PtrV]) "data1" (CILayoutFixed 1 [ObjV]) (CICon 1) noStatic`;

// generic data constructor with 2 non-heapobj fields
fun h$data2_e { return `Stack`[`Sp`]; }
`ClosureInfo "h$data2_e" (CIRegs 0 [PtrV]) "data2" (CILayoutFixed 2 [ObjV,ObjV]) (CICon 1) noStatic`;

fun h$con_e { return `Stack`[`Sp`]; };

fun h$catch a handler {
  `adjSp' 3`;
  `Stack`[`Sp` - 2] = h$currentThread.mask;
  `Stack`[`Sp` - 1] = handler;
  `Stack`[`Sp`] = h$catch_e;
  `R1` = a;
  return h$ap_1_0_fast();
}

fun h$noop_e {
  return `Stack`[`Sp`];
}
`ClosureInfo "h$noop_e" (CIRegs 1 [PtrV]) "no-op IO ()" (CILayoutFixed 0 []) (CIFun 1 0) noStatic`;
var !h$noop = `ApplExpr (jsv "h$c0") $ [jsv "h$noop_e"] ++ if csProf s then [jSystemCCS] else []`;

fun h$catch_e {
  `adjSpN' 3`;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$catch_e" (CIRegs 0 [PtrV]) "exception handler" (CILayoutFixed 2 [PtrV,IntV]) CIStackFrame noStatic`;


// function application to one argument
fun h$ap1_e {
  var d1 = `R1`.d1;
  var d2 = `R1`.d2;
  h$bh();
  `profStat s enterCostCentreThunk`;
  `R1` = d1;
  `R2` = d2;
  return h$ap_1_1_fast();
}
`ClosureInfo "h$ap1_e" (CIRegs 0 [PtrV]) "apply1" (CILayoutFixed 2 [PtrV, PtrV]) CIThunk noStatic`;

// function application to two arguments
fun h$ap2_e {
  var d1 = `R1`.d1;
  var d2 = `R1`.d2.d1;
  var d3 = `R1`.d2.d2;
  h$bh();
  `profStat s enterCostCentreThunk`;
  `R1` = d1;
  `R2` = d2;
  `R3` = d3;
  return h$ap_2_2_fast();
}
`ClosureInfo "h$ap2_e" (CIRegs 0 [PtrV]) "apply2" (CILayoutFixed 3 [PtrV, PtrV, PtrV]) CIThunk noStatic`;

// function application to three arguments
fun h$ap3_e {
  var d1 = `R1`.d1;
  var d2 = `R1`.d2.d1;
  var d3 = `R1`.d2.d2;
  var d4 = `R1`.d2.d3;
  h$bh();
  `profStat s enterCostCentreThunk`;
  `R1` = d1;
  `R2` = d2;
  `R3` = d3;
  `R4` = d4;
  return h$ap_3_3_fast();
}
`ClosureInfo "h$ap3_e" (CIRegs 0 [PtrV]) "apply3" (CILayoutFixed 4 [PtrV, PtrV, PtrV, PtrV]) CIThunk noStatic`;

// select first field
fun h$select1_e {
  var t = `R1`.d1;
  `adjSp' 3`;
  `Stack`[`Sp`-2] = `R1`;
  `Stack`[`Sp`-1] = h$upd_frame;
  `Stack`[`Sp`] = h$select1_ret;
  `R1`.f = h$blackhole;
  `R1`.d1 = h$currentThread;
  `R1`.d2 = null;
  `R1` = t;
  return h$ap_0_0_fast();
}
`ClosureInfo "h$select1_e" (CIRegs 0 [PtrV]) "select1" (CILayoutFixed 1 [PtrV]) CIThunk noStatic`;

fun h$select1_ret {
  `R1` = `R1`.d1;
  `adjSpN' 1`;
  return h$ap_0_0_fast();
}
`ClosureInfo "h$select1_ret" (CIRegs 0 [PtrV]) "select1ret" (CILayoutFixed 0 []) CIStackFrame noStatic`;

// select second field of a two-field constructor
fun h$select2_e {
  var t = `R1`.d1;
  `adjSp' 3`;
  `Stack`[`Sp`-2] = `R1`;
  `Stack`[`Sp`-1] = h$upd_frame;
  `Stack`[`Sp`] = h$select2_ret;
  `R1`.f = h$blackhole;
  `R1`.d1 = h$currentThread;
  `R1`.d2 = null;
  `R1` = t;
  return h$ap_0_0_fast();
}
`ClosureInfo "h$select2_e" (CIRegs 0 [PtrV]) "select2" (CILayoutFixed 1 [PtrV]) CIThunk noStatic`;

fun h$select2_ret {
  `R1` = `R1`.d2;
  `adjSpN' 1`;
  return h$ap_0_0_fast();
}
`ClosureInfo "h$select2_ret" (CIRegs 0 [PtrV]) "select2ret" (CILayoutFixed 0 []) CIStackFrame noStatic`;

// throw an exception: unwind the thread's stack until you find a handler
fun h$throw e async {
  //h$log("throwing exception: " + async);
  //h$dumpStackTop(`Stack`,0,`Sp`);
  var origSp = `Sp`;
  var lastBh = null; // position of last blackhole frame
  var f;
  while(`Sp` > 0) {
    //h$log("unwinding frame: " + `Sp`);
    f = `Stack`[`Sp`];
    if(f === null || f === undefined) {
      throw("h$throw: invalid object while unwinding stack");
    }
    if(f === h$catch_e) break;
    if(f === h$atomically_e) {
      if(async) { // async exceptions always propagate
        h$currentThread.transaction = null;
      } else if(!h$stmValidateTransaction()) { // restart transaction if invalid, don't propagate exception
        `push' s [jsv "h$checkInvariants_e"]`;
        return h$stmStartTransaction(`Stack`[`Sp`-2]);
      }
    }
    if(f === h$catchStm_e && !async) break; // catchSTM only catches sync
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
    `stackFrameSize size f`;
    `Sp` = `Sp` - size;
  }
  //h$log("unwound stack to: " + `Sp`);
  //h$dumpStackTop(`Stack`,0,origSp);
  if(`Sp` > 0) {
    var maskStatus = `Stack`[`Sp` - 2];
    var handler = `Stack`[`Sp` - 1];
    if(f === h$catchStm_e) {
      h$currentThread.transaction = `Stack`[`Sp` - 3];
      `adjSpN' 4`;
    } else if(`Sp` > 3) { // don't pop the toplevel handler
      `adjSpN' 3`;
    }
    `R1` = handler;
    `R2` = e;
    if(f !== h$catchStm_e) {  // don't clobber mask in STM?
      if(maskStatus === 0 && `Stack`[`Sp`] !== h$maskFrame && `Stack`[`Sp`] !== h$maskUnintFrame) {
        `Stack`[`Sp`+1] = h$unmaskFrame;
        `adjSp' 1`;
      } else if(maskStatus === 1) {
        `Stack`[`Sp`+1] = h$maskUnintFrame;
        `adjSp' 1`;
      }
      h$currentThread.mask = 2;
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
`ClosureInfo "h$raise_e" (CIRegs 0 [PtrV]) "h$raise_e" (CILayoutFixed 0 []) CIThunk noStatic`;

// a thunk that just raises an asynchronous exception
fun h$raiseAsync_e {
  return h$throw(`R1`.d1, true);
}
`ClosureInfo "h$raiseAsync_e" (CIRegs 0 [PtrV]) "h$raiseAsync_e" (CILayoutFixed 0 []) CIThunk noStatic`;

// a stack frame that raises an exception, this is pushed by
// the scheduler when raising an async exception
fun h$raiseAsync_frame {
  var ex = `Stack`[`Sp`-1];
  `adjSpN' 2`;
  return h$throw(ex,true);
}
`ClosureInfo "h$raiseAsync_frame" (CIRegs 0 []) "h$raiseAsync_frame" (CILayoutFixed 1 []) CIStackFrame noStatic`;

// reduce result if it's a thunk, follow if it's an ind
// add this to the stack if you want the outermost result
// to always be reduced to whnf, and not an ind
fun h$reduce {
  if(`isThunk (toJExpr R1)`) {
    return `R1`.f;
  } else {
    `adjSpN' 1`;
    return `Stack`[`Sp`];
  }
}
`ClosureInfo "h$reduce" (CIRegs 0 [PtrV]) "h$reduce" (CILayoutFixed 0 []) CIStackFrame noStatic`;

var h$gccheckcnt = 0;
fun h$gc_check next {
//  h$log("gc_check: todo");
  if(++h$gccheckcnt > 1000) {
    for(var i=`Sp`+1;i<`Stack`.length;i++) {
      `Stack`[i] = null;
    }
    h$gccheckcnt = 0;
  }
  return 0;
}

fun h$o o typ0 a size regs srefs {
  h$setObjInfo(o,typ0,"",[],a,size,regs,srefs);
}

// set heap/stack object information
fun h$setObjInfo o typ name fields a size regs srefs {
  o.t    = typ;
  o.i    = fields;
  o.n    = name;
  o.a    = a;
  o.r    = regs;        // active registers
  o.s    = srefs;
  o.m    = 0;           // placeholder for uniqe idents
  o.size = size;
}

fun h$static_thunk f {
  // fixme push stuff to restore stuff here
  var h = `jhFromList $ [("f", f), ("d1", jnull), ("d2", jnull), ("m", ji 0)]
                        ++ if csProf s then [("cc", jSystemCCS)] else []`;
  h$CAFs.push(h);
  h$CAFsReset.push(f);
  return h;
}

// print a closure
// fixme, property access here might be closure compiler incompatible
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
      case `RtsObjV`:
        r += "(" + d["d"+idx].toString() + " :: RTS object)";
        idx++;
        break;
      default:
        r += "unknown field: " + cl.i[i];
    }
  }
  h$log(r);
}

fun h$init_closure c xs {
  c.m = 0;
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

fun h$runInitStatic {
  if(h$initStatic.length == 0) return;
  for(var i=h$initStatic.length - 1;i>=0;i--) {
    h$initStatic[i]();
  }
  h$initStatic = [];
}

fun h$checkStack f {
  // some code doesn't write a stack frame header when called immediately
  if(f.t === `StackFrame`) `Stack`[`Sp`] = f;

  var idx = `Sp`;
  while(idx >= 0) {
    f = `Stack`[idx];
    var size, offset;
    if(typeof(f) === 'function') {
      if(f === h$ap_gen) {
        size = (`Stack`[idx - 1] >> 8) + 2;
        offset = 2;
      } else {
        var tag = `Stack`[idx].size;
        if(tag <= 0) {
          size = `Stack`[idx-1];
          offset = 2;
        } else {
          size = (tag & 0xff) + 1;
          offset = 1;
        }
      }
//      if(size < 1) throw("invalid stack frame size at: stack[" + idx + "], frame: " +`Stack`[idx].n);
//        h$log("checking frame: " + `Stack`[idx].n + " size " + size);
//      if(f !== h$returnf && f !== h$restoreThread) {
//        for(var i=0;i<size-offset;i++) {
//          if(typeof `Stack`[idx-offset-i] === 'function') {
//            h$dumpStackTop `Stack` 0 `Sp`;
//            throw("unexpected function in frame at: " + idx + " " + `Stack`[idx].n);
//          }
//        }
//      }
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
    if(typeof(r.f) !== 'function') {
      return "dodgy object";
    } else if(r.f.t === `Blackhole` && r.x) {
      return ("blackhole: -> " + h$printReg({ f: r.x.x1, d: r.d1.x2 }) + ")");
    } else {
      return ((r.alloc ? r.alloc + ': ' : '') + r.f.n + " (" + h$closureTypeName(r.f.t) + ", " + r.f.a + ")");
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
    h$log("warning: invalid stack frame");
    return;
  }
  var size = 0;
  var gt = `Stack`[`Sp`].size;
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

`rtsApply dflags s`;
// rtsPrim
`closureTypes`;

fun h$runio_e {
  `R1` = `R1`.d1;
  `Stack`[++`Sp`] = h$ap_1_0;
  return h$ap_1_0;
}
`ClosureInfo "h$runio_e" (CIRegs 0 [PtrV]) "runio" (CILayoutFixed 1 [PtrV]) CIThunk noStatic`;

fun h$runio c {
  return h$c1(h$runio_e, c);
}

fun h$flushStdout_e {
  `R1` = h$baseZCGHCziIOziHandlezihFlush;
  `R2` = h$baseZCGHCziIOziHandleziFDzistdout;
  return h$ap_1_1_fast();
}
`ClosureInfo "h$flushStdout_e" (CIRegs 0 []) "flushStdout" (CILayoutFixed 0 []) CIThunk noStatic`;
var !h$flushStdout = h$static_thunk(h$flushStdout_e);

var h$start = new Date();
fun h$dumpRes cl {
   h$printcl cl;
   var end = new Date();
   h$log("elapsed time: " + (end.getTime()-h$start.getTime()) + "ms");
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
        start = Math.max(start,0);
        for(var i=start;i<=sp;i++) {
           var s = stack[i];
           if(s && s.n) {
             h$log("stack[" + i + "] = " + s.n);
           } else {
             if(s === null) {
               h$log("stack[" + i + "] = null WARNING DANGER");
             } else if(typeof s === 'object' && s !== null && s.hasOwnProperty("f") && s.hasOwnProperty("d1") && s.hasOwnProperty("d2")) {
               if(typeof(s.f) !== 'function') {
                 h$log("stack[" + i + "] = WARNING: dodgy object");
               } else {
                 if(s.d1 === undefined) { h$log("WARNING: stack[" + i + "] d1 undefined"); }
                 if(s.d2 === undefined) { h$log("WARNING: stack[" + i + "] d2 undefined"); }
                 if(s.f.t === `Blackhole` && s.d1 && s.d1.x1 && s.d1.x1.n) {
                   h$log("stack[" + i + "] = blackhole -> " + s.d1.x1.n);
                 } else {
                   h$log("stack[" + i + "] = -> " + (s.alloc ? s.alloc + ': ' : '') + s.f.n + " (" + h$closureTypeName(s.f.t) + ", a: " + s.f.a + ")");
                 }
               }
             } else if(h$isInstanceOf(s,h$MVar)) {
               var val = s.val ===
                 null ? " empty"
                      : " value -> " + (typeof s.val === 'object' ? s.val.f.n + " (" + h$closureTypeName(s.val.f.t) + ", a: " + s.val.f.a + ")" : s.val);
               h$log("stack[" + i + "] = MVar " + val);
             } else if(h$isInstanceOf(s,h$MutVar)) {
               h$log("stack[" + i + "] = IORef -> " + (typeof s.val === 'object' ? (s.val.f.n + " (" + h$closureTypeName(s.val.f.t) + ", a: " + s.val.f.a + ")") : s.val));
             } else if(typeof s === 'object') {
               h$log("stack[" + i + "] = " + h$collectProps(s).substring(0,50));
             } else if(typeof s === 'function') {
               var re = new RegExp("([^\\n]+)\\n(.|\\n)*");
               h$log("stack[" + i + "] = " + (""+s).substring(0,50).replace(re,"$1"));
             } else {
               h$log("stack[" + i + "] = " + (""+s).substring(0,50));
             }
          }
        }
     }

// check that a haskell heap object is what we expect:
// f is a haskell entry function
// d exists, but might be null, if it isn't, warn for any undefined/null fields or fields with unfamiliar names
fun h$checkObj obj {
  if(typeof obj === 'boolean' || typeof obj === 'number') { return; }
  if(!obj.hasOwnProperty("f") || obj.f === null || obj.f === undefined || obj.f.a === undefined || typeof obj.f !== 'function') {
    h$log("h$checkObj: WARNING, something wrong with f:");
    h$log((""+obj).substring(0,200));
    h$log(h$collectProps(obj));
    h$log(typeof obj.f);
  }
  if(!obj.hasOwnProperty("d1") || obj.d1 === undefined) {
    h$log("h$checkObj: WARNING, something wrong with d1:");
    h$log((""+obj).substring(0,200));
  } else if(!obj.hasOwnProperty("d2") || obj.d2 === undefined) {
    h$log("h$checkObj: WARNING, something wrong with d2:");
    h$log((""+obj).substring(0,200));
  } else if(obj.d2 !== null && typeof obj.d2 === 'object' && obj.f.size !== 2) {
    var d = obj.d2;
    for(var p in d) {
      if(d.hasOwnProperty(p)) {
        if(p.substring(0,1) != "d") {
          h$log("h$checkObj: WARNING, unexpected field name: " + p);
          h$log((""+obj).substring(0,200));
        }
        if(d[p] === undefined) {
          h$log("h$checkObj: WARNING, undefined field detected: " + p);
          h$log((""+obj).substring(0,200));
        }
//        if(d[p] === null) {
//          h$log("h$checkObj: WARNING, null field detected: " + p);
//          h$log((""+obj).substring(0,200));
//        }
      }
    }
    switch(obj.f.size) {
      case 6: if(d.d5 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d5"); }
      case 5: if(d.d4 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d4"); }
      case 4: if(d.d3 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d3"); }
      case 3: if(d.d2 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d2"); }
              if(d.d1 === undefined) { h$log("h$checkObj: WARNING, undefined field detected: d1"); }
      default: d = obj.d2; // dummy
    }
  }
}

fun h$traceForeign f as {
  if(`not (csTraceForeign s)`) { return; }
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
  h$log("ffi: " + f + "(" + bs.join(",") + ")");
}

// the scheduler pushes this frame when suspending a thread that
// has not called h$reschedule explicitly
fun h$restoreThread {
  var f         = `Stack`[`Sp`-2];
  var frameSize = `Stack`[`Sp`-1];
//  h$log("restoreThread " + h$currentThread.tid + " sp: " + h$sp + " frame size: " + frameSize);
  var nregs = frameSize - 3;
  for(var i=1;i<=nregs;i++) {
    h$setReg(i, `Stack`[`Sp`-2-i]);
  }
  `Sp` = `Sp` - frameSize;
  return f;
}
`ClosureInfo "h$restoreThread" (CIRegs 0 []) "restoreThread" CILayoutVariable CIStackFrame noStatic`;

// return a closure in the stack frame to the next thing on the stack
fun h$return {
  `R1` = `Stack`[`Sp`-1];
//  h$log("h$return, returning: " + `R1`.f.n);
  `adjSpN' 2`;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$return" (CIRegs 0 []) "return" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic`;

// return a function in the stack frame for the next call
fun h$returnf {
  var r = `Stack`[`Sp`-1];
//  h$log("h$returnf, returning: " + r.n);
  `adjSpN' 2`;
  return r;
}
// fixme, check that this is only used when R1 is active
`ClosureInfo "h$returnf" (CIRegs 0 [PtrV]) "returnf" (CILayoutFixed 1 [ObjV]) CIStackFrame noStatic`;

// return this function when the scheduler needs to come into action
// (yield, delay etc), returning thread needs to push all relevant
// registers to stack frame, thread will be resumed by calling the stack top
fun h$reschedule {
  return h$reschedule;
}
// fixme check registers
`ClosureInfo "h$reschedule" (CIRegs 0 []) "reschedule" (CILayoutFixed 0 []) CIThunk noStatic`;

// carefully suspend the current thread, looking at the
// function that would be called next
fun h$suspendCurrentThread next {
  `assertRts s (next |!== (TxtI "h$reschedule")) ("suspend called with h$reschedule"::String)`;
  if(next === h$reschedule) { throw "suspend called with h$reschedule"; }
  // some stack calls do not write the function to the stack top as an optimization
  // do it here
  if(next.t === `StackFrame`) `Stack`[`Sp`] = next;
  if(`Stack`[`Sp`] === h$restoreThread || next === h$return) {
    h$currentThread.sp = `Sp`;
    return;
  }
  var nregs;
  var skipregs = 0;
  var t = next.t;
  // pap arity
  if(t === `Pap`) {
    nregs = ((`papArity (toJExpr R1)`) >> 8) + 1;
  } else if(t === `Fun` || t === `StackFrame`) {
    // for normal functions, the number active registers is in the .r proprty
    nregs    = next.r >> 8;
    skipregs = next.r & 0xff;
  } else {
    nregs = 1;  // Thunk, Con, Blackhole only have R1
  }
  // h$log("suspending: " + `Sp` + " nregs: " + nregs);
  `Sp` = `Sp`+nregs+skipregs+3;
  var i;
  for(i=1;i<=skipregs;i++) {
    `Stack`[`Sp`-2-i] = null;
  }
  for(i=skipregs+1;i<=nregs+skipregs;i++) {
    `Stack`[`Sp`-2-i] = h$getReg(i);
  }
  `Stack`[`Sp`-2] = next;
  `Stack`[`Sp`-1] = nregs+skipregs+3;
  `Stack`[`Sp`]   = h$restoreThread;
  h$currentThread.sp = `Sp`;
}

// debug thing, insert on stack to dump current result, should be boxed
fun h$dumpRes {
  h$log("h$dumpRes result: " + `Stack`[`Sp`-1]);
  h$log(`R1`);
  h$log(h$collectProps(`R1`));
  if(`R1`.f && `R1`.f.n) { h$log("name: " + `R1`.f.n); }
  if(`R1`.hasOwnProperty('d1')) { h$log("d1: " + `R1`.d1); }
  if(`R1`.hasOwnProperty('d2')) { h$log("d2: " + `R1`.d2); }
  if(`R1`.f) {
    var re = new RegExp("([^\\n]+)\\n(.|\\n)*");
    h$log("function: " + (""+`R1`.f).substring(0,50).replace(re,"$1"));
  }
  `adjSpN' 2`;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$dumpRes" (CIRegs 0 [PtrV]) "dumpRes" (CILayoutFixed 1 [ObjV]) CIThunk noStatic`;

// resume an interrupted computation, the stack
// we need to push is in d1, restore frame should
// be there
fun h$resume_e {
  //h$logSched("resuming computation: " + `R1`.d2);
  //h$logStack();
  var ss = `R1`.d1;
  `updateThunk' s`;
  for(var i=0;i<ss.length;i++) {
    `Stack`[`Sp`+1+i] = ss[i];
  }
  //h$dumpStackTop(`Stack`,`Sp`,`Sp`+ss.length);
  `Sp`=`Sp`+ss.length;
  `R1` = null;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$resume_e" (CIRegs 0 [PtrV]) "resume" (CILayoutFixed 0 []) CIThunk noStatic`;

fun h$unmaskFrame {
  //log("h$unmaskFrame: " + h$threadString(h$currentThread));
  h$currentThread.mask = 0;
  `adjSpN' 1`;
  // back to scheduler to give us async exception if pending
  if(h$currentThread.excep.length > 0) {
    `push' s [toJExpr R1, jsv "h$return"]`;
    return h$reschedule;
  } else {
    return `Stack`[`Sp`];
  }
}
`ClosureInfo "h$unmaskFrame" (CIRegs 0 [PtrV]) "unmask" (CILayoutFixed 0 []) CIStackFrame noStatic`;

fun h$maskFrame {
  //h$log("h$maskFrame: " + h$threadString(h$currentThread));
  h$currentThread.mask = 2;
  `adjSpN' 1`;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$maskFrame" (CIRegs 0 [PtrV]) "mask" (CILayoutFixed 0 []) CIStackFrame noStatic`;

fun h$maskUnintFrame {
  //h$log("h$maskUnintFrame: " + h$threadString(h$currentThread));
  h$currentThread.mask = 1;
  `adjSpN' 1`;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$maskUnintFrame" (CIRegs 0 [PtrV]) "maskUnint" (CILayoutFixed 0 []) CIStackFrame noStatic`;

// async ffi results are returned in R1 = { f: ... d1: [array of values], d2: null }
fun h$unboxFFIResult {
  var d = `R1`.d1;
  for(var i=0;i<d.length;i++) {
    h$setReg(i+1, d[i]);
  }
  `adjSpN' 1`;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$unboxFFIResult" (CIRegs 0 [PtrV]) "unboxFFI" (CILayoutFixed 0 []) CIStackFrame noStatic`;

// for non-strict things that are represented as an unboxed value:
// 1. enumerations
// 2. one-constructor types that have only one field that maps to a JS primitive
fun h$unbox_e {
  `R1` = `R1`.d1;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$unbox_e" (CIRegs 0 [PtrV]) "unboxed value" (CILayoutFixed 1 [DoubleV]) CIThunk noStatic`;

fun h$retryInterrupted {
  var a = `Stack`[`Sp`-1];
  `adjSpN' 2`;
  return a[0].apply(this, a.slice(1));
}
`ClosureInfo "h$retryInterrupted" (CIRegs 0 [ObjV]) "retry interrupted operation" (CILayoutFixed 1 [ObjV]) CIStackFrame noStatic`;

// STM support

fun h$atomically_e {
  if(h$stmValidateTransaction()) {
    h$stmCommitTransaction();
    `adjSpN' 2`;
    return `Stack`[`Sp`];
  } else {
    `push' s [jsv "h$checkInvariants_e"]`;
    return h$stmStartTransaction(`Stack`[`Sp`-2]);
  }
}
`ClosureInfo "h$atomically_e" (CIRegs 0 [PtrV]) "atomic operation" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic`;

fun h$checkInvariants_e {
  `adjSpN' 1`;
  return h$stmCheckInvariants();
}
`ClosureInfo "h$checkInvariants_e" (CIRegs 0 [PtrV]) "check transaction invariants" (CILayoutFixed 0 []) CIStackFrame noStatic`;

fun h$stmCheckInvariantStart_e {
  var t   = `Stack`[`Sp`-2];
  var inv = `Stack`[`Sp`-1];
  var m   = h$currentThread.mask;
  `adjSpN' 3`;
  var t1 = new h$Transaction(inv.action, t);
  t1.checkRead = new h$Set();
  h$currentThread.transaction = t1;
  `push' s [t1, m, jsv "h$stmInvariantViolatedHandler", jsv "h$catchStm_e"]`;
  `R1` = inv.action;
  return h$ap_1_0_fast();
}
`ClosureInfo "h$stmCheckInvariantStart_e" (CIRegs 0 []) "start checking invariant" (CILayoutFixed 2 [ObjV, RtsObjV]) CIStackFrame noStatic`

fun h$stmCheckInvariantResult_e {
  var inv = `Stack`[`Sp`-1];
  `adjSpN' 2`;
  // add all variables read by the check to the tvars
  h$stmUpdateInvariantDependencies(inv);
  h$stmAbortTransaction();
  return `Stack`[`Sp`];
}
`ClosureInfo "h$stmCheckInvariantResult_e" (CIRegs 0 [PtrV]) "finish checking invariant" (CILayoutFixed 1 [ObjV]) CIStackFrame noStatic`

// update invariant TVar dependencies and rethrow exception
// handler must be pushed above h$stmCheckInvariantResult_e frame
fun h$stmInvariantViolatedHandler_e {
  if(`Stack`[`Sp`] !== h$stmCheckInvariantResult_e) {
    throw "h$stmInvariantViolatedHandler_e: unexpected value on stack";
  }
  var inv = `Stack`[`Sp`-1];
  `adjSpN' 2`;
  h$stmUpdateInvariantDependencies(inv);
  h$stmAbortTransaction();
  return h$throw(`R2`, false);
}
`ClosureInfo "h$stmInvariantViolatedHandler_e" (CIRegs 0 [PtrV]) "finish checking invariant" (CILayoutFixed 0 []) (CIFun 2 1) noStatic`;

var !h$stmInvariantViolatedHandler =
  `ApplExpr (jsv "h$c") $ [jsv "h$stmInvariantViolatedHandler_e"] ++ if csProf s then [jSystemCCS] else []`;

fun h$stmCatchRetry_e {
  `adjSpN' 2`;
  h$stmCommitTransaction();
  return `Stack`[`Sp`];
}
`ClosureInfo "h$stmCatchRetry_e" (CIRegs 0 [PtrV]) "catch retry" (CILayoutFixed 1 [PtrV]) CIStackFrame noStatic`;

fun h$catchStm_e {
  `adjSpN' 4`;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$catchStm_e" (CIRegs 0 [PtrV]) "STM catch" (CILayoutFixed 3 [ObjV,PtrV,ObjV]) CIStackFrame noStatic`;

fun h$stmResumeRetry_e {
  if(`Stack`[`Sp`-2] !== h$atomically_e) {  // must be pushed just above atomically
    throw("h$stmResumeRetry_e: unexpected value on stack");
  }
  var blocked = `Stack`[`Sp`-1];
  `adjSpN' 2`;
  `push' s [jsv "h$checkInvariants_e"]`;
  h$stmRemoveBlockedThread(blocked, h$currentThread);
  return h$stmStartTransaction(`Stack`[`Sp`-2]);
}
`ClosureInfo "h$stmResumeRetry_e" (CIRegs 0 [PtrV]) "resume retry" (CILayoutFixed 0 []) CIStackFrame noStatic`;

fun h$lazy_e {
  var x = h$r1.d1();
  h$bh();
  `profStat s enterCostCentreThunk`;
  h$r1 = x;
  return h$stack[h$sp];
}
`ClosureInfo "h$lazy_e" (CIRegs 0 [PtrV]) "generic lazy value" (CILayoutFixed 0 []) CIThunk noStatic`;

|] <>
  -- Top-level statements to generate only in profiling mode
  profStat s [j|

fun h$setCcs_e {
  h$restoreCCS(`Stack`[`Sp`-1]);
  `adjSpN' 2`;
  return `Stack`[`Sp`];
}
`ClosureInfo "h$setCcs_e" (CIRegs 0 [PtrV]) "set cost centre stack" (CILayoutFixed 1 [ObjV]) CIStackFrame noStatic`;

|]
