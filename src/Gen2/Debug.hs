{-# LANGUAGE QuasiQuotes, 
             OverloadedStrings #-}

{-
  Add debugging traces to the generated code
-}

module Gen2.Debug where

import Language.Javascript.JMacro
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Monoid
import Data.Text (Text)

import Gen2.Utils
import Gen2.RtsTypes

-- trace all function calls
addDebug :: JStat -> JStat
addDebug s = everywhere (mkT addDebugTrace) s

addDebugTrace (AssignStat vi@(ValExpr (JVar i)) (ValExpr (JFunc as body))) =
    AssignStat vi (ValExpr (JFunc as $ mkTrace i <> body))
addDebugTrace s = s

-- don't trace while you trace
noTrace :: [Text]
noTrace = ["log", "getGlbl", "dumpHeap", "dumpHeapFromTo", "dumpStack", "dumpStackTop", "closureTypeName"]

mkTrace :: Ident -> JStat
mkTrace (TxtI s) | s `elem` noTrace = mempty
                 | otherwise  = [j| log(`s <> ": ["` + r1 + ", " + r2 + ", " + r3 + "] hp: " + hp);
                                    dumpStackTop stack Math.max(0,sp - 4) sp;
//                                    dumpHeapFromTo heap 0 40;
//                                    dumpHeapFromTo heap 10000 100000;
                                  |]

traceErrorCond :: JExpr -> String -> JStat
traceErrorCond e msg = [j| if(`e`) { `traceError msg`; } |]

traceError :: String -> JStat
traceError msg = [j| log ">>>>>> ERROR >>>>>>>>>>>>>>>>>>>";
                     log `msg`;
                     log ">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>";
                     dumpStackTop stack Math.max(0,sp - 4) sp;
                     `mconcat (map traceReg [1..4])`;
                     log "<<<<<< ERROR <<<<<<<<<<<<<<<<<<<";
                     `jsv "debugger; log"`();
                  |]
  where
    traceReg r = let reg = numReg r
                 in  [j| log("r" + `r` + ": " + `reg`);
                         if(`reg` && `reg` >= 0 && `reg` < heap.length) {
                           dumpHeapFromTo heap `reg` (`reg`+5);
                         }
                       |]
