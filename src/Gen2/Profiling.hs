{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Gen2.Profiling
  ( initCostCentres
  , enterCostCentreFun
  , enterCostCentreThunk
  , setSCC
  , CostCentre
  , CostCentreStack
  , ccsVar
  , ccsVarJ
  , pushCCSRestore
  , ifProfiling
  , ifProfiling'
  , profiling
  , profStat
  ) where

import           CLabel
import           CostCentre
import           DynFlags
import           Encoding
import           FastString
import           Id
import           Module
import           Outputable           hiding ((<>))
import           SrcLoc

import           Control.Applicative
import           Control.Lens
import           Data.Monoid
import qualified Data.Text            as T

import           Compiler.JMacro
import           Compiler.JMacro.Base

import           Gen2.ClosureInfo
import           Gen2.RtsTypes
import           Gen2.Utils

initCostCentres :: CollectedCCs -> G ()
initCostCentres (local_CCs, _extern_CCs, singleton_CCSs) = do
    mapM_ emitCostCentreDecl local_CCs
    mapM_ emitCostCentreStackDecl singleton_CCSs

emitCostCentreDecl :: CostCentre -> G ()
emitCostCentreDecl cc = do
    dflags  <- use gsDynFlags
    let is_caf = isCafCC cc
        label  = costCentreUserName cc
        modl   = Module.moduleNameString $ moduleName $ cc_mod cc
        loc    = showPpr dflags (costCentreSrcSpan cc)

    var <- ccVar cc
    let js =
          decl var <>
          [j| `var` = h$registerCC(`label`, `modl`, `loc`, `is_caf`); |]
    emitGlobal js

emitCostCentreStackDecl :: CostCentreStack -> G ()
emitCostCentreStackDecl ccs = do
    case maybeSingletonCCS ccs of
      Just cc -> do
        ccs_var <- singletonCCSVar cc
        cc_var  <- ccVar cc
        let js =
              decl ccs_var <>
              [j| `ccs_var` = h$registerCCS(`cc_var`); |]
        emitGlobal js

      Nothing -> pprPanic "emitCostCentreStackDecl" (ppr ccs)

ccsVar :: CostCentreStack -> G (Maybe Ident)
ccsVar ccs
  | noCCSAttached ccs = return Nothing {- pprPanic "no ccs attached" (ppr ccs) -}
  | isCurrentCCS ccs = return $ Just $ TxtI "h$CCCS"
  | dontCareCCS == ccs = return $ Just $ TxtI "h$CCS_DONT_CARE"
  | otherwise =
      case maybeSingletonCCS ccs of
        Just cc -> Just <$> singletonCCSVar cc
        Nothing -> pprPanic "ccsVar" (ppr ccs)

ccsVarJ :: CostCentreStack -> G (Maybe JExpr)
ccsVarJ cc = fmap (\v -> [je| `v` |]) <$> ccsVar cc

singletonCCSVar :: CostCentre -> G Ident
singletonCCSVar cc = do
    curModl <- use gsModule
    let cc_label  = costCentreUserName cc
        ccs_label = cc_label ++ "_ccs"
    return $ TxtI $ T.pack (moduleNameColons (moduleName curModl) ++ "_" ++ zEncodeString ccs_label)

ccVar :: CostCentre -> G Ident
ccVar cc = do
    curModl <- use gsModule
    let is_caf = isCafCC cc
        label  = costCentreUserName cc
    return $ TxtI $ T.pack $ moduleNameColons (moduleName curModl) ++ "_" ++ zEncodeString
      (if isCafCC cc then "CAF_ccs" else label)

enterCostCentreFun :: CostCentreStack -> JStat
enterCostCentreFun ccs
  | isCurrentCCS ccs = [j| h$enterFunCCS(h$CCCS, `R1`.cc); |]
  | otherwise = mempty -- top-level function, nothing to do

enterCostCentreThunk :: JStat
enterCostCentreThunk =
    -- FIXME: function call is for debugging purposes, inline it
    [j| h$enterThunkCCS(`R1`.cc); |]

setSCC :: CostCentre -> Bool -> Bool -> G JStat
-- FIXME: ignoring tick flags for now
setSCC cc _tick True = do
    ccI <- ccVar cc
    return [j| h$CCCS = h$pushCostCentre(h$CCCS, `ccI`); |]
setSCC _cc _tick _push = return mempty

pushCCSRestore :: Ident -> C
pushCCSRestore ccsId =
    push [ [je| function {
                  h$CCCS = `ccsId`;
                  `adjSpN 1`
                  return `Stack`[`Sp`];
                }
              |] ]

ifProfiling :: Monoid m => m -> G m
ifProfiling m = do
    prof <- profiling
    return $ if prof then m else mempty

ifProfiling' :: G () -> G ()
ifProfiling' a = do
    prof <- profiling
    if prof then a else return ()

profiling :: G Bool
profiling = csProf <$> use gsSettings

profStat :: CgSettings -> JStat -> JStat
profStat s e = if csProf s then e else mempty

