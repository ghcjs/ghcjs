{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Gen2.Profiling
  ( initCostCentres
  , enterCostCentreFun
  , setSCC
  , CostCentre
  , CostCentreStack
  , ccsVar
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

import           Control.Lens
import           Data.Monoid
import qualified Data.Text            as T

import           Compiler.JMacro
import           Compiler.JMacro.Base

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

ccsVar :: CostCentreStack -> G Ident
ccsVar ccs
  | noCCSAttached ccs = return $ TxtI "h$CCCS" -- FIXME
  | isCurrentCCS ccs = return $ TxtI "h$CCCS"
  | dontCareCCS == ccs = return $ TxtI "h$CCS_DONT_CARE"
  | otherwise =
      case maybeSingletonCCS ccs of
        Just cc -> singletonCCSVar cc
        Nothing -> pprPanic "ccsVar" (ppr ccs)

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
    return $ TxtI $ T.pack (moduleNameColons (moduleName curModl) ++ "_" ++ zEncodeString label)

enterCostCentreFun :: CostCentreStack -> JExpr -> G JStat
enterCostCentreFun ccs i
  | isCurrentCCS ccs = do
      ccs' <- ccsVar ccs
      return [j| h$enterFunCCS(`ccs'`, `i`.cc); |]
  | otherwise = return mempty -- top-level function, nothing to do

setSCC :: CostCentre -> Bool -> Bool -> G JStat
-- FIXME: ignoring push/tick flags for now
setSCC cc _ _ = do
    ccI <- ccVar cc
    return [j| h$CCCS = h$pushCostCentre(h$CCCS, `ccI`); |]

