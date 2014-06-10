{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Gen2.Profiling
  ( initCostCentres
  , CostCentre
  , CostCentreStack
  , ccsVar
  ) where

import           CLabel
import           CostCentre
import           DynFlags
import           Encoding
import           FastString
import           Module
import           Outputable           hiding ((<>))
import           SrcLoc

import           Control.Lens
import           Data.Monoid
import qualified Data.Text            as T

import           Compiler.JMacro
import           Compiler.JMacro.Base

import           Gen2.ClosureInfo
import           Gen2.RtsTypes
import           Gen2.Utils

import           Debug.Trace

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
    trace ("emitting: " ++ show (renderJs js)) (return ())

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
        trace ("emitting: " ++ show (renderJs js)) (return ())

      Nothing -> pprPanic "emitCostCentreStackDecl" (ppr ccs)

ccsVar :: CostCentreStack -> G Ident
ccsVar ccs
  | noCCSAttached ccs = return $ TxtI "h$CCCS" -- FIXME
  | isCurrentCCS ccs = return $ TxtI "h$CCCS"
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
