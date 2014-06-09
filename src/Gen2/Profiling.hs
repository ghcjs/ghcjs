{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Gen2.Profiling (initCostCentres) where

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
    curModl <- use gsModule
    let is_caf = isCafCC cc
        label  = costCentreUserName cc
        modl   = Module.moduleNameString $ moduleName $ cc_mod cc
        loc    = showPpr dflags (costCentreSrcSpan cc)
        var    = TxtI $ T.pack (moduleNameColons (moduleName curModl) ++ "_" ++ zEncodeString label)

    let js =
          decl var <>
          [j| `var` = h$registerCC(`label`, `modl`, `loc`, `is_caf`); |]
    emitGlobal js
    trace ("emitting: " ++ show (renderJs js)) (return ())

emitCostCentreStackDecl :: CostCentreStack -> G ()
emitCostCentreStackDecl ccs = do
    case maybeSingletonCCS ccs of
      Just cc -> do
        dflags  <- use gsDynFlags
        curModl <- use gsModule
        let cc_label  = costCentreUserName cc
            ccs_label = cc_label ++ "_ccs"
            var       = TxtI $ T.pack (moduleNameColons (moduleName curModl) ++ "_" ++ zEncodeString ccs_label)

        let js =
              decl var <>
              [j| `var` = h$registerCCS(`JVar $ TxtI $ T.pack cc_label`); |]
        emitGlobal js
        trace ("emitting: " ++ show (renderJs js)) (return ())

      Nothing -> pprPanic "emitCostCentreStackDecl" (ppr ccs)

