{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Gen2.Profiling
  ( module CostCentre
  , initCostCentres
  , costCentreLbl
  , costCentreStackLbl
  , ccsVarJ
  , ifProfiling
  , ifProfiling'
  , ifProfilingM
  , profiling
  , profStat
  , setSCC
  , enterCostCentreFun
  , enterCostCentreThunk
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

--------------------------------------------------------------------------------
-- Initialization

initCostCentres :: CollectedCCs -> G ()
initCostCentres (local_CCs, extern_CCs, singleton_CCSs) = do
    mapM_ emitCostCentreDecl local_CCs
    mapM_ emitCostCentreDecl extern_CCs
    mapM_ emitCostCentreStackDecl singleton_CCSs

emitCostCentreDecl :: CostCentre -> G ()
emitCostCentreDecl cc = do
    dflags <- use gsDynFlags
    ccsLbl <- costCentreLbl cc
    let is_caf = isCafCC cc
        label  = costCentreUserName cc
        modl   = Module.moduleNameString $ moduleName $ cc_mod cc
        loc    = showPpr dflags (costCentreSrcSpan cc)
        js     =
          decl ccsLbl <>
          [j| `ccsLbl` = new h$CC(`label`, `modl`, `loc`, `is_caf`); |]
    emitGlobal js

emitCostCentreStackDecl :: CostCentreStack -> G ()
emitCostCentreStackDecl ccs = do
    case maybeSingletonCCS ccs of
      Just cc -> do
        ccsLbl <- singletonCCSLbl cc
        ccLbl  <- costCentreLbl cc
        let js =
              decl ccsLbl <>
              [j| `ccsLbl` = new h$CCS(null, `ccLbl`); |]
        emitGlobal js
      Nothing -> pprPanic "emitCostCentreStackDecl" (ppr ccs)

--------------------------------------------------------------------------------
-- Entering to cost-centres

enterCostCentreFun :: CostCentreStack -> JStat
enterCostCentreFun ccs
  | isCurrentCCS ccs = [j| h$enterFunCCS(h$CCCS, `R1`.cc); |]
  | otherwise = mempty -- top-level function, nothing to do

enterCostCentreThunk :: JStat
enterCostCentreThunk = [j| h$enterThunkCCS(`R1`.cc); |]

setSCC :: CostCentre -> Bool -> Bool -> G JStat
-- FIXME: ignoring tick flags for now
setSCC cc _tick True = do
    ccI <- costCentreLbl cc
    return [j| h$CCCS = h$pushCostCentre(h$CCCS, `ccI`); |]
setSCC _cc _tick _push = return mempty

--------------------------------------------------------------------------------
-- Helpers for generating profiling related things

ifProfiling :: Monoid m => m -> G m
ifProfiling m = do
    prof <- profiling
    return $ if prof then m else mempty

ifProfilingM :: Monoid m => G m -> G m
ifProfilingM m = do
    prof <- profiling
    if prof then m else return mempty

ifProfiling' :: G () -> G ()
ifProfiling' a = do
    prof <- profiling
    if prof then a else return ()

profiling :: G Bool
profiling = csProf <$> use gsSettings

profStat :: CgSettings -> JStat -> JStat
profStat s e = if csProf s then e else mempty

--------------------------------------------------------------------------------
-- Generating cost-centre and cost-centre stack variables

costCentreLbl' :: CostCentre -> G String
costCentreLbl' cc = do
    df      <- use gsDynFlags
    curModl <- use gsModule
    let lbl = show $ runSDoc (ppr cc) (initSDocContext df $ mkCodeStyle CStyle)
    return . zEncodeString $
      moduleNameColons (moduleName curModl) ++ "_" ++ if isCafCC cc then "CAF_ccs" else lbl

costCentreLbl :: CostCentre -> G Ident
costCentreLbl cc = TxtI . T.pack <$> costCentreLbl' cc

costCentreStackLbl' :: CostCentreStack -> G (Maybe String)
costCentreStackLbl' ccs
  | noCCSAttached ccs  = return Nothing -- pprPanic "no ccs attached" (ppr ccs)
  | isCurrentCCS ccs   = return $ Just "h$CCCS"
  | dontCareCCS == ccs = return $ Just "h$CCS_DONT_CARE"
  | otherwise          =
      case maybeSingletonCCS ccs of
        Just cc -> Just <$> singletonCCSLbl' cc
        Nothing -> pprPanic "ccsVar" (ppr ccs)

costCentreStackLbl :: CostCentreStack -> G (Maybe Ident)
costCentreStackLbl ccs = fmap (TxtI . T.pack) <$> costCentreStackLbl' ccs

singletonCCSLbl' :: CostCentre -> G String
singletonCCSLbl' cc = do
    curModl <- use gsModule
    ccLbl   <- costCentreLbl' cc
    let ccsLbl = ccLbl ++ "_ccs"
    return . zEncodeString $ moduleNameColons (moduleName curModl) <> "_" <> ccsLbl

singletonCCSLbl :: CostCentre -> G Ident
singletonCCSLbl cc = TxtI . T.pack <$> singletonCCSLbl' cc

ccsVarJ :: CostCentreStack -> G (Maybe JExpr)
ccsVarJ ccs = fmap (ValExpr . JVar) <$> costCentreStackLbl ccs

