{-
   some instances for printing the StgSyn AST in Haskell syntax.
-}

{-# LANGUAGE StandaloneDeriving, FlexibleInstances #-}

module Gen2.StgAst where

import StgSyn
import Outputable
import Type
import Module
import PrimOp
import ForeignCall
import UniqSet
import DataCon
import UniqFM
import DynFlags
import CostCentre
import CoreSyn
import Literal
import BasicTypes

showPpr' a = showPpr (defaultDynFlags undefined) a
showSDoc' a = showSDoc (defaultDynFlags undefined) a

instance Show CostCentre where show _ = "CostCentre"
instance Show CostCentreStack where show _ = "CostCentreStack"
instance Show StgBinderInfo where show _ = "StgBinderInfo"
instance Show Module where show = showPpr'
instance Show (UniqFM a) where show _ = "UniqSet"
instance Show Type where show = showPpr'
instance Show AltType where show = showPpr'
instance Show SRT where show _ = "SRT"
instance Show PrimCall where show = showPpr'
instance Show ForeignCall where show = showPpr'
instance Show DataCon where show = showPpr'
instance Show Var where show = showPpr'

deriving instance Show UpdateFlag

deriving instance Show Literal
deriving instance Show FunctionOrData
deriving instance Show PrimOp
deriving instance Show AltCon
deriving instance Show StgExpr
deriving instance Show StgBinding
deriving instance Show StgRhs
deriving instance Show StgOp

instance Show (GenStgArg Var) where
  show a@(StgVarArg occ) = "StgVarArg " ++ show occ ++ " :: " ++ show (stgArgType a)
  show (StgLitArg l)   = "StgLitArg " ++ show l
--  show (StgTypeArg t)  = "StgTypeArg " ++ showPpr t


