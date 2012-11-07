{-
   some instances for printing the StgSyn AST in Haskell syntax.
-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gen2.StgAst where

import           BasicTypes
import           CoreSyn
import           CostCentre
import           DataCon
import           DynFlags
import           ForeignCall
import           Literal
import           Module
import           Outputable
import           PrimOp
import           StgSyn
import           Type
import           UniqFM
import           UniqSet

#if __GLASGOW_HASKELL__ >= 706
showPpr' a = showPpr (defaultDynFlags undefined) a
showSDoc' a = showSDoc (defaultDynFlags undefined) a
#else
showPpr' a = showPpr a
showSDoc' a = showSDoc a
#endif

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
#if __GLASGOW_HASKELL__ >= 706
instance Show DataCon where show = showPpr'
instance Show Var where show = showPpr'
#endif

deriving instance Show UpdateFlag

#if __GLASGOW_HASKELL__ >= 706
deriving instance Show Literal
deriving instance Show PrimOp
deriving instance Show AltCon
#endif
deriving instance Show FunctionOrData
deriving instance Show StgExpr
deriving instance Show StgBinding
deriving instance Show StgRhs
deriving instance Show StgOp

instance Show (GenStgArg Var) where
  show a@(StgVarArg occ) = "StgVarArg " ++ show occ ++ " :: " ++ show (stgArgType a)
  show (StgLitArg l)   = "StgLitArg " ++ show l
--  show (StgTypeArg t)  = "StgTypeArg " ++ showPpr t


