{-
   some instances for printing the StgSyn AST in Haskell syntax.
-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gen2.StgAst where

import           BasicTypes
import           CoreSyn
import           CostCentre
import           DataCon
import           DynFlags
import           ForeignCall
import           Id
import           Literal
import           Module
import           Outputable    hiding ((<>))
import           PrimOp
import           StgSyn
import           Type
import           UniqFM
import           UniqSet

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Set      (Set)
import qualified Data.Set      as S

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

s = S.singleton
l = F.foldMap

-- | collect Ids that this binding refers to
--   (does not include the bindees themselves)
bindingRefs :: StgBinding -> Set Id
bindingRefs (StgNonRec _ rhs) = rhsRefs rhs
bindingRefs (StgRec bs)       = l (rhsRefs . snd) bs

rhsRefs :: StgRhs -> Set Id
rhsRefs (StgRhsClosure _ _ _ _ _ _ body) = exprRefs body
rhsRefs (StgRhsCon _ d args) = l s (dataConImplicitIds d) <> l argRefs args

exprRefs :: StgExpr -> Set Id
exprRefs (StgApp f args) = s f <> l argRefs args
exprRefs (StgConApp d args) = l s (dataConImplicitIds d) <> l argRefs args
exprRefs (StgOpApp _ args _) = l argRefs args
exprRefs (StgLit {}) = mempty
exprRefs (StgLam {}) = mempty
exprRefs (StgCase expr _ _ _ _ _ alts) = exprRefs expr <> alts^.folded._4.to exprRefs
exprRefs (StgLet _ expr) = exprRefs expr
exprRefs (StgLetNoEscape _ _ _ expr) = exprRefs expr
exprRefs (StgSCC _ _ _ expr) = exprRefs expr
exprRefs (StgTick _ _ expr) = exprRefs expr

argRefs :: StgArg -> Set Id
argRefs (StgVarArg id) = s id
argRefs _ = mempty

