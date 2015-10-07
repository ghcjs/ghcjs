{-
   some instances for printing the StgSyn AST in Haskell syntax.
-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gen2.StgAst where

import           Data.Char     (isSpace)
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Set      (Set)
import qualified Data.Set      as S
import           DataCon
import           DynFlags

import           BasicTypes
import           Control.Lens
import           CoreSyn
import           CostCentre
import           ForeignCall
import           Id
import           Literal
import           Module
import           Name
import           Outputable    hiding ((<>))
import           PrimOp
import           StgSyn
import           TyCon
import           Type
import           Unique
import           UniqFM
import           UniqSet
import           IdInfo
import qualified Var

import           Coercion
import           CoAxiom
import           Gen2.Utils
import qualified CoreSyn as Core
import Coercion

-- this is a hack to be able to use pprShow in a Show instance, should be removed
{-# NOINLINE hackPprDflags #-}
hackPprDflags :: DynFlags
hackPprDflags = unsafeGlobalDynFlags

-- | replace all whitespace with space
fixSpace :: String -> String
fixSpace xs = map f xs
  where
    f c | isSpace c = ' '
        | otherwise = c


-- fixme make this more informative
instance Show Type where
  show ty = fixSpace (showPpr hackPprDflags ty)
instance Show CostCentre where show _ = "CostCentre"
instance Show CostCentreStack where show _ = "CostCentreStack"
instance Show StgBinderInfo where show _ = "StgBinderInfo"
#if __GLASGOW_HASKELL__ >= 709
instance Show Module where show m = packageKeyString (modulePackageKey m) ++ ":" ++ moduleNameString (moduleName m)
#else
instance Show Module where show m = packageIdString (modulePackageId m) ++ ":" ++ moduleNameString (moduleName m)
#endif
instance Show (UniqFM Id) where show u = "[" ++ show (uniqSetToList u) ++ "]"
instance Show TyCon where show = show . tyConName
instance Show SRT where
  show NoSRT = "SRT:NO"
  show (SRTEntries e) = "SRT:" ++ show e
  show (SRT i j _b) = "SRT:BMP" ++ show [i,j]
#if __GLASGOW_HASKELL__ >= 709
instance Show PackageKey where show = packageKeyString
#else
instance Show PackageId where show = packageIdString
#endif
instance Show Name where
  show n = case nameModule_maybe n of
                  Nothing -> show (nameOccName n)
                  Just m  -> show m ++ "." ++ show (nameOccName n)
instance Show OccName where show = occNameString
instance Show DataCon where show d = show (dataConName d)
instance Show Var where show v = "(" ++ show (Var.varName v) ++ "[" ++
                                 encodeUnique (getKey (getUnique v))
                                 ++ "] <" {- ++ show (idDetails v) -} ++ "> :: " ++ show (Var.varType v) ++ ")"
instance Show IdDetails where
  show VanillaId          = "VanillaId"
  show (RecSelId {})      = "RecSelId"
  show (DataConWorkId dc) = "DataConWorkId " ++ show dc
  show (DataConWrapId dc) = "DataConWrapId " ++ show dc
  show (ClassOpId {})     = "ClassOpId"
  show (PrimOpId {})      = "PrimOpId"
  show (FCallId {})       = "FCallId"
  show (TickBoxOpId {})   = "VanillaId"
  show (DFunId {})        = "DFunId"


deriving instance Show UpdateFlag
deriving instance Show PrimOpVecCat
deriving instance Show Literal
deriving instance Show PrimOp
deriving instance Show AltCon
deriving instance Show AltType
deriving instance Show PrimCall
deriving instance Show ForeignCall
deriving instance Show CCallTarget
deriving instance Show CCallSpec
deriving instance Show CCallConv
deriving instance Show FunctionOrData
deriving instance Show StgExpr
deriving instance Show StgBinding
deriving instance Show StgRhs
deriving instance Show StgOp
#if __GLASGOW_HASKELL__ >= 709
deriving instance Show a => Show (Tickish a)
#endif
-- 
deriving instance Show Coercion
deriving instance Show a => Show (Expr a)
deriving instance Show a => Show (Bind a)
instance Show CoAxiomRule where show _ = "CoAxiomRule"
instance Show (CoAxiom a) where show _ = "CoAxiom"
deriving instance Show LeftOrRight
deriving instance Show Role
instance Show (GenStgArg Var) where
  show a@(StgVarArg occ) = "StgVarArg " ++ show occ ++ " :: " ++ show (stgArgType a)
  show (StgLitArg l)   = "StgLitArg " ++ show l

s :: a -> Set a
s = S.singleton

l :: (a -> Set Id) -> [a] -> Set Id
l = F.foldMap

-- | collect Ids that this binding refers to
--   (does not include the bindees themselves)
-- first argument is Id -> StgExpr map for unfloated arguments
bindingRefs :: UniqFM StgExpr -> StgBinding -> Set Id
bindingRefs u (StgNonRec _ rhs) = rhsRefs u rhs
bindingRefs u (StgRec bs)       = l (rhsRefs u . snd) bs

rhsRefs :: UniqFM StgExpr -> StgRhs -> Set Id
rhsRefs u (StgRhsClosure _ _ _ _ _ _ body) = exprRefs u body
rhsRefs u (StgRhsCon _ d args) = l s (dataConImplicitIds d) <> l (argRefs u) args

exprRefs :: UniqFM StgExpr -> StgExpr -> Set Id
exprRefs u (StgApp f args) = s f <> l (argRefs u) args
exprRefs u (StgConApp d args) = l s (dataConImplicitIds d) <> l (argRefs u) args
exprRefs u (StgOpApp _ args _) = l (argRefs u) args
exprRefs _ (StgLit {}) = mempty
exprRefs _ (StgLam {}) = mempty
exprRefs u (StgCase expr _ _ _ _ _ alts) = exprRefs u expr <> alts^.folded._4.to (exprRefs u)
exprRefs u (StgLet bnd expr) = bindingRefs u bnd <> exprRefs u expr
exprRefs u (StgLetNoEscape _ _ bnd expr) = bindingRefs u bnd <> exprRefs u expr
#if __GLASGOW_HASKELL__ < 709
exprRefs u (StgTick _ _ expr) = exprRefs u expr
exprRefs u (StgSCC _ _ _ expr) = exprRefs u expr
#else
exprRefs u (StgTick _ expr) = exprRefs u expr
#endif

argRefs :: UniqFM StgExpr -> StgArg -> Set Id
argRefs u (StgVarArg id)
  | Just e <- lookupUFM u id = exprRefs u e
  | otherwise                = s id
argRefs _ _ = mempty

