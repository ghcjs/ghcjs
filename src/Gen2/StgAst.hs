{-
   some instances for printing the StgSyn AST in Haskell syntax.
-}

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Gen2.StgAst where

import           BasicTypes
import           Control.Lens
import           CoreSyn
import           CostCentre
import           Data.Char (isSpace)
import qualified Data.Foldable as F
import qualified Data.List      as L
import           Data.Monoid
import           Data.Set      (Set)
import qualified Data.Set      as S
import           DataCon
import           DynFlags
import           ForeignCall
import           Id
import           Literal
import           Module
import           Name
import           Outputable    hiding ((<>))
import           PrimOp
import           StgSyn
import           SysTools (initSysTools)
import           TyCon
import           Type
import           UniqFM
import           UniqSet
import qualified Var

import           Compiler.Info

import           Control.Monad
import           System.Environment (getArgs)
import           System.IO.Unsafe

-- this is a hack to be able to use pprShow in a Show instance, should be removed
{-# NOINLINE hackPprDflags #-}
hackPprDflags :: DynFlags
hackPprDflags = unsafePerformIO $ do
  args <- getArgs
  let (minusB_args, args1) = L.partition ("-B" `L.isPrefixOf`) args
      mbMinusB | null minusB_args = Nothing
               | otherwise = Just . drop 2 . last $ minusB_args
  libDir <- getGlobalPackageBase
  mySettings <- initSysTools (mbMinusB `mplus` Just libDir)
  initDynFlags (defaultDynFlags mySettings)

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
instance Show Module where show m = packageIdString (modulePackageId m) ++ ":" ++ moduleNameString (moduleName m)
instance Show (UniqFM Id) where show u = "[" ++ show (uniqSetToList u) ++ "]"
instance Show TyCon where show = show . tyConName
instance Show SRT where
  show NoSRT = "SRT:NO"
  show (SRTEntries e) = "SRT:" ++ show e
  show (SRT i j b) = "SRT:BMP" ++ show [i,j]
instance Show PackageId where show = packageIdString
instance Show Name where
  show n = case nameModule_maybe n of
                  Nothing -> show (nameOccName n)
                  Just m  -> show m ++ "." ++ show (nameOccName n)
instance Show OccName where show = occNameString
#if __GLASGOW_HASKELL__ >= 706
instance Show DataCon where show d = show (dataConName d)
instance Show Var where show v = "(" ++ show (Var.varName v) ++ " :: " ++ show (Var.varType v) ++ ")"
#endif

deriving instance Show UpdateFlag

#if __GLASGOW_HASKELL__ >= 706
deriving instance Show Literal
deriving instance Show PrimOp
deriving instance Show AltCon
#endif
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
exprRefs (StgLet bnd expr) = bindingRefs bnd <> exprRefs expr
exprRefs (StgLetNoEscape _ _ bnd expr) = bindingRefs bnd <> exprRefs expr
exprRefs (StgSCC _ _ _ expr) = exprRefs expr
exprRefs (StgTick _ _ expr) = exprRefs expr

argRefs :: StgArg -> Set Id
argRefs (StgVarArg id) = s id
argRefs _ = mempty

