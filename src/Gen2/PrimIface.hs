{-# LANGUAGE CPP #-}
{-
  override the GHC.Prim interface to make sure we have the right types
  for platform-dependent primop types
 -}

module Gen2.PrimIface (ghcjsPrimIface, mkGhcjsPrimOpId) where

import Control.Applicative
import Data.Maybe

import Avail
import BasicTypes
import Demand
import FastString
import HscTypes
import Id
import IdInfo
import MkId hiding (primOpRules) -- use our own corrected rules
import Name
import PrelNames
import PrimOp
import Rules
import TyCon
import Type
import TysPrim
import TysWiredIn
import Unique
import CmmType

import Gen2.GHC.PrelRules

ghcjsPrimIface :: ModIface
ghcjsPrimIface
  = (emptyModIface gHC_PRIM) {
        mi_exports  = ghcjsPrimExports,
        mi_decls    = [],
        mi_fixities = fixities,
        mi_fix_fn  = mkIfaceFixCache fixities
    }
  where
    fixities = (getOccName seqId, Fixity 0 InfixR)  -- seq is infixr 0
             : mapMaybe mkFixity allThePrimOps
    mkFixity op = (,) (primOpOcc op) <$> primOpFixity op

ghcjsPrimExports :: [IfaceExport]
ghcjsPrimExports
 = map (Avail . idName) ghcPrimIds ++
   map (Avail . idName . mkGhcjsPrimOpId) allThePrimOps ++
   [ AvailTC n [n]
   | tc <- funTyCon : coercibleTyCon : primTyCons, let n = tyConName tc  ]

-- include our own primop type list, this must match the host
-- compiler version and be processed with WORD_SIZE_IN_BITS=32
primOpInfo :: PrimOp -> PrimOpInfo
#if __GLASGOW_HASKELL__ >= 709
#error "unsupported GHC version"
#elif __GLASGOW_HASKELL__ >= 707
#include "prim/primop-primop-info-708.hs-incl"
#else
#error "unsupported GHC version"
#endif

primOpStrictness :: PrimOp -> Arity -> StrictSig
#if __GLASGOW_HASKELL__ >= 709
#error "unsupported GHC version"
#elif __GLASGOW_HASKELL__ >= 707
#include "prim/primop-strictness-708.hs-incl"
#else
#error "unsupported GHC version"
#endif

mkGhcjsPrimOpId :: PrimOp -> Id
mkGhcjsPrimOpId prim_op
  = id
  where
    (tyvars,arg_tys,res_ty, arity, strict_sig) = ghcjsPrimOpSig prim_op
    ty   = mkForAllTys tyvars (mkFunTys arg_tys res_ty)
    name = mkWiredInName gHC_PRIM (primOpOcc prim_op)
                         (mkPrimOpIdUnique (primOpTag prim_op + 500000)) -- do not clash, does this help?
                         (AnId id) UserSyntax
    id   = mkGlobalId (PrimOpId prim_op) name ty info

    info = noCafIdInfo
           `setSpecInfo`          mkSpecInfo (maybeToList $ primOpRules name prim_op)
           `setArityInfo`         arity
           `setStrictnessInfo`    strict_sig
           `setInlinePragInfo`    neverInlinePragma

-- get the type sig from our local primop list
ghcjsPrimOpSig :: PrimOp -> ([TyVar], [Type], Type, Arity, StrictSig)
ghcjsPrimOpSig op
  = (tyvars, arg_tys, res_ty, arity, primOpStrictness op arity)
  where
    arity = length arg_tys
    (tyvars, arg_tys, res_ty)
      = case primOpInfo op of
        Monadic   _occ ty                    -> ([],     [ty],    ty       )
        Dyadic    _occ ty                    -> ([],     [ty,ty], ty       )
        Compare   _occ ty                    -> ([],     [ty,ty], intPrimTy)
        GenPrimOp _occ tyvars arg_tys res_ty -> (tyvars, arg_tys, res_ty   )

data PrimOpInfo
  = Dyadic      OccName         -- string :: T -> T -> T
                Type
  | Monadic     OccName         -- string :: T -> T
                Type
  | Compare     OccName         -- string :: T -> T -> Bool
                Type

  | GenPrimOp   OccName         -- string :: \/a1..an . T1 -> .. -> Tk -> T
                [TyVar]
                [Type]
                Type

mkDyadic, mkMonadic, mkCompare :: FastString -> Type -> PrimOpInfo
mkDyadic str  ty = Dyadic  (mkVarOccFS str) ty
mkMonadic str ty = Monadic (mkVarOccFS str) ty
mkCompare str ty = Compare (mkVarOccFS str) ty

mkGenPrimOp :: FastString -> [TyVar] -> [Type] -> Type -> PrimOpInfo
mkGenPrimOp str tvs tys ty = GenPrimOp (mkVarOccFS str) tvs tys ty

