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
import           Name
import           Outputable    hiding ((<>))
import           PrimOp
import           StgSyn
import           TyCon
import           Type
import           UniqFM
import           UniqSet
import qualified Var

import           Control.Lens
import qualified Data.Foldable as F
import           Data.Monoid
import           Data.Set      (Set)
import qualified Data.Set      as S
import qualified Data.List      as L

-- fixme make this more informative
instance Show Type where
  show ty = "_TYPE_"
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
instance Show Var where show v = show (Var.varName v)
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


{-
   Remove all LetNoEscape because we cannot (yet?) generate code for that.
   This means updating the live variables lists everywhere
 -}
removeNoEscape :: StgBinding -> StgBinding
removeNoEscape (StgNonRec b rhs) =
  StgNonRec b $ fst (removeNoEscapeR mempty rhs)
removeNoEscape (StgRec bs) =
  StgRec $ map (\(b,r) -> (b, fst (removeNoEscapeR mempty r))) bs

removeNoEscapeR :: Set Id -> StgRhs -> (StgRhs, Set Id)
removeNoEscapeR i (StgRhsClosure ccs bi live upd srt bs body) =
  let (body', bodyrefs) = removeNoEscapeE i body
      live' = L.nub $ live ++ (S.toList $ S.intersection i bodyrefs)
  in (StgRhsClosure ccs bi live' upd srt bs body', bodyrefs)
removeNoEscapeR i con@(StgRhsCon _ _ args)  =
  (con, S.unions $ map argRefs args)

removeNoEscapeE :: Set Id -> StgExpr -> (StgExpr, Set Id)
removeNoEscapeE i (StgLetNoEscape l1 l2 (StgNonRec b rhs) expr) =
  let i' = S.union i (S.fromList $ uniqSetToList l1 ++ uniqSetToList l2) -- S.insert b i
      (expr', exprrefs) = removeNoEscapeE i' expr
      (rhs', rhsrefs)   = removeNoEscapeR i' rhs
  in  (StgLet (StgNonRec b rhs') expr', rhsrefs `S.union` exprrefs)
removeNoEscapeE i (StgLetNoEscape l1 l2 (StgRec bs) expr) =
  let i' = S.union i (S.fromList $ uniqSetToList l1 ++ uniqSetToList l2) -- map fst bs)
      (expr', exprrefs) = removeNoEscapeE i' expr
      (bs', bsrefs) = unzip $ map (\(b,r) -> let (r', refs) = removeNoEscapeR i' r
                                             in ((b,r'), refs)
                                  ) bs
  in (StgLet (StgRec bs') expr', S.unions (exprrefs:bsrefs))
removeNoEscapeE i (StgLet (StgNonRec b rhs) expr) =
  let (rhs', rhsrefs)   = removeNoEscapeR i rhs
      (expr', exprrefs) = removeNoEscapeE i expr
  in (StgLet (StgNonRec b rhs') expr', rhsrefs `S.union` exprrefs)
removeNoEscapeE i (StgLet (StgRec bs) expr) =
  let (expr', exprrefs) = removeNoEscapeE i expr
      (bs', bsrefs)     = unzip $ map (\(b,r) ->
                                        let (r', refs) = removeNoEscapeR i r
                                        in  ((b,r'),refs)
                                      ) bs
  in (StgLet (StgRec bs') expr', S.unions (exprrefs:bsrefs))
removeNoEscapeE i (StgSCC cc b c expr) =
  let (expr', refs) = removeNoEscapeE i expr 
  in  (StgSCC cc b c expr', refs)
removeNoEscapeE i (StgTick m n expr) = 
  let (expr', refs) = removeNoEscapeE i expr
  in  (StgTick m n expr', refs)
removeNoEscapeE i (StgCase e live1 live2 b srt at alts) = 
  let (alts', altrefs) = unzip (map f alts)
      (e', exprrefs)   = removeNoEscapeE i e
--      newLiveAlts      = S.toList $ S.intersection i (S.unions altrefs)
--      newLiveExpr      = S.toList $ S.intersection i exprrefs
      unused1          = i S.\\ S.unions (exprrefs:altrefs)
      unused2          = i S.\\ S.unions altrefs
      live1' = delListFromUniqSet live1 (S.toList unused1)
      live2' = delListFromUniqSet live2 (S.toList unused2)    
--      live1' = addListToUniqSet live1 (newLiveAlts ++ newLiveExpr)
--      live2' = addListToUniqSet live2 newLiveAlts
      f :: StgAlt -> (StgAlt, Set Id)
      f (ac, live, liveactive, expr) =
        let  (expr', exprrefs) = removeNoEscapeE i expr
--             newlive = S.toList (S.intersection i exprrefs)
--             active' = replicate (length newlive) True ++ liveactive
--        in ((ac, newlive++live, active', expr'), exprrefs)
        in ((ac, live, liveactive, expr'), exprrefs)
  in (StgCase e' live1' live2' b srt at alts', S.unions (exprrefs:altrefs))
removeNoEscapeE i x = (x, exprRefs x)


