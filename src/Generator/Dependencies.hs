{-# LANGUAGE CPP #-}
module Generator.Dependencies where

import Id as Stg (Id)
import Module (Module)
import Name (NamedThing (getName), nameModule)
import Panic (panic)
import StgSyn as Stg
import CoreSyn as Stg (AltCon)
import Data.Set as Set hiding (map, filter)
import qualified Data.Set as Set (map, filter)
import Generator.Helpers (isExternalId)

dependencies :: Module -> [(Stg.Id, StgRhs)] -> [Module]
dependencies mod = toList . delete mod . Set.map (nameModule . getName) . Set.filter isExternalId . ids

ids :: [(Stg.Id, StgRhs)] -> Set Id
ids = unions . map (uncurry go)
  where go id rhs = insert id $ idsOfRhs rhs

idsOfRhs :: StgRhs -> Set Id
idsOfRhs (StgRhsClosure _ _ _ _ _ args body) = fromList args `union` idsOfExpr body
idsOfRhs (StgRhsCon _ _ args) = idsOfArgs args

idsOfExpr :: StgExpr -> Set Id
idsOfExpr (StgApp f args) = insert f $ idsOfArgs args
idsOfExpr (StgConApp _ args) = idsOfArgs args
idsOfExpr (StgOpApp _ args _) = idsOfArgs args
idsOfExpr (StgLit _) = empty
idsOfExpr (StgLam _ _ _) = panic "unexpected StgLam"
idsOfExpr (StgCase expr _ _ bndr _ _ alts) =
  insert bndr $ idsOfExpr expr `union` idsOfAlts alts
idsOfExpr (StgLet bndn body) = idsOfBinding bndn `union` idsOfExpr body
idsOfExpr (StgLetNoEscape _ _ bndn body) = idsOfBinding bndn `union` idsOfExpr body
#if __GLASGOW_HASKELL__ >= 703
idsOfExpr (StgSCC _ _ _ expr) = idsOfExpr expr
#else
idsOfExpr (StgSCC _ expr) = idsOfExpr expr
#endif
idsOfExpr (StgTick _ _ expr) = idsOfExpr expr

idsOfArgs :: [StgArg] -> Set Id
idsOfArgs = unions . map idsOfArg

idsOfArg :: StgArg -> Set Id
idsOfArg (StgVarArg id) = singleton id
idsOfArg _ = empty

idsOfBinding :: StgBinding -> Set Id
idsOfBinding (StgNonRec id rhs) = ids [(id, rhs)]
idsOfBinding (StgRec bs) = ids bs

idsOfAlts :: [(AltCon, [Id], [Bool], StgExpr)] -> Set Id
idsOfAlts = unions . map idsOfAlt

idsOfAlt :: (AltCon, [Id], [Bool], StgExpr) -> Set Id
idsOfAlt (_, ids, _, expr) = fromList ids `union` idsOfExpr expr

