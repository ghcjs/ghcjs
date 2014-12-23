{-# LANGUAGE TupleSections, CPP #-}

module Gen2.Sinker (sinkPgm) where

import UniqSet
import VarSet
import UniqFM
import StgSyn
import Id
import Name
import Module
import Literal
import Digraph

import Control.Applicative
import Control.Lens

import Data.Char
import Data.List (partition)
import Data.Maybe
import Data.Traversable

import Gen2.ClosureInfo

{- |
  GHC floats constants to the top level. This is fine in native code, but with JS
  they occupy some global variable name. We can unfloat some unexported things:

  - global constructors, as long as they're referenced only once by another global
       constructor and are not in a recursive binding group
  - literals (small literals may also be sunk if they are used more than once)
 -}

sinkPgm :: Module                         -- ^ the module, since we treat definitions from the
                                          --   current module differently
        -> [StgBinding]                   -- ^ the bindings
        -> (UniqFM StgExpr, [StgBinding]) -- ^ a map with sunken replacements for nodes, for where
                                          --   the replacement does not fit in the 'StgBinding' AST
                                          --   and the new bindings
sinkPgm m pgm =
  let usedOnce = collectUsedOnce pgm
      sinkables = listToUFM $
          concatMap alwaysSinkable pgm ++
          filter ((`elementOfUniqSet` usedOnce) . fst) (concatMap (onceSinkable m) pgm)
      isSunkBind (StgNonRec b _e) | elemUFM b sinkables = True
      isSunkBind _                                      = False
  in (sinkables, filter (not . isSunkBind) $ topSortDecls m pgm)

{- |
  always sinkable, values that may be duplicated in the generated code:

   *  small literals
-}
alwaysSinkable :: StgBinding -> [(Id, StgExpr)]
alwaysSinkable (StgNonRec b rhs)
  | (StgRhsClosure _ccs _bi _ _upd _srt _ e@(StgLit l)) <- rhs,
     isSmallSinkableLit l && isLocal b = [(b,e)]
  | (StgRhsCon _ccs dc as@[StgLitArg l]) <- rhs,
     isSmallSinkableLit l && isLocal b && isUnboxableCon dc = [(b,StgConApp dc as)]
alwaysSinkable _ = []

isSmallSinkableLit :: Literal -> Bool
isSmallSinkableLit (MachChar c) = ord c < 100000
isSmallSinkableLit (MachInt i)  = i > -100000 && i < 100000
isSmallSinkableLit (MachWord i) = i < 100000
isSmallSinkableLit _            = False


{- |
   once sinkable: may be sunk, but duplication is not ok
-}
onceSinkable :: Module -> StgBinding -> [(Id, StgExpr)]
onceSinkable _m (StgNonRec b rhs)
  | Just e <- getSinkable rhs, isLocal b = [(b,e)]
  where
    getSinkable (StgRhsCon _ccs dc args)
      = Just (StgConApp dc args)
    getSinkable (StgRhsClosure _ccs _bi _ _upd _srt _ e@(StgLit{}))
      = Just e
    getSinkable _ = Nothing
onceSinkable _ _ = []

-- | collect all idents used only once in an argument at the top level
--   and never anywhere else
collectUsedOnce :: [StgBinding] -> IdSet
collectUsedOnce binds = intersectUniqSets (usedOnce foldArgs) (usedOnce foldArgsTop)
  where
    usedOnce f = fst . foldrOf (traverse . f) g (emptyUniqSet, emptyUniqSet) $ binds
    g i t@(once, mult)
      | i `elementOfUniqSet` mult = t
      | i `elementOfUniqSet` once
        = (delOneFromUniqSet once i, addOneToUniqSet mult i)
      | otherwise = (addOneToUniqSet once i, mult)

-- | fold over all id in StgArg used at the top level in an StgRhsCon
foldArgsTop :: Fold StgBinding Id
foldArgsTop f e@(StgNonRec b r) 
  | (StgRhsCon ccs dc args) <- r =
     StgNonRec b . StgRhsCon ccs dc <$> (traverse . foldArgsA) f args
  | otherwise                    = pure e
foldArgsTop f (StgRec bs) =
  StgRec <$> sequenceA (map (\(b,r) -> (,) b <$> g r) bs)
    where
      g (StgRhsCon ccs dc args) =
          StgRhsCon ccs dc <$> (traverse . foldArgsA) f args
      g x                       = pure x

-- | fold over all Id in StgArg in the AST
foldArgs :: Fold StgBinding Id
foldArgs f (StgNonRec b r) = StgNonRec b <$> foldArgsR f r
foldArgs f (StgRec bs)     =
  StgRec <$> sequenceA (map (\(b,r) -> (,) b <$> foldArgsR f r) bs)

foldArgsR :: Fold StgRhs Id
foldArgsR f (StgRhsClosure x0 x1 x2 x3 x4 x5 e) =
  StgRhsClosure x0 x1 x2 x3 x4 x5 <$> foldArgsE f e
foldArgsR f (StgRhsCon x y args)                =
  StgRhsCon x y <$> (traverse . foldArgsA) f args

foldArgsE :: Fold StgExpr Id
foldArgsE f (StgApp x args)            = StgApp <$> f x <*> (traverse . foldArgsA) f args
foldArgsE f (StgConApp c args)         = StgConApp c <$> (traverse . foldArgsA) f args
foldArgsE f (StgOpApp x args t)        = StgOpApp x  <$> (traverse . foldArgsA) f args <*> pure t
foldArgsE f (StgLam b e)               = StgLam b    <$> foldArgsE f e
foldArgsE f (StgCase e l1 l2 b s a alts) =
  StgCase <$> foldArgsE f e <*> pure l1 <*> pure l2
          <*> pure b <*> pure s <*> pure a
          <*> sequenceA (map (\(ac,bs,us,e) -> (,,,) ac bs us <$> foldArgsE f e) alts)
foldArgsE f (StgLet b e)               = StgLet <$> foldArgs f b <*> foldArgsE f e
foldArgsE f (StgLetNoEscape l1 l2 b e) = StgLetNoEscape l1 l2 <$> foldArgs f b <*> foldArgsE f e
#if __GLASGOW_HASKELL__ < 709
foldArgsE f (StgSCC cc b1 b2 e)        = StgSCC cc b1 b2 <$> foldArgsE f e
foldArgsE f (StgTick m i e)            = StgTick m i <$> foldArgsE f e
#else
foldArgsE f (StgTick i e)              = StgTick i <$> foldArgsE f e
#endif
foldArgsE _ e                          = pure e

foldArgsA :: Fold StgArg Id
foldArgsA f (StgVarArg i) = StgVarArg <$> f i
foldArgsA _ a             = pure a

isLocal :: Id -> Bool
isLocal i = isNothing (nameModule_maybe . idName $ i) && not (isExportedId i)

{- | since we have sequential initialization,
     topsort the non-recursive constructor bindings
 -}
topSortDecls :: Module -> [StgBinding] -> [StgBinding]
topSortDecls _m binds = rest ++ nr'
  where
    (nr, rest) = partition isNonRec binds
    isNonRec (StgNonRec {}) = True
    isNonRec _              = False
    vs   = map getV nr
    keys = mkUniqSet (map snd vs)
    getV e@(StgNonRec b _) = (e, b)
    getV _                 = error "topSortDecls: getV, unexpected binding"
    collectDeps (StgNonRec b (StgRhsCon _ _dc args)) =
      [ (i, b) | StgVarArg i <- args, i `elementOfUniqSet` keys ]
    collectDeps _ = []
    g = graphFromVerticesAndAdjacency vs (concatMap collectDeps nr)
    nr' | (not . null) [()| CyclicSCC _ <- stronglyConnCompG g]
            = error "topSortDecls: unexpected cycle"
        | otherwise = map fst (topologicalSortG g)
