{-# LANGUAGE Rank2Types #-}
module Generator.TopLevel (generate) where

import Data.Monoid (mconcat)
import Data.List (partition)

import Module as Stg (Module, moduleName, moduleNameString)
import Id as Stg (Id)

import StgSyn
import qualified Javascript.Language as Js
import Javascript.Language (Javascript)

import Generator.Helpers
import Generator.Core (withBindings, notExportedDecl, definition)
import Generator.Dependencies
import Control.Applicative ((<$>))
import qualified Data.Set as S

type BindDependInfo = [(Stg.Id, [Stg.Id])]

generate :: Module -> [(StgBinding, BindDependInfo)] -> (forall js. Javascript js => Gen js)
generate thisMod binds = do
  addTopLevelIds thisMod $ map fst notExportedBindings
  exports <- withBindings definition exportedBindings
  decls   <- map (\(i, r) -> Js.declare [(i, r)]) <$> mapM (notExportedDecl False) notExportedBindings
  allDeps <- mapM deps allBindings
  return . mconcat $
    [Js.comment $ "GHCJS Haskell Module " ++ show (moduleNameString $ moduleName thisMod, modDeps)]
    ++ allDeps ++ [exports] ++ decls
  where allBindings = joinBindings binds
        (exportedBindings, notExportedBindings) = partition (isExternalId . fst) allBindings
        deps :: Javascript js => (Stg.Id, StgRhs) -> Gen js
        deps (id, rhs) = do
            thisId <- stgIdToJs id
            depIds <- mapM stgIdToJs . S.toList $ idsOfRhs rhs
            return . Js.comment $ show (thisId, filter isTopLevel depIds)
        isTopLevel ('$':'$':_) = True
        isTopLevel _ = False
        modDeps = map (moduleNameString . moduleName) $ dependencies thisMod allBindings

joinBindings :: [(StgBinding, BindDependInfo)] -> [(Stg.Id, StgRhs)]
joinBindings = concat . map (stgBindingToList . fst)




















