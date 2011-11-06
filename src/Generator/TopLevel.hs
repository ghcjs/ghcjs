{-# LANGUAGE Rank2Types #-}
module Generator.TopLevel (generate) where

import Data.Monoid (mconcat)
import Data.List (partition)

import Module (Module)
import Id as Stg (Id)

import StgSyn
import qualified Javascript.Language as Js
import Javascript.Language (Javascript)

import Generator.Helpers
import Generator.Core (withBindings, notExportedDecl, definition)
import Control.Applicative ((<$>))

type BindDependInfo = [(Stg.Id, [Stg.Id])]

generate :: Module -> [(StgBinding, BindDependInfo)] -> (forall js. Javascript js => Gen js)
generate _thisMod binds = do
  addTopLevelIds _thisMod $ map fst notExportedBindings
  exports <- withBindings definition exportedBindings
  decls   <- Js.declare <$> mapM notExportedDecl notExportedBindings
  return $ mconcat [exports, decls]
  where allBindings = joinBindings binds
        (exportedBindings, notExportedBindings) = partition (isExternalId . fst) allBindings

joinBindings :: [(StgBinding, BindDependInfo)] -> [(Stg.Id, StgRhs)]
joinBindings = concat . map (stgBindingToList . fst)




















