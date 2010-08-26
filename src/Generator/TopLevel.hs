{-# LANGUAGE Rank2Types #-}
module Generator.TopLevel (generate) where

import Data.Monoid (mconcat)
import Data.List (partition)

import Module (Module)
import Id as Stg (Id)

import StgSyn
import Javascript.Language as Js

import Generator.Helpers
import Generator.Dependencies (dependencies)
import Generator.Core (declarations, withBindings, definitions)

type BindDependInfo = [(Stg.Id, [Stg.Id])]

generate :: Module -> [(StgBinding, BindDependInfo)] -> (forall js. Javascript js => js)
generate thisMod binds =
  mconcat
    [ Js.assign modRef $ Js.new (Js.property haskellRoot "Module") []
    , Js.assign (Js.property modRef "dependencies") $
        Js.list . map (Js.string . moduleName) $ dependencies thisMod allBindings
    , Js.assign (Js.property modRef "initBeforeDependencies") $
        Js.function [] $
          mconcat
            [ declarations exportedBindings
            , withBindings (stubDefinition modRef) exportedBindings
            ]
    , Js.assign (Js.property modRef "initAfterDependencies") $
        Js.function [] $
          mconcat
            [ declarations notExportedBindings
            , definitions allBindings
            ]
    ]
  where modRef = stgModuleToJs thisMod
        allBindings = joinBindings binds
        (exportedBindings, notExportedBindings) = partition (isExternalId . fst) allBindings

joinBindings :: [(StgBinding, BindDependInfo)] -> [(Stg.Id, StgRhs)]
joinBindings = concat . map (stgBindingToList . fst)

stubDefinition :: Javascript js => Expression js -> Stg.Id -> StgRhs -> js
stubDefinition mod id (StgRhsCon _cc _con _stgargs) =
  mconcat
    [ Js.assignProperty object "evaluated" Js.false
    , Js.assignProperty object "evaluate" $
        Js.function [] (dataStubExpression mod)
    ]
  where object = stgIdToJs id
stubDefinition mod id (StgRhsClosure _cc _bi _fvs upd_flag _srt stgargs _body) =
  mconcat
    [ Js.assignProperty object "evaluated" Js.false
    , Js.assignProperty object method $
         Js.function argNames (stubExpression mod object method args)
    ]
  where object = stgIdToJs id
        method
          | isUpdatable upd_flag = "evaluateOnce"
          | otherwise = "evaluate"
        args = map stgIdToJs stgargs
        argNames = map stgIdToJsId stgargs

stubExpression :: Javascript js => Expression js -> Expression js -> String -> [Expression js] -> js
stubExpression mod object method args =
  mconcat
    [ Js.callMethod mod "loadDependencies" []
    , Js.jumpToMethod object method args
    ]

dataStubExpression :: Javascript js => Expression js -> js
dataStubExpression mod =
  mconcat
    [ Js.callMethod mod "loadDependencies" []
    , Js.return (Js.var "this")
    ]

