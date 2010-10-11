{-# LANGUAGE Rank2Types #-}
module Generator.TopLevel (generate) where

import Data.Monoid (mconcat, mempty)
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
            [ withBindings stubDefinitionFix exportedBindings
            , declarations notExportedBindings
            , definitions allBindings
            ]
    ]
  where modRef = stgModuleToJs thisMod
        allBindings = joinBindings binds
        (exportedBindings, notExportedBindings) = partition (isExternalId . fst) allBindings

joinBindings :: [(StgBinding, BindDependInfo)] -> [(Stg.Id, StgRhs)]
joinBindings = concat . map (stgBindingToList . fst)

stubDefinition :: Javascript js => Expression js -> Stg.Id -> StgRhs -> js
stubDefinition mod id = def (stgIdToJs id)
  where def object (StgRhsCon _cc _con _stgargs) =
          mconcat
            [ Js.assignProperty object "evaluated" Js.false
            , Js.assignProperty object "evaluate" $
                Js.function [] $
                  mconcat
                    [ Js.expression $ Js.nativeMethodCall mod "loadDependencies" []
                    , Js.return object
                    ]
            ]
        def object (StgRhsClosure _cc _bi _fvs upd_flag _srt stgargs _body) =
          mconcat
            [ changeEvaluated
            , Js.assignProperty object method $
                Js.function argNames $
                  mconcat
                    [ Js.expression $ Js.nativeMethodCall mod "loadDependencies" []
                    , Js.jumpToMethod object method args
                    ]
            ]
          where method
                  | isUpd = "evaluateOnce"
                  | otherwise = "evaluate"
                changeEvaluated
                  | isUpd = mempty
                  | otherwise = Js.assignProperty object "evaluated" Js.false
                isUpd = isUpdatable upd_flag
                args = map stgIdToJs stgargs
                argNames = map stgIdToJsId stgargs

stubDefinitionFix :: Javascript js => Stg.Id -> StgRhs -> js
stubDefinitionFix id = def (stgIdToJs id)
  where def object (StgRhsCon _cc _con _stgargs) =
          mconcat
            [ Js.assignProperty object "evaluated" Js.true
            , Js.assignProperty object "evaluate" $
                Js.function [] (Js.return Js.this)
            ]
        def object (StgRhsClosure _cc _bi _fvs upd_flag _srt _stgargs _body)
              | isUpdatable upd_flag = mempty
              | otherwise = Js.assignProperty object "evaluated" Js.true

