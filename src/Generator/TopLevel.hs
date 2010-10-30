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
import Generator.Core (withBindings, definition, creation)
import qualified RTS.Objects as RTS

type BindDependInfo = [(Stg.Id, [Stg.Id])]

generate :: Module -> [(StgBinding, BindDependInfo)] -> (forall js. Javascript js => js)
generate thisMod binds =
  mconcat
    [ Js.assign modRef $ Js.new (Js.property RTS.root "Module") []
    , Js.assign (Js.property modRef "dependencies") $
        Js.list . map (Js.string . moduleName) $ dependencies thisMod allBindings
    , Js.assign (Js.property modRef "initBeforeDependencies") $
        Js.function [] $
          mconcat
            [ withBindings topDeclaration exportedBindings
            , withBindings (stubDefinition modRef) exportedBindings
            ]
    , Js.assign (Js.property modRef "initAfterDependencies") $
        Js.function [] $
          mconcat
            [ withBindings stubDefinitionFix exportedBindings
            , withBindings topDeclaration notExportedBindings
            , withBindings topDefinition allBindings
            ]
    ]
  where modRef = stgModuleToJs thisMod
        allBindings = joinBindings binds
        (exportedBindings, notExportedBindings) = partition (isExternalId . fst) allBindings

joinBindings :: [(StgBinding, BindDependInfo)] -> [(Stg.Id, StgRhs)]
joinBindings = concat . map (stgBindingToList . fst)

topDeclaration :: Javascript js => Stg.Id -> StgRhs -> js
topDeclaration id
  | isExternalId id = Js.assignProperty Js.this (stgIdToJsProperyName id) . creation
  | otherwise = Js.declare (stgIdToJsId id) . creation

topDefinition :: Javascript js => Stg.Id -> StgRhs -> js
topDefinition id
  | isExternalId id = definition $ Js.property Js.this (stgIdToJsProperyName id)
  | otherwise = definition $ Js.var . stgIdToJsId $ id

stubDefinition :: Javascript js => Expression js -> Stg.Id -> StgRhs -> js
stubDefinition mod = def . Js.property Js.this . stgIdToJsProperyName
  where def object (StgRhsCon _cc _con _stgargs) =
          mconcat
            [ RTS.markAsNotEvaluated object
            , Js.assignProperty object RTS.evalFunctionName $
                Js.function [] $
                  mconcat
                    [ Js.expression $ Js.nativeMethodCall mod "loadDependencies" []
                    , Js.return Js.this
                    ]
            ]
        def object (StgRhsClosure _cc _bi _fvs upd_flag _srt stgargs _body) =
          mconcat
            [ changeEvaluated
            , Js.assignProperty object method $
                Js.function argNames $
                  mconcat
                    [ Js.expression $ Js.nativeMethodCall mod "loadDependencies" []
                    , Js.jumpToMethod Js.this method args
                    ]
            ]
          where method
                  | isUpd = RTS.thunkEvalOnceFunctionName
                  | otherwise = RTS.evalFunctionName
                changeEvaluated
                  | isUpd = mempty
                  | otherwise = RTS.markAsNotEvaluated object
                isUpd = isUpdatable upd_flag
                args = map stgIdToJs stgargs
                argNames = map stgIdToJsId stgargs

stubDefinitionFix :: Javascript js => Stg.Id -> StgRhs -> js
stubDefinitionFix = def . Js.property Js.this . stgIdToJsProperyName
  where def object (StgRhsCon _cc _con _stgargs) =
          mconcat
            [ RTS.markAsEvaluated object
            , Js.assignProperty object RTS.evalFunctionName $
                Js.function [] (Js.return Js.this)
            ]
        def object (StgRhsClosure _cc _bi _fvs upd_flag _srt _stgargs _body)
              | isUpdatable upd_flag = mempty
              | otherwise = RTS.markAsEvaluated object

