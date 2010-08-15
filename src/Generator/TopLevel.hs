module Generator.TopLevel (generate) where

import Module (Module)
import Id as Stg (Id, isExportedId)

import StgSyn
import qualified Javascript.Language as Js

import Generator.Helpers (stgIdToJs, stgIdToJsId, stgModuleToJs, modulePath, stgBindingToList, haskellRoot)
import Generator.Core (declarations, withBindings, definitions)

generate :: Module -> [Module] -> [StgBinding] -> IO Js.Program
generate thisMod importedMods binds =
  return $ Js.sequence
    [ Js.assign modRef $ Js.new (Js.property haskellRoot "Module") []
    , Js.assign (Js.property modRef "dependencies") $
        Js.list . map (Js.string . modulePath) $ importedMods
    , Js.assign (Js.property modRef "initBeforeDependecies") $
        Js.function [] $
          Js.sequence
            [ declarations exportedBindings
            , withBindings (stubDefinition modRef) exportedBindings
            ]
    , Js.assign (Js.property modRef "initAfterDependecies") $
        Js.function [] $
          Js.sequence
            [ declarations allBindings
            , definitions allBindings
            ]
    ]
  where modRef = stgModuleToJs thisMod
        allBindings = joinBindings binds
        exportedBindings = filter (isExportedId . fst) allBindings

joinBindings :: [StgBinding] -> [(Id, StgRhs)]
joinBindings = concat . map stgBindingToList

stubDefinition :: Js.Expression -> Stg.Id -> StgRhs -> Js.Program
stubDefinition mod id (StgRhsCon _cc _con _stgargs) =
  Js.sequence
    [ Js.assignProperty object "evaluated" Js.false
    , Js.assignProperty object "evaluate" $
        Js.function [] (dataStubExpression mod object)
    ]
  where object = stgIdToJs id
stubDefinition mod id (StgRhsClosure _cc _bi _fvs upd_flag _srt stgargs _body) =
  Js.sequence
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

stubExpression :: Js.Expression -> Js.Expression -> String -> [Js.Expression] -> Js.Program
stubExpression mod object method args =
  Js.sequence
    [ Js.declareMethodCallResult "$res" mod "loadDependencies" []
    , Js.jumpToMethod object method args
    ]

dataStubExpression :: Js.Expression -> Js.Expression -> Js.Program
dataStubExpression mod object =
  Js.sequence
    [ Js.if_ (Js.not $ Js.property object "evaluated") $
        Js.sequence
          [ Js.declareMethodCallResult "$res" mod "loadDependencies" []
          , Js.jumpToMethod object "evaluate" []
          ]
    , Js.return $ object
    ]

