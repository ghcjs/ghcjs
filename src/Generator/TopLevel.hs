module Generator.TopLevel (generate) where

import Data.Monoid (mconcat)

import Module (Module)
import Id as Stg (Id)
import Name (isExternalName, NamedThing (getName))

import StgSyn
import Javascript.Language as Js

import Generator.Helpers (stgIdToJs, stgIdToJsId, stgModuleToJs, moduleName, stgBindingToList, haskellRoot)
import Generator.Core (declarations, withBindings, definitions)

generate :: Javascript js => Module -> [Module] -> [StgBinding] -> IO js
generate thisMod importedMods binds =
  Prelude.return $ mconcat
    [ Js.assign modRef $ Js.new (Js.property haskellRoot "Module") []
    , Js.assign (Js.property modRef "dependencies") $
        Js.list . map (Js.string . moduleName) $ importedMods
    , Js.assign (Js.property modRef "initBeforeDependencies") $
        Js.function [] $
          mconcat
            [ declarations exportedBindings
            , withBindings (stubDefinition modRef) exportedBindings
            ]
    , Js.assign (Js.property modRef "initAfterDependencies") $
        Js.function [] $
          mconcat
            [ declarations allBindings
            , definitions allBindings
            ]
    ]
  where modRef = stgModuleToJs thisMod
        allBindings = joinBindings binds
        exportedBindings = filter (isExternalName . getName . fst) allBindings

joinBindings :: [StgBinding] -> [(Stg.Id, StgRhs)]
joinBindings = concat . map stgBindingToList

stubDefinition :: Javascript js => Expression js -> Stg.Id -> StgRhs -> js
stubDefinition mod id (StgRhsCon _cc _con _stgargs) =
  mconcat
    [ Js.assignProperty object "evaluated" Js.false
    , Js.assignProperty object "evaluate" $
        Js.function [] (dataStubExpression mod object)
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

dataStubExpression :: Javascript js => Expression js -> Expression js -> js
dataStubExpression mod object =
  mconcat
    [ Js.if_ (Js.not $ Js.property object "evaluated") $
        mconcat
          [ Js.callMethod mod "loadDependencies" []
          , Js.return $ object
          ]
    , Js.return $ object
    ]

