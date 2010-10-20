module RTS.Objects where

import qualified Javascript.Language as Js
import Javascript.Language (Javascript, Expression)

haskellRoot :: Javascript js => Expression js
haskellRoot = Js.var "$hs"

haskellModules :: Javascript js => Expression js
haskellModules = Js.property haskellRoot "modules"

haskellModuleDependencies :: Javascript js => Expression js -> Expression js
haskellModuleDependencies mod = Js.property mod "dependencies"

haskellTrue :: Javascript js => Expression js
haskellTrue = haskellModules # "GHCziBool" # "hs_True"
  where (#) = Js.property

haskellFalse :: Javascript js => Expression js
haskellFalse = haskellModules # "GHCziBool" # "hs_False"
  where (#) = Js.property

haskellThunk :: Javascript js => Expression js
haskellThunk = Js.property haskellRoot "Thunk"

haskellFunc :: Javascript js => Expression js
haskellFunc = Js.property haskellRoot "Func"

haskellConApp :: Javascript js => Expression js
haskellConApp = Js.property haskellRoot "Data"

haskellThunkEvalOnceFunctionName :: Js.Id
haskellThunkEvalOnceFunctionName = "evaluateOnce"

haskellEvalFunctionName :: Js.Id
haskellEvalFunctionName = "evaluate"

haskellConAppArgVector :: Javascript js => Expression js -> Expression js
haskellConAppArgVector object = Js.property object "data"

haskellConAppTag :: Javascript js => Expression js -> Expression js
haskellConAppTag object = Js.property object "tag"

haskellApplyMethodName :: Js.Id
haskellApplyMethodName = "hscall"

-- notEvaluated propery is undefined for primitive types
-- and undefined is treated as false in Javascript
haskellIsNotEvaluatedAndNotPrimitive :: Javascript js => Expression js -> Expression js
haskellIsNotEvaluatedAndNotPrimitive object = Js.property object "notEvaluated"

haskellMarkAsNotEvaluated :: Javascript js => Expression js -> js
haskellMarkAsNotEvaluated object = Js.assignProperty object "notEvaluated" Js.true

haskellMarkAsEvaluated :: Javascript js => Expression js -> js
haskellMarkAsEvaluated object = Js.assignProperty object "notEvaluated" Js.false

