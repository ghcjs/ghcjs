module RTS.Objects where

import qualified Javascript.Language as Js
import Javascript.Language (Javascript, Expression)

root :: Javascript js => Expression js
root = Js.var "$hs"

modules :: Javascript js => Expression js
modules = Js.property root "modules"

moduleDependencies :: Javascript js => Expression js -> Expression js
moduleDependencies mod = Js.property mod "dependencies"

true :: Javascript js => Expression js
true = modules # "GHCziBool" # "hs_True"
  where (#) = Js.property

false :: Javascript js => Expression js
false = modules # "GHCziBool" # "hs_False"
  where (#) = Js.property

thunk :: Javascript js => Expression js
thunk = Js.property root "Thunk"

func :: Javascript js => Expression js
func = Js.property root "Func"

conApp :: Javascript js => Expression js
conApp = Js.property root "Data"

thunkEvalOnceFunctionName :: Js.Id
thunkEvalOnceFunctionName = "evaluateOnce"

evalFunctionName :: Js.Id
evalFunctionName = "evaluate"

conAppArgVector :: Javascript js => Expression js -> Expression js
conAppArgVector object = Js.property object "data"

conAppTag :: Javascript js => Expression js -> Expression js
conAppTag object = Js.property object "tag"

applyMethodName :: Js.Id
applyMethodName = "hscall"

-- notEvaluated propery is undefined for primitive types
-- and undefined is treated as false in Javascript
isNotEvaluatedAndNotPrimitive :: Javascript js => Expression js -> Expression js
isNotEvaluatedAndNotPrimitive object = Js.property object "notEvaluated"

markAsNotEvaluated :: Javascript js => Expression js -> js
markAsNotEvaluated object = Js.assignProperty object "notEvaluated" Js.true

markAsEvaluated :: Javascript js => Expression js -> js
markAsEvaluated object = Js.assignProperty object "notEvaluated" Js.false

