module RTS.Objects where

import qualified Javascript.Language as Js
import Javascript.Language (Javascript, Expression)

root :: Javascript js => Expression js
root = Js.var "$hs"

true :: Javascript js => Expression js
true = Js.var "$$GHCziTypes_True"

false :: Javascript js => Expression js
false = Js.var "$$GHCziTypes_False"

makeThunkStub :: Javascript js => Expression js
makeThunkStub = Js.var "$Thunk" -- Js.property (Js.var "m") "T"

makeFuncStub :: Javascript js => Expression js
makeFuncStub = Js.var "$Func" -- Js.property (Js.var "m") "F"

makeDataStub :: Javascript js => Expression js
makeDataStub = Js.var "$Data" -- Js.property (Js.var "m") "D"

makeThunkLocalStub :: Javascript js => Expression js
makeThunkLocalStub = Js.var "$Thunk"

makeFuncLocalStub :: Javascript js => Expression js
makeFuncLocalStub = Js.var "$Func"

makeDataLocalStub :: Javascript js => Expression js
makeDataLocalStub = Js.var "$Data"

makeThunk :: Javascript js => Expression js
makeThunk = Js.var "$t"

makeFunc :: Javascript js => Expression js
makeFunc = Js.var "$f"

makeData :: Javascript js => Expression js
makeData = Js.var "$d"

returnData :: Javascript js => Expression js
returnData = Js.var "$R"

conAppArgVector :: Javascript js => Expression js -> Expression js
conAppArgVector object = Js.property object "v"

conAppTag :: Javascript js => Expression js -> Expression js
conAppTag object = Js.property object "g"

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

