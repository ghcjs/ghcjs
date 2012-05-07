module RTS.Objects where

import qualified Javascript.Language as Js
import Javascript.Language (Javascript, Expression)

root :: Javascript js => Expression js
root = Js.var "$hs"

true :: Javascript js => Expression js
true = Js.var "$$GHCziTypes_True"

false :: Javascript js => Expression js
false = Js.var "$$GHCziTypes_False"

makeThunk :: Javascript js => Expression js -> Maybe ([Expression js]) -> [Expression js] -> Expression js
makeThunk f (Just live) info = Js.nativeFunctionCall (Js.var "$t") ([f, Js.list live] ++ info)
makeThunk f Nothing info = Js.nativeFunctionCall (Js.var "$T") (f:info)

makeFunc :: Javascript js => Int -> Expression js -> Maybe ([Expression js]) -> [Expression js] -> Expression js
makeFunc arity f (Just live) info = Js.nativeFunctionCall (Js.var "$f") ([Js.int arity, f, Js.list live] ++ info)
makeFunc arity f Nothing info = Js.nativeFunctionCall (Js.var "$F") ([Js.int arity, f] ++ info)

makeDataF :: Javascript js => Int -> Expression js -> [Expression js] -> Expression js
makeDataF tag f info = Js.nativeFunctionCall (Js.var "$D") ([Js.int tag, f] ++ info)

makeDataValue :: Javascript js => Int -> [Expression js] -> [Expression js] -> Expression js
makeDataValue tag v info = Js.nativeFunctionCall (Js.var "$d") ([Js.int tag, Js.list v] ++ info)

setLiveList :: Javascript js => Expression js -> [Expression js] -> Expression js
setLiveList o live = Js.nativeFunctionCall (Js.var "$S") [o, Js.list live]

returnData :: Javascript js => Int -> [Expression js] -> Expression js -> Expression js
returnData tag v info = Js.nativeFunctionCall (Js.var "$R") ([Js.int tag, Js.list v] ++ [info])

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

