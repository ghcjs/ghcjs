module Generator.FFI where

import Id as Stg
import StgSyn as Stg
import PrimOp
import Javascript.Language as Js

returnForeignFunctionCallResult :: Javascript js => a -> b -> c -> js
returnForeignFunctionCallResult _ _ _ = Js.throw . Js.string $ "Unsupported: foreign function call"

bindForeignFunctionCallResult :: Javascript js => Stg.Id -> a -> b -> c -> js
bindForeignFunctionCallResult _ _ _ _ = Js.throw . Js.string $ "Unsupported: foreign function call"

returnPrimitiveCallResult :: Javascript js => PrimCall -> [StgArg] -> js
returnPrimitiveCallResult _ _ = Js.throw . Js.string $ "Unsupported: primitive call"

bindPrimitiveCallResult :: Javascript js => Stg.Id -> PrimCall -> [StgArg] -> js
bindPrimitiveCallResult _ _ _ = Js.throw . Js.string $ "Unsupported: primitive call"

