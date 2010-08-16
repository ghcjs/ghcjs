module Generator.FFI where

import StgSyn as Stg
import PrimOp
import Javascript.Language as Js

foreignFunctionCall :: Javascript js => a -> b -> c -> Expression js
foreignFunctionCall _ _ _ = Js.unsafeStringToExpression "$hs.alert ('Unsupported: foreign function call')"

primitiveCall :: Javascript js => PrimCall -> [StgArg] -> Expression js
primitiveCall _ _ = Js.unsafeStringToExpression "$hs.alert ('Unsupported: primitive call')"

