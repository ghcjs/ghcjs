module Generator.FFI where

import StgSyn as Stg
import PrimOp
import qualified Javascript.Language as Js

foreignFunctionCall :: a -> b -> c -> Js.Expression
foreignFunctionCall _ _ _ = Js.unsafeStringToExpression "$hs.alert ('Unsupported: foreign function call')"

primitiveCall :: PrimCall -> [StgArg] -> Js.Expression
primitiveCall _ _ = Js.unsafeStringToExpression "$hs.alert ('Unsupported: primitive call')"

