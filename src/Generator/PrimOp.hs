module Generator.PrimOp where

import StgSyn as Stg
import PrimOp
import qualified Javascript.Language as Js

primitiveOperation :: PrimOp -> [StgArg] -> Js.Expression
primitiveOperation op _ = Js.unsafeStringToExpression alert
  where alert = concat ["$hs.alert ('primitive operation ", show op, ". Not implemeted yet.')"]

