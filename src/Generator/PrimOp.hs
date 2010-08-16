module Generator.PrimOp where

import StgSyn as Stg
import PrimOp
import Javascript.Language as Js

primitiveOperation :: Javascript js => PrimOp -> [StgArg] -> Expression js
primitiveOperation op _ = Js.unsafeStringToExpression alert
  where alert = concat ["$hs.alert ('primitive operation ", show op, ". Not implemeted yet.')"]

