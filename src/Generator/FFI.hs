module Generator.FFI
  ( returnForeignFunctionCallResult
  , declareForeignFunctionCallResult
  , returnPrimitiveCallResult
  , declarePrimitiveCallResult
  ) where

import Id as Stg
import Unique as Stg
import StgSyn as Stg
import ForeignCall (ForeignCall (CCall), CCallSpec (CCallSpec), CCallTarget (DynamicTarget, StaticTarget))
import FastString (FastString, unpackFS)
import Encoding (zDecodeString)
import PrimOp
import Javascript.Language as Js
import Generator.Helpers

returnForeignFunctionCallResult :: Javascript js => ForeignCall -> Stg.Unique -> [StgArg] -> js
returnForeignFunctionCallResult (CCall (CCallSpec target _ccallConv _safety)) _ args =
  case target
  of DynamicTarget -> Js.throw . Js.string $ "Unsupported: foreign function call"
     (StaticTarget clabelString _) ->
       Js.return $ foreignCall clabelString args

declareForeignFunctionCallResult :: Javascript js => Stg.Id -> ForeignCall -> Stg.Unique -> [StgArg] -> js
declareForeignFunctionCallResult binder (CCall (CCallSpec target _ccallConv _safety)) _ args =
  case target
  of DynamicTarget -> Js.throw . Js.string $ "Unsupported: foreign function call"
     (StaticTarget clabelString _) ->
       Js.declare (stgIdToJsId binder) $ foreignCall clabelString args

foreignCall :: Javascript js => FastString -> [StgArg] -> Expression js
foreignCall clabelString args =
  Js.list [stgArgToJs . last $ args, nativeFunctionCall (Js.var . zDecodeString . unpackFS $ clabelString) (map stgArgToJs . init $ args)]

returnPrimitiveCallResult :: Javascript js => PrimCall -> [StgArg] -> js
returnPrimitiveCallResult _ _ = Js.throw . Js.string $ "Unsupported: primitive call"

declarePrimitiveCallResult :: Javascript js => Stg.Id -> PrimCall -> [StgArg] -> js
declarePrimitiveCallResult _ _ _ = Js.throw . Js.string $ "Unsupported: primitive call"

