{-# LANGUAGE CPP #-}

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
import Javascript.Language as Js hiding(return)
import qualified Javascript.Language as Js (return)
import Generator.Helpers

returnForeignFunctionCallResult :: Javascript js => ForeignCall -> Stg.Unique -> [StgArg] -> Gen js
returnForeignFunctionCallResult (CCall (CCallSpec target _ccallConv _safety)) _ args =
  case target
  of DynamicTarget -> return . Js.throw . Js.string $ "Unsupported: foreign function call"
#if GHC7
     (StaticTarget clabelString _) -> do
#else
     (StaticTarget clabelString) -> do
#endif
           a <- mapM stgArgToJs args
           return . Js.return $ foreignCall clabelString a

declareForeignFunctionCallResult :: Javascript js => Stg.Id -> ForeignCall -> Stg.Unique -> [StgArg] -> Gen js
declareForeignFunctionCallResult binder (CCall (CCallSpec target _ccallConv _safety)) _ args =
  case target
  of DynamicTarget -> return . Js.throw . Js.string $ "Unsupported: foreign function call"
#if GHC7
     (StaticTarget clabelString _) -> do
#else
     (StaticTarget clabelString) -> do
#endif
            b <- stgIdToJsId binder
            a <- mapM stgArgToJs args
            return $ Js.declare [(b, foreignCall clabelString a)]

foreignCall :: Javascript js => FastString -> [Expression js] -> Expression js
foreignCall clabelString args =
  Js.list [last $ args, nativeFunctionCall (Js.var . zDecodeString . unpackFS $ clabelString) (init args)]

returnPrimitiveCallResult :: Javascript js => PrimCall -> [StgArg] -> Gen js
returnPrimitiveCallResult _ _ = return $ Js.throw . Js.string $ "Unsupported: primitive call"

declarePrimitiveCallResult :: Javascript js => Stg.Id -> PrimCall -> [StgArg] -> Gen js
declarePrimitiveCallResult _ _ _ = return $ Js.throw . Js.string $ "Unsupported: primitive call"

