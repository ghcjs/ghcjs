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
import PrimOp
import Javascript.Language as Js hiding(return)
import qualified Javascript.Language as Js (return)
import Generator.Helpers
import Data.Monoid (mconcat)

returnForeignFunctionCallResult :: Javascript js => ForeignCall -> Stg.Unique -> [StgArg] -> Gen js
returnForeignFunctionCallResult (CCall (CCallSpec target _ccallConv _safety)) _ args = do
  let func = case target of
                DynamicTarget                 -> "$hs_dynamicCall"
#if __GLASGOW_HASKELL__ >= 706
                StaticTarget clabelString _ _ -> unpackFS clabelString
#else
                StaticTarget clabelString _   -> unpackFS clabelString
#endif
  a <- mapM stgArgToJs args
  return $ mconcat [
      foreignCall func a
    , Js.return $ Js.list [last $ a, Js.var "$ff"]]

declareForeignFunctionCallResult :: Javascript js => Stg.Id -> ForeignCall -> Stg.Unique -> [StgArg] -> Gen js
declareForeignFunctionCallResult binder (CCall (CCallSpec target _ccallConv _safety)) _ args = do
  let func = case target of
                DynamicTarget                 -> "$hs_dynamicCall"
#if __GLASGOW_HASKELL__ >= 706
                StaticTarget clabelString _ _ -> unpackFS clabelString
#else
                StaticTarget clabelString _   -> unpackFS clabelString
#endif
  b <- stgIdToJsId binder
  a <- mapM stgArgToJs args
  return $ mconcat [
      foreignCall func a
    , Js.declare [(b, Js.list [last $ a, Js.var "$ff"])]]

foreignCall :: Javascript js => String -> [Expression js] -> js
foreignCall clabelString args =
  Js.declare [("$ff", nativeFunctionCall (Js.var clabelString) (init args))]

returnPrimitiveCallResult :: Javascript js => PrimCall -> [StgArg] -> Gen js
returnPrimitiveCallResult (PrimCall clabelString _) args = do
    a <- mapM stgArgToJs args
    return . Js.return $ nativeFunctionCall (Js.var . unpackFS $ clabelString) a

declarePrimitiveCallResult :: Javascript js => Stg.Id -> PrimCall -> [StgArg] -> Gen js
declarePrimitiveCallResult binder (PrimCall clabelString _) args = do
    b <- stgIdToJsId binder
    a <- mapM stgArgToJs args
    return $ Js.declare [(b, nativeFunctionCall (Js.var . unpackFS $ clabelString) a)]

