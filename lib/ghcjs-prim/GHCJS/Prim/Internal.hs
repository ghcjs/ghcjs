{- | Code used by the RTS

 -}

#include "foreign-compat.h"

module GHCJS.Prim.Internal ( blockedIndefinitelyOnMVar
                           , blockedIndefinitelyOnSTM
                           , wouldBlock
                           , ignoreException
                           , setCurrentThreadResultException
                           , setCurrentThreadResultValue
                           ) where

import           Control.Exception

import           GHCJS.Prim
import           GHC.Exts

wouldBlock :: SomeException
wouldBlock = toException WouldBlockException

blockedIndefinitelyOnMVar :: SomeException
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

blockedIndefinitelyOnSTM :: SomeException
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

setCurrentThreadResultException :: SomeException -> IO ()
setCurrentThreadResultException e
  | Just WouldBlockException <- fromException e =
      js_setCurrentThreadResultWouldBlock
  | Just (JSException v _) <- fromException e =
      js_setCurrentThreadResultJSException v
  | otherwise =
      js_setCurrentThreadResultHaskellException (toJSString (show e))

setCurrentThreadResultValue :: IO JSVal -> IO ()
setCurrentThreadResultValue x = js_setCurrentThreadResultValue =<< x

FOREIGN_IMPORT(unsafe, js_setCurrentThreadResultWouldBlock, IO (), "h$setCurrentThreadResultWouldBlock();")
FOREIGN_IMPORT(unsafe, js_setCurrentThreadResultJSException, JSVal -> IO (), "h$setCurrentThreadResultJSException($1);")
FOREIGN_IMPORT(unsafe, js_setCurrentThreadResultHaskellException, JSVal -> IO (), "h$setCurrentThreadResultHaskellException($1);")
FOREIGN_IMPORT(unsafe, js_setCurrentThreadResultValue, JSVal -> IO (), "h$setCurrentThreadResultValue($1);")
