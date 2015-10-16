{- | Code used by the RTS

 -}

module GHCJS.Prim.Internal ( blockedIndefinitelyOnMVar
                           , blockedIndefinitelyOnSTM
                           , wouldBlock
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


