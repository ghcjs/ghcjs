{-
  Test case for GHCJS #288, bug when waking up threads when committing
  an STM transaction.

  by Robert J. Macomber (rjmac)
 -}

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (unless)

data X = Wrong | Correct
  deriving (Show)

thread :: TVar Bool -> MVar X -> MVar X -> IO ()
thread tvar mvar result = do
  atomically $ do
    t <- readTVar tvar
    unless t retry
  m <- takeMVar mvar
  putMVar result m

main :: IO ()
main = do
  tvar <- newTVarIO False
  mvar <- newEmptyMVar
  result <- newEmptyMVar
  forkIO $ thread tvar mvar result
  threadDelay 200000 -- give the thread time to block
  atomically $ writeTVar tvar True
  threadDelay 200000 -- give the thread time to block again
  atomically $ writeTVar tvar False -- trigger a spurious wakeup
  threadDelay 200000 -- and give it time to wake up and retrieve the wrong value
  putMVar mvar Correct -- this _should_ have caused the real wakeup
  res <- takeMVar result -- This will likely actually crash because of broken invariants
  print res
