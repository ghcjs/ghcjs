{-# LANGUAGE JavaScriptFFI, CPP #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar

#ifdef __GHCJS__
foreign import javascript unsafe "h$runSync(h$mainZCMainzisync)" startSync :: IO ()
#else
startSync :: IO ()
startSync = return ()
#endif

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-# NOINLINE fib36 #-}
fib36 :: Int
fib36 = fib 36

main :: IO ()
main = do
  mv <- newEmptyMVar
  forkIO $ do
    putStrLn "forked started"
    putMVar mv ()
    fib36 `seq` putStrLn ("forked: " ++ show fib36)
  _ <- takeMVar mv
  threadDelay 10000
  startSync
  threadDelay 4000000

-- synchronous thread, should bump into black hole from
-- forked thread, continue computation
sync = fib36 `seq` putStrLn ("sync: " ++ show fib36)
