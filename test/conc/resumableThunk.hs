-- test for resumable thunks:
-- kill thread while it's computing fib 35
-- main thread should resume the computation

module Main where

import Control.Concurrent

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-2) + fib (n-1)

fib35 = fib 35

main = do
  t <- forkIO (putStrLn $ "1: " ++ show fib35)
  threadDelay 100000
  killThread t
  -- block on fib35 as quickly as possible, the async exception will unblock it
  fib35 `seq` (putStrLn $ "2: " ++ show fib35)

