-- Test for bug #418

module Main where

import Control.Concurrent
import System.IO.Unsafe (unsafeInterleaveIO)

main = do
    v <- newEmptyMVar
    a <- unsafeInterleaveIO (readMVar v)
    t <- forkIO (putStrLn "thread 1" >> print a)
    threadDelay (100*1000)
    killThread t
    forkIO $ putStrLn "thread 2" >> print a
    putMVar v ()
    threadDelay (100*1000)
    return ()

