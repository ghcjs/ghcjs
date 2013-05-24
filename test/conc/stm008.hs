module Main where

-- test case by Ryan Yates

import Control.Concurrent
import Control.Concurrent.STM

test c = do
    y <- atomically $ do
        always $ do
            always $ do
                x <- readTVar c
                if x
                  then return True
                  else retry
            return True
        readTVar c
    print y

main = do
    c <- newTVarIO False
    forkIO (test c)
    threadDelay 500000
    atomically (writeTVar c True)
    putStrLn "Done."
