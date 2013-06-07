-- based on t7493 report by Patrick Palka

module Main where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Monad
import Control.Applicative

main = do
    q <- newTQueueIO
    atomically $ writeTQueue q True
    atomically $ writeTQueue q False
    replicateM_ 10 $ do
        xs <- atomically $ many $ readTQueue q
        print xs
        threadDelay 50000
