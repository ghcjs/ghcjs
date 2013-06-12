-- from the Haskell wiki: http://www.haskell.org/haskellwiki/Simple_STM_example

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

main = do shared <- atomically $ newTVar 0
          before <- atomRead shared
          putStrLn $ "Before: " ++ show before
--          forkIO $ 25 `timesDo` (dispVar shared >> milliSleep 20)
          forkIO $ 10 `timesDo` (appV ((+) 2) shared >> milliSleep 50)
          forkIO $ 20 `timesDo` (appV pred shared >> milliSleep 25)
          milliSleep 6000
          after <- atomRead shared
          putStrLn $ "After: " ++ show after
 where timesDo = replicateM_
       milliSleep = threadDelay . (*) 1000

atomRead = atomically . readTVar
dispVar x = atomRead x >>= print
appV fn x = atomically $ readTVar x >>= writeTVar x . fn

