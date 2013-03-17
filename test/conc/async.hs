{-# LANGUAGE ScopedTypeVariables #-}
-- test for async exceptions
module Main where

import Control.Concurrent
import Control.Concurrent.MVar
import qualified Control.Exception as E

main = do
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar
  t1 <- forkIO $ do
          takeMVar mv1 `E.catch` \(e::E.SomeException) -> putStrLn ("1: " ++ show e)
          putStrLn "1"
  t2 <- forkIO $ do
          takeMVar mv1 `E.catch` \(e::E.SomeException) -> putStrLn ("2: " ++ show e)
          putStrLn "2"
  t3 <- forkIO $ do
          threadDelay 100000
          takeMVar mv1
          putStrLn "3"
          putMVar mv2 ()
  threadDelay 100000
  putStrLn "main"
  throwTo t1 E.Overflow
  threadDelay 100000
  putMVar mv1 ()
  threadDelay 100000
  putMVar mv1 ()
  takeMVar mv2

