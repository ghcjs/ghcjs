{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Concurrent.STM
import qualified Control.Exception as E

main = do
  x <- atomically (newTVar 0)
  y <- atomically (newTVar 1)
  atomically (always $ invariant x y)
  updates x y `E.catch` \(e :: E.SomeException) -> print e
  print =<< atomically (readTVar x)
  print =<< atomically (readTVar y)

updates x y = do
  putStrLn "first"
  atomically (writeTVar x 25) -- this should add `y' to the watched variables for the invariant
  putStrLn "second"
  atomically (writeTVar y 10) -- this should fail
  putStrLn "third"
  atomically (writeTVar x 25)
  putStrLn "fourth"

-- check that x*y < 100
invariant :: TVar Integer -> TVar Integer -> STM Bool
invariant x y = do
  xv <- readTVar x
  if xv == 0 then return True
             else do
               yv <- readTVar y
               return (xv*yv < 100)
