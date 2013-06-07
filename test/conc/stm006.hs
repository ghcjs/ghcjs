{-# LANGUAGE ScopedTypeVariables #-}

-- testcases by Ryan Yates

module Main where

import Control.Concurrent.STM
import Control.Applicative
import qualified Control.Exception as E

test x = x `E.catch` \(e::E.SomeException) -> print e

main = do
  putStrLn "1"

  test $ do
    x <- atomically $
        do a <- newTVar True
           (always (readTVar a) >> retry) `orElse` return ()
           return a
    atomically (writeTVar x False) -- Should not and does not fail

  putStrLn "2 should fail"

  test $ do
    y <- atomically $
        do a <- newTVar True
           always (readTVar a) `orElse` return ()
           return a
    atomically (writeTVar y False) -- Should fail, but does not!

  putStrLn "3 should fail"

  test $ do
    z <- atomically $
        do a <- newTVar True
           always (readTVar a)
           return a
    atomically (writeTVar z False) -- should and does fail

  putStrLn "."
