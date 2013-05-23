-- testcases by Ryan Yates, #7493

module Main where

import Control.Concurrent.STM

main = sequence_ [test1,test2,test3,test4,test5,test6]

-- should print []
test1 = do
  x <- atomically $ do
         r <- newTVar []
         writeTVar r [2]
         writeTVar r [] `orElse` return ()
         readTVar r
  print x

-- should print [1]
test2 = do
  x <- atomically $ do
         r <- newTVar [1]
         writeTVar r [2]
         writeTVar r [1] `orElse` return ()
         readTVar r
  print x

-- should print 1
test3 = do
  x <- atomically $ do
         r <- newTVar 1
         writeTVar r 2
         writeTVar r 1 `orElse` return ()
         readTVar r
  print x

-- should print Nothing
test4 = do
  x <- atomically $ do
         r <- newTVar Nothing
         writeTVar r (Just 2)
         writeTVar r Nothing `orElse` return ()
         readTVar r
  print x

-- should print 1
test5 = do
  x <- atomically $ do
         r <- newTVar 1
         writeTVar r 2
         writeTVar r 1 `orElse` return ()
         readTVar r
  print x

-- should print 1
test6 = do
  x <- atomically $ do
         let a = 1
         r <- newTVar a
         writeTVar r 2
         writeTVar r a `orElse` return ()
         readTVar r
  print x
