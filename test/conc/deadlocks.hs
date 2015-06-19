{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, ScopedTypeVariables #-}
{- 
   some tests for the storage manager

   checks that deadlocks are detected and dealt with properly
-}
module Main where

import System.Mem
import System.Mem.Weak
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Monad
import Data.Maybe

import qualified Control.Exception as E

delay :: IO ()
delay = threadDelay 4000000 -- long enough for GC to kick in

foreign import javascript interruptible "setTimeout($c, 4000);" js_delay :: IO ()

expectException :: String -> IO () -> IO ()
expectException xs m = (m >> putStrLn ("ERROR: expected exception: " ++ xs))
                       `E.catch`
                       \(_::E.SomeException) -> putStrLn ("OK: " ++ xs)

printException :: String -> IO () -> IO ()
printException xs m = m `E.catch` \(e::E.SomeException) -> putStrLn (xs ++ ": " ++ show e)

main :: IO ()
main = do
  testFFI
  testMVar
  testSTM
  testWeak

testFFI :: IO ()
testFFI = putStrLn "testFFI" >> sequence_ [test1]
  where
    test1 = do
      mv1 <- newEmptyMVar
      forkIO $ js_delay >> putMVar mv1 "FFI test 1 OK"
      putStrLn =<< takeMVar mv1

testMVar :: IO ()
testMVar = putStrLn "testMVar" >> sequence_ [test1,test2,test3]
  where
    -- unreachable MVar
    test1 = expectException "MVar test 1" $ do 
      mv1 <- newEmptyMVar
      void (takeMVar mv1)
    test2 = do
      mv1 <- newEmptyMVar
      mv2 <- newEmptyMVar
      let thread a b = forkIO $ printException "MVar test 2" (void $ takeMVar a) >> putMVar b () >> putStrLn "OK"
      -- one of these should get an exception, both should finish and print OK
      thread mv1 mv2
      thread mv2 mv1
      delay
    test3 = do
      let chain mv1 0 = return mv1
          chain mv1 n = do
            mv2 <- newEmptyMVar
            forkIO $ takeMVar mv1 >>= \v -> threadDelay 1000000 >> putMVar mv2 v
            return mv2
      mv1 <- newEmptyMVar
      mv2 <- chain mv1 50
      delay
      putMVar mv1 "MVar test3: OK"
      putStrLn =<< takeMVar mv2

testWeak :: IO ()
testWeak = putStrLn "testWeak" >> sequence_ [test1]
  where
    test1 = do
      mv <- newEmptyMVar
      wmv1 <- mkWeakMVar mv (putStrLn "Weak test 1 finalizer")
      putStrLn "Weak test 1"
      forkIO $ delay >> putMVar mv "OK"
      print . isJust =<< deRefWeak wmv1
      putStrLn =<< takeMVar mv
      delay
      print . isJust =<< deRefWeak wmv1


testSTM :: IO ()
testSTM = putStrLn "testSTM" >> sequence_ [test1]
  where
    test1 = expectException "STM test 1" $ do
      tv1 <- atomically (newTVar (0::Int))
      forkIO $ atomically (readTVar tv1 >>= \v -> if v < 1 then retry else writeTVar tv1 (v+1))
      delay
      atomically (modifyTVar tv1 (+1))
      print =<< atomically (readTVar tv1 >>= \v -> if v < 2 then retry else return v)
      atomically (readTVar tv1 >>= \v -> if v < 3 then retry else return ())
      putStrLn "ERROR"

