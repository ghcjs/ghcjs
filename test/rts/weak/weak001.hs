module Main where

import Control.Concurrent
import System.Mem
import System.Mem.Weak
import Data.IORef

delay :: IO ()
delay = performGC >> threadDelay 2000000 >> performGC

main :: IO ()
main = test1 >> test2 >> test3 >> test4

test1 :: IO ()
test1 = do
  putStrLn "# test1"
  x  <- newIORef "xyz"
  wx <- mkWeakIORef x (putStrLn "weak IORef finalized")
  delay
  putStrLn . ("1: " ++) =<< maybe (return "<empty>") readIORef =<< deRefWeak wx
  delay
  putStrLn . ("2: " ++) =<< readIORef x
  delay
  putStrLn "final wait"
  delay
  putStrLn "done"

test2 :: IO ()
test2 = do
  putStrLn "# test2"
  x <- newIORef "xyz"
  y <- newIORef "yzx"
  z <- newIORef "zxy" 
  wx <- mkWeakIORef x (putStrLn "x: weak IORef finalized" >> readIORef y >>= putStrLn)
  wy <- mkWeakIORef y (putStrLn "y: weak IORef finalized" >> readIORef z >>= putStrLn)
  wz <- mkWeakIORef z (putStrLn "z: weak IORef finalized" >> readIORef x >>= putStrLn)
  delay
  putStrLn . ("1: " ++) =<< maybe (return "<empty>") readIORef =<< deRefWeak wx
  delay
  putStrLn . ("2: " ++) =<< readIORef x
  delay
  putStrLn "final wait"
  delay
  putStrLn "done"

test3 :: IO ()
test3 = do
  putStrLn "# test3"
  x <- newIORef "x"
  wx <- mkWeakIORef x (putStrLn "wx finalized")
  putStrLn . ("1: " ++) =<< maybe (return "<empty>") readIORef =<< deRefWeak wx
  finalize wx
  delay
  putStrLn . ("2: " ++) =<< maybe (return "<empty>") readIORef =<< deRefWeak wx
  putStrLn . ("3: " ++) =<< readIORef x

-- key keeps value alive even if weak reference is unreachable
test4 :: IO ()
test4 = do
  putStrLn "# test4"
  x <- newIORef "x"
  wx <- mkWeakIORef x (putStrLn "wx finalized")
  putStrLn . ("1: " ++) =<< readIORef x
  delay
  putStrLn . ("2: " ++) =<< readIORef x
  -- finalization should happen here
  delay
  putStrLn "done"
