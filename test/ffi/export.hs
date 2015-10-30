{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad

import Data.IORef
import Debug.Trace

import System.Mem.Weak
import System.Mem

import GHCJS.Foreign.Export

main :: IO ()
main = sequence_ [test1, test2, test3, test4]

p :: Weak (IORef String) -> Export (IORef String) -> IO ()
p w x = do
  derefExport x >>= \case
    Nothing -> putStrLn "<empty export>"
    Just r  -> readIORef r >>= putStrLn
  deRefWeak w >>= \case
    Nothing -> putStrLn "<empty weak>"
    Just r  -> readIORef r >>= putStrLn

-- simple export, with finalizer
test1 :: IO ()
test1 = do
  let 
  putStrLn "test1"
  ior <- newIORef "xyz"
  w   <- mkWeakIORef ior (putStrLn "ioref finalized")
  r'  <- withExport ior $ \r -> do
    performGC
    p w r
    return r
  performGC
  p w r'

-- export with unboxed representation
test2 :: IO ()
test2 = do
  putStrLn "test2"
  let x = 42424242 :: Int
  i <- js_getInt
  x' <- evaluate (x*i)
  r' <- withExport x' $ \r -> do
    performGC
    print =<< derefExport r
    return r
  performGC
  print =<< derefExport r'

-- manual release
test3 :: IO ()
test3 = do
  let pr xs = derefExport >=> maybe (return "<empty>") readIORef >=> putStrLn . ((xs ++ " ") ++)
  putStrLn "test3"
  ior <- newIORef "xyz"
  e <- export ior
  forkIO $ do
    performGC
    threadDelay 200000
    pr "forked" e
    releaseExport e
    pr "forked" e
  performGC
  pr "main" e
  threadDelay 400000
  pr "main" e

-- test4: export falsy values
test4 :: IO ()
test4 = do
  putStrLn "test4"
  i1 <- js_getInt
  i2 <- js_getInt
  v1 <- evaluate (i1 /= i2)
  v2 <- evaluate (i1 - i2)
  withExport v1 $ derefExport >=> print 
  withExport v2 $ derefExport >=> print

foreign import javascript unsafe "$r = 2;" js_getInt :: IO Int
