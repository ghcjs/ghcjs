{-# LANGUAGE JavaScriptFFI, InterruptibleFFI #-}
{-
 Test whether throwing exceptions from JavaScript gives the expected result
-}

module Main where

import           Control.Concurrent
import qualified Control.Exception as Ex
import           System.IO

import           GHCJS.Prim

foreign import javascript unsafe
    "throw 'unsafe exception';" js_unsafeExcep              :: IO ()
foreign import javascript safe
   "throw 'safe exception';"  js_safeExcep                  :: IO ()
foreign import javascript interruptible
   "throw 'interruptible exception';" js_interruptibleExcep :: IO ()

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  testHandle "unsafe"        js_unsafeExcep
  testHandle "safe"          js_safeExcep
  testHandle "interruptible" js_interruptibleExcep

testHandle :: String -> IO () -> IO ()
testHandle descr a = do
  putStrLn ("running " ++ descr ++ " no handler")
  forkIO $ do
    putStrLn "before"
    a
    putStrLn "after"
  threadDelay 500000
  putStrLn ("running " ++ descr ++ " handler")
  forkIO $ do
    putStrLn "before"
    a `Ex.catch` theHandler
    putStrLn "after"
  threadDelay 500000

theHandler :: Ex.SomeException -> IO ()
theHandler e = putStrLn ("got exception: " ++ show e)
