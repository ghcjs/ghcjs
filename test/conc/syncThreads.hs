{-# LANGUAGE ScopedTypeVariables, JavaScriptFFI, ForeignFunctionInterface #-}

module Main where

import Prelude hiding (print, putStrLn)

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import Data.List (intersperse)

import GHCJS.Types
import GHCJS.Concurrent
import GHCJS.Foreign.Callback

import qualified Data.JSString as JSS

---------------------------------------------------------------------------
-- our usual standard io implementation is asynchronous, use this more
-- primitive mechanism to print the results

print :: Show a => a -> IO ()
print = putStrLn . show

putStrLn :: String -> IO ()
putStrLn = js_log . JSS.pack

foreign import javascript unsafe
  "console.log($1);"
  js_log :: JSString -> IO ()

---------------------------------------------------------------------------

foreign import javascript unsafe
  "$1();"
  js_runCallback :: Callback a -> IO ()

---------------------------------------------------------------------------

main :: IO ()
main = sequence_ .
       intersperse (putStrLn "---------------------------") $
         [ synchronously1
         , wouldBlock1
         , wouldBlock2
         , callback1
         , callback2
         , callback3
         ]

printIsSync :: IO ()
printIsSync = do
  tid  <- myThreadId
  sync <- isThreadSynchronous tid
  ca   <- isThreadContinueAsync tid
  print (sync, ca)

printMVar :: MVar () -> IO ()
printMVar = print <=< takeMVar

synchronously1 :: IO ()
synchronously1 = do
  putStrLn "synchronously1"
  printIsSync
  synchronously printIsSync
  printIsSync
  let h x m = m `catch` \(e::SomeException) -> do
                           putStrLn ("handler: " ++ x)
                           printIsSync
  h "outside" (synchronously $ printIsSync >> error "err")
  printIsSync
  synchronously (h "inside" (printIsSync >> error "err"))
  printIsSync
  putStrLn "synchronously1 done"
  
-- blocking on MVar should give us an exception, the exception can be handled
wouldBlock1 :: IO ()
wouldBlock1 = do
  putStrLn "wouldBlock1"
  x `catch` \(e::SomeException) ->
    do putStrLn "exception caught: outer"
       print e
  putStrLn "wouldBlock1 done"
  where
    x = do
      mv1 <- newEmptyMVar
      printIsSync
      synchronously $ do
        printIsSync
        printMVar mv1 `catch` \(e::SomeException) -> do
          putStrLn "exeption caught inner1"
          print e
        printMVar mv1 `catch` \(e::WouldBlockException) -> do
          putStrLn "exeption caught inner2"
          print e
        putStrLn "ok"
        printMVar mv1
        putStrLn "unreachable"
      printIsSync

-- threadDelay should give us the exception too
wouldBlock2 :: IO ()
wouldBlock2 = do
  putStrLn "wouldBlock2"
  threadDelay 500000
  let x = synchronously $ do
            threadDelay 500000 `catch` \(e::WouldBlockException) ->
              putStrLn "exception caught: inner"
            printIsSync
            putStrLn "ok"
            threadDelay 500000
            putStrLn "unreachable"
  x `catch` \(e::WouldBlockException) ->
    putStrLn "exception caught: outer"
  putStrLn "wouldBlock2 done"

-- synchronous callbacks give us an exception
callback1 :: IO ()
callback1 = do
  putStrLn "callback1"
  mv1 <- newEmptyMVar
  cb1 <- syncCallback ThrowWouldBlock $ do
    printIsSync
    putStrLn "ok"
    printMVar mv1 `catch` \(e::WouldBlockException) ->
      putStrLn "exception: would block"
    printIsSync
    putStrLn "ok"
    printMVar mv1
    -- thread would block error ends up on stderr
    putStrLn "unreachable"
  js_runCallback cb1
  putStrLn "callback1 finished"
  releaseCallback cb1

callback2 :: IO ()
callback2 = do
  putStrLn "callback2"
  mv1 <- newEmptyMVar
  mv2 <- newEmptyMVar
  cb1 <- syncCallback ContinueAsync $ do
    printIsSync
    putStrLn "ok"
    printMVar mv1
    putStrLn "callback"
    printIsSync
    putMVar mv2 ()
  js_runCallback cb1
  putStrLn "main"
  putMVar mv1 ()
  printMVar mv2
  putStrLn "main"
  putStrLn "callback2 done"  

-- async callbacks are async
callback3 :: IO ()
callback3 = do
  putStrLn "callback3"
  mv1 <- newEmptyMVar
  cb1 <- asyncCallback $ do
    putStrLn "async callback"
    printIsSync
    putMVar mv1 ()
    printMVar mv1
    putStrLn "callback"
    putMVar mv1 ()
  js_runCallback cb1
  printMVar mv1
  putStrLn "main"
  putMVar mv1 ()
  printMVar mv1
  putStrLn "main"
  printIsSync
  releaseCallback cb1
  putStrLn "callback3 done"
