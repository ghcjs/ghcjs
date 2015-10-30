{-# LANGUAGE JavaScriptFFI, OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.Char
import Data.IORef
import Data.Monoid

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception
import Control.Monad

import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Types
import GHCJS.Prim

import GHCJS.Marshal
import qualified Data.JSString as JSS
import qualified Data.JSString.Internal as JSSI

import System.Mem
import System.Mem.Weak

main :: IO ()
main = sequence_ [test1, test2, test3]

tou :: JSVal -> String
tou = map toUpper . JSS.unpack . jss
{-# NOINLINE tou #-}

-- sync callbacks without result
test1 :: IO ()
test1 = do
  js_log "test1"
  mv <- newEmptyMVar
  let jsa = jsval ("ab" :: JSString)
      jsb = jsval ("bc" :: JSString)
      jsc = jsval ("cd" :: JSString)
      x  = takeMVar mv >> js_log "mvar taken"
      x' = x `catch` \(_::WouldBlockException) -> js_log "would block"
      t0 n ob xx = t n (syncCallback ob xx) (js_runCallback n)
      t1 n ob xx = t n (syncCallback1 ob xx) (js_runCallback1 n jsa)
      t2 n ob xx = t n (syncCallback2 ob xx) (js_runCallback2 n jsa jsb)
      t3 n ob xx = t n (syncCallback3 ob xx) (js_runCallback3 n jsa jsb jsc)
      t name mkCB runCB = do
        js_log (">>> " <> name)
        cb <- mkCB
        runCB cb
        putMVar mv ()
        threadDelay 200000
        tryTakeMVar mv
        releaseCallback cb
  t0 "cb1" ContinueAsync   x  -- mvar taken async / result false
  t0 "cb2" ThrowWouldBlock x  -- result false
  t0 "cb3" ContinueAsync   x' -- mvar taken async / result false
  t0 "cb4" ThrowWouldBlock x' -- mvar not taken / would block printed / result true
  t1 "cb5" ThrowWouldBlock (js_log . JSS.pack . tou)
  t2 "cb6" ThrowWouldBlock (\a b -> js_log (JSS.pack $ tou a ++ tou b))
  t3 "cb7" ThrowWouldBlock (\a b c -> js_log (JSS.pack $ tou a ++ tou b ++ tou c))

-- sync callbacks with result
test2 :: IO ()
test2 = do
  js_log "test2"
  mv <- newEmptyMVar
  let jsa = jsval ("ab" :: JSString)
      jsb = jsval ("bc" :: JSString)
      jsc = jsval ("cd" :: JSString)
      s1       = return . jsval . JSS.pack . tou
      s2 a b   = return . jsval . JSS.pack $ tou a ++ tou b
      s3 a b c = return . jsval . JSS.pack $ tou a ++ tou b ++ tou c
      t0 n xx = t n (syncCallback' xx)  (js_runCallback n)
      t1 n xx = t n (syncCallback1' xx) (js_runCallback1 n jsa)
      t2 n xx = t n (syncCallback2' xx) (js_runCallback2 n jsa jsb)
      t3 n xx = t n (syncCallback3' xx) (js_runCallback3 n jsa jsb jsc)
      t name mkCB runCB = do
        js_log (">>> " <> name)
        cb <- mkCB
        runCB cb
        putMVar mv ()
        threadDelay 200000
        tryTakeMVar mv
        releaseCallback cb
      tmc = takeMVar mv `catch` \(_::WouldBlockException) -> js_log "would block"
  t1 "cb1" (return . jsval . JSS.pack . tou)
  t2 "cb2" (\a b -> return . jsval . JSS.pack $ tou a ++ tou b)
  t3 "cb3" (\a b c -> return . jsval . JSS.pack $ tou a ++ tou b ++ tou c)
  t1 "cb4" (\a     -> takeMVar mv >> s1 a)
  t2 "cb5" (\a b   -> takeMVar mv >> s2 a b)
  t3 "cb6" (\a b c -> takeMVar mv >> s3 a b c)
  t1 "cb7" (\a     -> tmc >> s1 a)
  t2 "cb8" (\a b   -> tmc >> s2 a b)
  t3 "cb9" (\a b c -> tmc >> s3 a b c)
  
test3 :: IO ()
test3 = do
  js_log "test3"
  ior <- newIORef "abc"
  w   <- mkWeakIORef ior (js_log "finalized")
  let v1 = jsval ("xyz" :: JSString)
  cb1 <- syncCallback  ThrowWouldBlock (readIORef ior >>= js_log . ("value: " <>))
  cb2 <- syncCallback1 ThrowWouldBlock
    (\x -> readIORef ior >>= \y -> js_log (jss x <> y))
  performGC
  js_runCallback  "cb1" cb1
  js_runCallback1 "cb2" v1 cb2 
  writeIORef ior "def"
  js_runCallback  "cb1" cb1
  js_runCallback1 "cb2" v1 cb2 
  js_log "test3.1"
  performGC
  threadDelay 500000
  js_log =<< maybe (return "<empty>") readIORef =<< deRefWeak w
  js_log "test3.2" -- finalizer should not have run
  releaseCallback cb1
  releaseCallback cb2
  performGC
  threadDelay 500000
  js_log =<< maybe (return "<empty>") readIORef =<< deRefWeak w
  js_log "test3.3" -- finalizer should have run

foreign import javascript unsafe
  "h$log($1);"
  js_log :: JSString -> IO ()

foreign import javascript unsafe
  "try { h$log($1 + ' result: ' + $2()) } catch(e) { h$log($1 + ' exception: ' + e); }"
  js_runCallback :: JSString -> Callback a -> IO ()

foreign import javascript unsafe
  "try { h$log($1 + ' result: ' + $3($2)) } catch(e) { h$log($1 + ' exception: ' + e); }"
  js_runCallback1 :: JSString -> JSVal -> Callback a -> IO ()

foreign import javascript unsafe
  "try { h$log($1 + ' result: ' + $4($2,$3)) } catch(e) { h$log($1 + ' exception: ' + e); }"
  js_runCallback2 :: JSString -> JSVal -> JSVal -> Callback a -> IO ()

foreign import javascript unsafe
  "try { h$log($1 + ' result: ' + $5($2,$3,$4)) } catch(e) { h$log($1 + ' exception: ' + e); }"
  js_runCallback3 :: JSString -> JSVal -> JSVal -> JSVal -> Callback a -> IO ()


foreign import javascript unsafe "$r = $1;" jss :: JSVal -> JSString
