{-# LANGUAGE JavaScriptFFI, CPP #-}

module Main where

import GHCJS.Foreign
import GHCJS.Types

#ifdef __GHCJS__
foreign import javascript unsafe "$1();" runCallback :: JSFun (IO ()) -> IO ()
#else
runCallback :: JSFun (IO ()) -> IO ()
runCallback = error "not JavaScript"
#endif

main = do
  s <- syncCallback (putStrLn "b")
  putStrLn "a"
  runCallback s
  putStrLn "c"

