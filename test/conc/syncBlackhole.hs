{-# LANGUAGE JavaScriptFFI #-}
module Main where

import Control.Concurrent
import Control.Concurrent.MVar

import Prelude hiding (putStrLn)
import qualified Data.JSString as JSS

foreign import javascript unsafe "h$runSync(h$mainZCMainzisync)" startSync :: IO ()

{-
   standard IO is implemented with async callbacks in node.js, use a
   synchronous alternative to avoid the WouldBlock exception
 -}
foreign import javascript unsafe "console.log($1);" js_log :: JSS.JSString -> IO ()

putStrLn :: String -> IO ()
putStrLn = js_log . JSS.pack

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

{-# NOINLINE fib36 #-}
fib36 :: Int
fib36 = fib 36

main :: IO ()
main = do
  mv <- newEmptyMVar
  forkIO $ do
    putStrLn "forked started"
    putMVar mv ()
    fib36 `seq` putStrLn ("forked: " ++ show fib36)
  _ <- takeMVar mv
  threadDelay 10000
  startSync
  threadDelay 4000000

-- synchronous thread, should bump into black hole from
-- forked thread, continue computation
sync = fib36 `seq` putStrLn ("sync: " ++ show fib36)
