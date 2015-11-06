{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Main where

import           Control.Concurrent
import           Control.Exception

import qualified Data.JSString as JSS
import           Data.Monoid

import           JavaScript.Web.Location
import           JavaScript.Web.WebSocket (WebSocketRequest(..), WebSocket)
import qualified JavaScript.Web.WebSocket as WS
import           JavaScript.Web.MessageEvent (MessageEvent)
import qualified JavaScript.Web.MessageEvent as ME
import           JavaScript.Web.CloseEvent (CloseEvent)
import qualified JavaScript.Web.CloseEvent as CE

import           JavaScript.TypedArray.ArrayBuffer (ArrayBuffer)

main :: IO ()
main = test1 >> test2 >> test3

-- text messages, close from client
test1 :: IO ()
test1 = do
  putStrLn "test1"
  ws <- connectWS True
  WS.send "-5" ws
  threadDelay 1000000
  WS.send "5" ws
  threadDelay 1000000
  WS.close Nothing Nothing ws
  threadDelay 1000000

test2 :: IO ()
test2 = do
  putStrLn "test2"
  ws  <- connectWS True
  buf <- js_makeArrayBuffer
  WS.sendArrayBuffer buf ws
  WS.send "0" ws
  threadDelay 1000000

test3 :: IO ()
test3 = do
  putStrLn "test3"
  (connectWS False >>= WS.close Nothing Nothing) `catch` \(e::SomeException) -> print e
  threadDelay 1000000

connectWS :: Bool -> IO WebSocket
connectWS valid = do
  host <- getHost =<< getWindowLocation
  WS.connect $ WebSocketRequest
    { url       = "ws://" <> (if valid then host else "nonexistenthost:62312") <> "/ws"
    , protocols = []
    , onClose   = Just showClose
    , onMessage = Just showMessage
    }

showMessage :: MessageEvent -> IO ()
showMessage msg = putStrLn ("received message: " ++ str)
  where
    str = case ME.getData msg of
      ME.StringData s      -> "string: " ++ JSS.unpack s
      ME.BlobData _        -> "Blob"
      ME.ArrayBufferData _ -> "ArrayBuffer"

showClose :: CloseEvent -> IO ()
showClose ce = putStrLn $
  "connection closed, status: " ++ show (CE.getCode ce) ++
  " clean: " ++ show (CE.wasClean ce) ++
  " reason: " ++ show (JSS.unpack $ CE.getReason ce)

{-
printWebSocketInfo :: WebSocket -> IO ()
printWebSocketInfo ws = do
  let pi n xs = putStrLn (n ++ " " ++ JSS.unpack xs)
  pi "protocol"   =<< WS.getProtocol   ws
  pi "extensions" =<< WS.getExtensions ws
  putStrLn . ("buffered amount "++) . show =<< WS.getBufferedAmount ws
  pi "url" (WS.getUrl ws)
  putStrLn . ("readystate "++) . show =<< WS.getReadyState ws
  putStrLn . ("binary type "++) . show =<< WS.getBinaryType ws
-}
-- fixme use JavaScript.TypedArray API when it's done
foreign import javascript unsafe
  "var x = new Int8Array(2); x[0] = 1; x[1] = 2; $r = x.buffer;"
  js_makeArrayBuffer :: IO ArrayBuffer
