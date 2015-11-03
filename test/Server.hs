{-# LANGUAGE OverloadedStrings #-}

{-
  A simple HTTP server that serves static source and data files
  for tests, and additionally supports some dynamic responses:
 -}
module Server (startServer) where

import           Control.Concurrent
import qualified Control.Exception as E
import           Control.Monad

import qualified Network.HTTP.Types as HTTP

import qualified Network.Wai
import qualified Network.Wai as W
import qualified Network.Wai.Application.Static as Static
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WebSockets as WaiWS
import qualified Network.Wai.Parse as NWP

import           Network.Socket
import qualified Network.WebSockets as WS

import           Data.Maybe

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy.Encoding as TLE

import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import           Data.Int

import           Text.Read

import           Prelude hiding (FilePath)
import           Filesystem.Path

{-
   Start the test server with static file root path on the next
   available port number. Returns the port number
 -}
startServer :: FilePath -> IO Int
startServer path = do
  s <- socket AF_INET Stream defaultProtocol
  bind s (SockAddrInet aNY_PORT iNADDR_ANY)
  listen s 4
  forkIO $
    Warp.runSettingsSocket Warp.defaultSettings s
    (WaiWS.websocketsOr WS.defaultConnectionOptions
                        handleWebSocket
                        (handleReq path))
  fromIntegral <$> socketPort s


handleReq :: FilePath -> Network.Wai.Application
handleReq path req resp
  | ["empty.html"]   <- pi  = handleEmpty            req resp
--  | ("runmain.js":_) <- rpi = handleRunMain  runMain req resp
  | ("pong":_)       <- pi  = handlePong             req resp
  | ("status":_)     <- pi  = handleStatus           req resp
  | ("close":_)      <- pi  = handleClose            req resp
  | ("truncate":_)   <- pi  = handleTruncate         req resp
  | ("stream":_)     <- pi  = handleStream           req resp
  | otherwise               = handleStatic   path    req resp
  where
    rpi = reverse pi
    pi  = Network.Wai.pathInfo req

handleStatic :: FilePath -> Network.Wai.Application
handleStatic path = Static.staticApp (Static.defaultFileServerSettings path)

handleEmpty :: Network.Wai.Application
handleEmpty req resp =
  let d = "<html><head></head><body></body></html>"
      l = BL.toStrict . B.toLazyByteString . B.int64Dec . BL.length $ d
  in  resp $ W.responseLBS HTTP.status200
                           [ ("Content-Type"  , "text/html")
                           , ("Content-Length", l)
                           ]
                           d

-- /**/runmain.js
-- serve a customized webdriver runner
{-
handleRunMain :: BL.ByteString -> Network.Wai.Application
handleRunMain runMain req resp =
  resp $ W.responseLBS HTTP.status200
                       [ ("Content-Type",   "application/javascript")
                       , ("Content-Length", BL.toStrict . B.toLazyByteString .
                                            B.int64Dec  . BL.length $ runMain)
                       ]
                       runMain
-}

-- /pong: respond with same data as GET or POST data argument
handlePong :: Network.Wai.Application
handlePong req resp
  | W.requestMethod req == HTTP.methodGet  = f (queryString' req)
  | W.requestMethod req == HTTP.methodPost =
      NWP.parseRequestBody NWP.lbsBackEnd req >>= f . fst
  | otherwise = invalidMethod resp
  where
    f q = let d = maybe "pong" BL.fromStrict (lookup "data" q)
          in  respondWith resp HTTP.status200 q (BL.length d) d

-- /status/CODE: respond with status code, reply with POST body
--               or data argument, default body if none
handleStatus :: Network.Wai.Application
handleStatus req resp
  | W.requestMethod req == HTTP.methodGet  = f (queryString' req)
  | W.requestMethod req == HTTP.methodPost =
      NWP.parseRequestBody NWP.lbsBackEnd req >>= f . fst
  | otherwise = invalidMethod resp
  where
    s | (_:code:_) <- Network.Wai.pathInfo req
      , Just c0 <- readMaybeT code = HTTP.mkStatus c0 "Status"
      | otherwise = HTTP.status200
    f q = let d = maybe "pong" BL.fromStrict (lookup "data" q)
          in  respondWith resp s q (BL.length d) d

-- /close/DELAY: close the connection without a response after DELAY ms
handleClose :: Network.Wai.Application
handleClose req respond
  | (_:delay:_) <- W.pathInfo req
  , Just ms <- readMaybeT delay = f ms
  | otherwise = f 0
  where
    f d = do
      threadDelay (d*1000)
      -- fixme check that this closes the connection
      respond $ W.responseLBS (error "no status") [] ""

-- /truncate/BYTES: claims to reply with 2*BYTES response, but
--                  closes the connection after sending BYTES bytes
--                  default value: 32kiB with 64kiB content length
handleTruncate :: Network.Wai.Application
handleTruncate req resp
  | W.requestMethod req == HTTP.methodGet  = f (queryString' req)
  | W.requestMethod req == HTTP.methodPost =
      NWP.parseRequestBody NWP.lbsBackEnd req >>= f . fst
  | otherwise = invalidMethod resp
  where
    l | (_:bytes:_) <- W.pathInfo req
      , Just c0 <- readMaybeT bytes = c0
      | otherwise = 32768
    d = "abcdefghijklmnopqrstuvwxyz1234567890"
    f q = respondWith resp HTTP.status200 q (2*l) (BL.take l $ BL.cycle d)

-- /stream/CHUNKSIZE: sends an infinite stream of chunks of size
--                    CHUNKSIZE, use delay (ms) for the delay
--                    between chunks
handleStream :: Network.Wai.Application
handleStream req resp
  | W.requestMethod req == HTTP.methodGet  = f (queryString' req)
  | W.requestMethod req == HTTP.methodPost =
      NWP.parseRequestBody NWP.lbsBackEnd req >>= f . fst
  | otherwise = invalidMethod resp
  where
    chunkSize | (_:cs0:_) <- W.pathInfo req
              , Just cs <- readMaybeT cs0 = cs
              | otherwise = 32678
    d = B.lazyByteString $
        BL.take chunkSize (BL.cycle "abcdefghijklmnopqrstuvwxyz1234567890")
    hdrs = [("Content-Type", "text/plain")]
    f q = do
      let delay = maybe (return ()) threadDelay
                  (readMaybeB =<< lookup "delay" q)
      resp $ W.responseStream HTTP.status200 hdrs $ \write flush ->
        forever (write d >> flush >> delay)

handleWebSocket :: WS.ServerApp
handleWebSocket pending = do
  putStrLn "accepting WebSocket request"
  conn <- WS.acceptRequest pending
  let handleMessages = forever $ do
        d <- WS.receiveDataMessage conn
        case d of
          WS.Text t    -> do
            putStrLn "received text message"
            case reads . TL.unpack . TLE.decodeUtf8 $ t of
             [(i, [])] -> case i of
                            0         -> do
                                 putStrLn "closing connection"
                                 WS.sendClose conn (""::T.Text)
                            _ | i < 0 -> replicateM_ (negate i) $
                                 WS.sendDataMessage conn (WS.Text "TestTextMessage")
                            _         -> replicateM_ i $
                                 WS.sendDataMessage conn (WS.Binary "TestBinaryMessage")
             _         -> putStrLn "received non-numeric message"
          WS.Binary bs ->
            putStrLn "received binary message"
      handleConnectionException :: WS.ConnectionException -> IO ()
      handleConnectionException = print
  handleMessages `E.catch` handleConnectionException

----

respondWith :: (W.Response -> IO W.ResponseReceived) -> HTTP.Status -> [(ByteString, ByteString)] -> Int64 -> BL.ByteString -> IO W.ResponseReceived
respondWith respond status query contentLength content = do
  maybe (return ()) threadDelay (readMaybeB =<< lookup "delay" query)
  let ct = fromMaybe "text/plain" (lookup "content-type" query)
      hdrs = [ ("Content-Type",   ct)
             , ("Content-Length", BL.toStrict . B.toLazyByteString . B.int64Dec $ contentLength)
             ]
  respond (W.responseLBS status hdrs content)

queryString' :: W.Request -> [(ByteString, ByteString)]
queryString' = mapMaybe sequence . W.queryString

readMaybeB :: Read a => ByteString -> Maybe a
readMaybeB = readMaybeT . TE.decodeUtf8With TE.lenientDecode

readMaybeT :: Read a => Text -> Maybe a
readMaybeT = readMaybe . T.unpack

invalidMethod :: (W.Response -> IO W.ResponseReceived) -> IO W.ResponseReceived
invalidMethod respond = respond $
  W.responseLBS HTTP.methodNotAllowed405 [] "Method not allowed"

