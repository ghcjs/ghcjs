{-# LANGUAGE DeriveGeneric, OverloadedStrings, LambdaCase, ScopedTypeVariables, TupleSections #-}

module Client ( startSessions
              , runTest
              , closeSession
              , Server(..)
              , Session
              , sessionName
              ) where

import           Control.Concurrent (forkIO)

import           Control.Concurrent.Lifted
import           Control.Concurrent.Chan.Lifted
import           Control.Concurrent.MVar.Lifted
import           Control.Exception.Lifted

import           Control.Monad.IO.Class

import           Data.Maybe
import           Data.Monoid
import           Data.Time.Clock

import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Class as WD

import           Data.Text (Text)
import qualified Data.Text as T

import           Types

data Server = Server { serverPort :: Int
                     , wdScript   :: Text
                     }

data Session = Session { sessionName  :: String
                       , sessionQueue :: (Chan (Maybe ( FilePath
                                                      , FilePath
                                                      , [String]
                                                      , MVar (Maybe (StdioResult,Integer))
                                                      )))

                       }

wdConfig :: Text -> Int -> WD.Browser -> WD.WDConfig
wdConfig host port browser =
  WD.defaultConfig { WD.wdHost         = T.unpack host
                   , WD.wdPort         = port
                   , WD.wdCapabilities = caps
                   }
  where
    caps = WD.defaultCaps { WD.javascriptEnabled = Just True
                          , WD.browser           = browser
                          }

startSessions :: Server -> Text -> Int -> IO [Session]
startSessions server host port = do
  sessions <- mapM (startSession server host port)
                   [ ("Firefox",           WD.firefox)
                   , ("Chrome",            WD.chrome)
                   , ("Internet Explorer", WD.ie)
                   , ("Opera",             WD.opera)
                   , ("Safari",            WD.Browser "safari")
                   ]
  return (catMaybes sessions)

startSession :: Server -> Text -> Int -> (String, WD.Browser) -> IO (Maybe Session)
startSession server host port (bname, browser) = do
  mv <- newEmptyMVar
  _ <- forkIO (sess mv `catch` \(_::SomeException) -> putMVar mv Nothing)
  takeMVar mv
  where
    -- cfg = wdConfig host port browser
    sess mv = WD.runSession (wdConfig host port browser) $
      WD.finallyClose $ do
        WD.setScriptTimeout 300000
        WD.setPageLoadTimeout 300000
        WD.openPage (serverUrl server "empty.html")
        ch <- liftIO  newChan
        let s = Session bname ch
        liftIO $ putMVar mv (Just s)
        runSessionChan server s

closeSession :: Session -> IO ()
closeSession s = writeChan (sessionQueue s) Nothing

runTest :: FilePath -> FilePath -> [String] -> Session -> IO (Maybe (StdioResult, Integer))
runTest dir page args sess = do
  res <- newEmptyMVar
  writeChan (sessionQueue sess) (Just (dir, page, args, res))
  readMVar res

serverUrl :: Server -> String -> String
serverUrl s e = "http://127.0.0.1:" <> show (serverPort s) <> "/" <> e

runSessionChan :: forall wd. (MonadIO wd, WD.WebDriver wd)
               => Server
               -> Session
               -> wd ()
runSessionChan server sess =
  liftIO (readChan $ sessionQueue sess) >>= \x -> case x of
    Just (dir, page, args, res) -> (processCommand dir page args res `catch` handler)
                        `finally`
                        (liftIO (tryPutMVar res Nothing) >> runSessionChan server sess)
    Nothing -> liftIO (putStrLn $ "closing session " ++ sessionName sess)
  where
    handler :: SomeException -> wd ()
    handler e =  liftIO . putStrLn $ "exception running test in " ++
                                     sessionName sess ++ "\n" ++ show e
    processCommand dir page args res = do
      t0 <- liftIO getCurrentTime
      WD.openPage (serverUrl server (dir <> "/" <> page))
      r <- WD.asyncJS [WD.JSArg args, WD.JSArg (serverUrl server dir)] (wdScript server)
      t1 <- liftIO getCurrentTime
      liftIO (putMVar res (fmap (,round (1000 * diffUTCTime t1 t0)) r))
      WD.openPage (serverUrl server "empty.html")
