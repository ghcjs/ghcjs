{-# LANGUAGE ScopedTypeVariables #-}
module GHCJS
    (
      runGhcSession
    , runGhcjsSession
    , setGhcjsSuffixes
    , sourceErrorHandler
    , addPkgConf
    , module GHC
    ) where

import           DsMeta                 (templateHaskellNames)
import           DynFlags
import           ErrUtils               (fatalErrorMsg'')
import           Exception
import           GHC
import           HscTypes
import           IfaceEnv               (initNameCache)
import           MonadUtils
import           Packages
import           Panic                  (handleGhcException)
import           PrelInfo               (wiredInThings)
import           PrelNames              (basicKnownKeyNames)
import           PrimOp                 (allThePrimOps)

import           Control.Monad
import           Data.IORef
import           Data.Maybe             (isJust)
import           System.Environment     (getArgs)
import           System.Exit            (ExitCode (..), exitWith)
import           System.IO              (hPutStrLn, stderr)

import           Compiler.GhcjsPlatform
import           Compiler.Info
import           Compiler.Settings
import           Compiler.Utils

import qualified Gen2.PrimIface         as Gen2

runGhcjsSession :: Maybe FilePath  -- ^ Directory with library files,
                   -- like GHC's -B argument
                -> GhcjsSettings
                -> Ghc b           -- ^ Action to perform
                -> IO b
runGhcjsSession mbMinusB settings m = runGhcSession mbMinusB $ do
    base <- liftIO ghcjsDataDir
    dflags <- getSessionDynFlags
    jsEnv <- liftIO newGhcjsEnv
    _ <- setSessionDynFlags
         $ setGhcjsPlatform settings jsEnv [] base
         $ updateWays $ addWay' (WayCustom "js")
         $ setGhcjsSuffixes False dflags
    fixNameCache
    m

runGhcSession :: Maybe FilePath -> Ghc b -> IO b
runGhcSession mbMinusB a = do
    libDir <- getGlobalPackageBase
    errorHandler
      fatalMessager
      defaultFlushOut $
        runGhc (mbMinusB `mplus` Just libDir) $ a


-- | make sure we don't show panic messages with the "report GHC bug" text, since
--   those are probably our fault.
errorHandler :: (ExceptionMonad m, MonadIO m)
                    => FatalMessager -> FlushOut -> m a -> m a
errorHandler fm (FlushOut flushOut) inner =
  ghandle (\exception -> liftIO $ do
           flushOut
           case fromException exception of
                -- an IO exception probably isn't our fault, so don't panic
                Just (ioe :: IOException) ->
                  fatalErrorMsg'' fm (show ioe)
                _ -> case fromException exception of
                     Just UserInterrupt -> exitWith (ExitFailure 1)
                     Just StackOverflow ->
                         fatalErrorMsg'' fm "stack overflow: use +RTS -K<size> to increase it"
                     _ -> case fromException exception of
                          Just (ex :: ExitCode) -> liftIO $ throwIO ex
                          _ -> case fromException exception of
                               Just (Panic str) -> fatalErrorMsg'' fm str
                               _ -> fatalErrorMsg'' fm (show exception)
           exitWith (ExitFailure 1)
         ) $

  -- error messages propagated as exceptions
  handleGhcException
            (\ge -> liftIO $ do
                flushOut
                case ge of
                     PhaseFailed _ code -> exitWith code
                     Signal _ -> exitWith (ExitFailure 1)
                     _ -> do fatalErrorMsg'' fm (show ge)
                             exitWith (ExitFailure 1)
            ) $
  inner

sourceErrorHandler :: GhcMonad m => m a -> m a
sourceErrorHandler m = handleSourceError (\e -> do
  GHC.printException e
  liftIO $ exitWith (ExitFailure 1)) m

fatalMessager :: String -> IO ()
fatalMessager str = do
  hPutStrLn stderr str
  dumpArgs <- getEnvOpt "GHCJS_ERROR_ARGUMENTS"
  when dumpArgs $ do
    args <- getArgs
    hPutStrLn stderr (str ++ "\n--- arguments: \n" ++ unwords args ++ "\n---\n")
  exitWith (ExitFailure 1)


-- TODO: move the code below somewhere more appropriate

-- | Sets up GHCJS package databases, requires a call to initPackages
addPkgConf :: DynFlags -> IO DynFlags
addPkgConf df = do
  db1 <- getGlobalPackageDB
  db2 <- getUserPackageDB
  let replaceConf GlobalPkgConf = PkgConfFile db1
      replaceConf UserPkgConf   = PkgConfFile db2
      replaceConf x             = x
  return $ df { extraPkgConfs = map replaceConf . extraPkgConfs df }


setGhcjsSuffixes :: Bool     -- oneshot option, -c
                 -> DynFlags
                 -> DynFlags
setGhcjsSuffixes oneshot df = df
    { objectSuf     = mkGhcjsSuf (objectSuf df)
    , dynObjectSuf  = mkGhcjsSuf (dynObjectSuf df)
    , hiSuf         = mkGhcjsSuf (hiSuf df)
    , dynHiSuf      = mkGhcjsSuf (dynHiSuf df)
    , outputFile    = fmap mkGhcjsOutput (outputFile df)
    , dynOutputFile = fmap mkGhcjsOutput (dynOutputFile df)
    , outputHi      = fmap mkGhcjsOutput (outputHi df)
    , ghcLink       = if oneshot then NoLink else ghcLink df
    }


-- replace primops in the name cache so that we get our correctly typed primops
fixNameCache :: GhcMonad m => m ()
fixNameCache = do
  sess <- getSession
  liftIO $ modifyIORef (hsc_NC sess) $ \(NameCache u _) ->
    (initNameCache u knownNames)
    where
      knownNames = map getName (filter (not.isPrimOp) wiredInThings) ++
                      basicKnownKeyNames ++
                      templateHaskellNames ++
                      map (getName . AnId . Gen2.mkGhcjsPrimOpId) allThePrimOps
      isPrimOp (AnId i) = isPrimOpId i
      isPrimOp _        = False
