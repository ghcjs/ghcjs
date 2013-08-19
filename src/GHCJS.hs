{-# LANGUAGE ScopedTypeVariables #-}
module GHCJS
    (
      runGhcSession
    , sourceErrorHandler
    , module GHC
    ) where

import DynFlags
import ErrUtils           (fatalErrorMsg'')
import Exception
import GHC
import MonadUtils
import Panic              (handleGhcException)

import Control.Monad
import System.Environment (getArgs)
import System.Exit        (ExitCode (..), exitWith)
import System.IO          (hPutStrLn, stderr)

import Compiler.Info
import Compiler.Variants

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
