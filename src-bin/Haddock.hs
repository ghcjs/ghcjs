{-# Language PackageImports #-}

{-
  Haddock for GHCJS

  Most of this file has been copied from the Haddock package
 -}

-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Simon Marlow 2003-2006,
--                    David Waern  2006-2010
-- License     :  BSD-like
--
-- Maintainer  :  haddock@projects.haskell.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Haddock - A Haskell Documentation Tool
--
-- Program entry point and top-level code.
-----------------------------------------------------------------------------

module Main where

import "haddock-internal" Haddock hiding
          (haddock, withGhc, withGhc', getGhcDirs)
import Haddock.Types
import Haddock.InterfaceFile
import Haddock.Options
import Haddock.Utils

import Control.Monad hiding (forM_)
import Data.Foldable (forM_)
import System.IO
import System.Exit
import System.Directory

import qualified Paths_haddock_internal

import GHC hiding (verbosity)
import DynFlags hiding (verbosity)
import StaticFlags (discardStaticFlags)

import Data.Monoid (mempty)
import qualified Compiler.Info          as Info
import qualified Compiler.GhcjsPlatform as Ghcjs
import qualified Compiler.GhcjsProgram  as Ghcjs
import qualified Compiler.Settings      as Ghcjs

main :: IO ()
main = do
  args <- Info.getFullArguments -- adds wrapper arguments for Windows
  haddock args

-------------------------------------------------------------------------------
-- * Top level
-------------------------------------------------------------------------------


-- | Run Haddock with given list of arguments.
--
-- Haddock's own main function is defined in terms of this:
--
-- > main = getArgs >>= haddock
haddock :: [String] -> IO ()
haddock args = handleTopExceptions $ do

  -- Parse command-line flags and handle some of them initially.
  -- TODO: unify all of this (and some of what's in the 'render' function),
  -- into one function that returns a record with a field for each option,
  -- or which exits with an error or help message.
  (flags, files) <- parseHaddockOpts args
  shortcutFlags flags
  qual <- case qualification flags of {Left msg -> throwE msg; Right q -> return q}

  -- inject dynamic-too into flags before we proceed
  flags' <- withGhc' flags $ do
        df <- getDynFlags
        case lookup "GHC Dynamic" (compilerInfo df) of
          Just "YES" -> return $ Flag_OptGhc "-dynamic-too" : flags
          _ -> return flags

  unless (Flag_NoWarnings `elem` flags) $ do
    forM_ (warnings args) $ \warning -> do
      hPutStrLn stderr warning

  withGhc' flags' $ do

    dflags <- getDynFlags

    if not (null files) then do
      (packages, ifaces, homeLinks) <- readPackagesAndProcessModules flags files

      -- Dump an "interface file" (.haddock file), if requested.
      forM_ (optDumpInterfaceFile flags) $ \path -> liftIO $ do
        writeInterfaceFile path InterfaceFile {
            ifInstalledIfaces = map toInstalledIface ifaces
          , ifLinkEnv         = homeLinks
          }

      -- Render the interfaces.
      liftIO $ renderStep dflags flags qual packages ifaces

    else do
      when (any (`elem` [Flag_Html, Flag_Hoogle, Flag_LaTeX]) flags) $
        throwE "No input file(s)."

      -- Get packages supplied with --read-interface.
      packages <- liftIO $ readInterfaceFiles freshNameCache (readIfaceArgs flags)

      -- Render even though there are no input files (usually contents/index).
      liftIO $ renderStep dflags flags qual packages []

withGhc' :: [Flag] -> Ghc a -> IO a
withGhc' flags action = do
  libDir <- fmap snd (getGhcDirs flags)

  -- Catches all GHC source errors, then prints and re-throws them.
  let handleSrcErrors action' = flip handleSourceError action' $ \err -> do
        printException err
        liftIO exitFailure

  withGhc libDir (ghcFlags flags) (\_ -> handleSrcErrors action)

-------------------------------------------------------------------------------
-- * Creating a GHC session
-------------------------------------------------------------------------------

-- | Start a GHC session with the -haddock flag set. Also turn off
-- compilation and linking. Then run the given 'Ghc' action.
withGhc :: String -> [String] -> (DynFlags -> Ghc a) -> IO a
withGhc libDir flags ghcActs = runGhc (Just libDir) $ do
  dynflags  <- getSessionDynFlags
  dynflags' <- parseGhcFlags (gopt_set dynflags Opt_Haddock) {
    hscTarget = HscNothing,
    ghcMode   = CompManager,
    ghcLink   = NoLink
    }
  env <- liftIO Ghcjs.newGhcjsEnv
  let dynflags'' = Ghcjs.setGhcjsPlatform mempty env [] libDir $
                   updateWays $ addWay' (WayCustom "js") $
                   Ghcjs.setGhcjsSuffixes False $
                   gopt_unset dynflags' Opt_SplitObjs
  defaultCleanupHandler dynflags'' $ do
      -- ignore the following return-value, which is a list of packages
      -- that may need to be re-linked: Haddock doesn't do any
      -- dynamic or static linking at all!
      _ <- Ghcjs.setSessionDynFlags dynflags''
      ghcActs dynflags''
  where
    parseGhcFlags :: MonadIO m => DynFlags -> m DynFlags
    parseGhcFlags dynflags = do
      -- TODO: handle warnings?

      -- NOTA BENE: We _MUST_ discard any static flags here, because we cannot
      -- rely on Haddock to parse them, as it only parses the DynFlags. Yet if
      -- we pass any, Haddock will fail. Since StaticFlags are global to the
      -- GHC invocation, there's also no way to reparse/save them to set them
      -- again properly.
      --
      -- This is a bit of a hack until we get rid of the rest of the remaining
      -- StaticFlags. See GHC issue #8276.
      let flags' = discardStaticFlags flags
      (dynflags', rest, _) <- parseDynamicFlags dynflags (map noLoc $ flags')
      if not (null rest)
        then throwE ("Couldn't parse GHC options: " ++ unwords flags')
        else return dynflags'

-------------------------------------------------------------------------------
-- * Misc
-------------------------------------------------------------------------------

getHaddockLibDir :: [Flag] -> IO String
getHaddockLibDir flags =
  case [str | Flag_Lib str <- flags] of
    [] -> do
      d <- Paths_haddock_internal.getDataDir -- provided by Cabal
      doesDirectoryExist d >>= \exists -> case exists of
        True -> return d
        False -> do
          -- If directory does not exist then we are probably invoking from
          -- ./dist/build/haddock/haddock so we use ./resources as a fallback.
          doesDirectoryExist "resources" >>= \exists_ -> case exists_ of
            True -> return "resources"
            False -> die ("Haddock's resource directory (" ++ d ++ ") does not exist!\n")
    fs -> return (last fs)


getGhcDirs :: [Flag] -> IO (String, String)
getGhcDirs flags =
  case [ dir | Flag_GhcLibDir dir <- flags ] of
    [] -> error "haddock-ghcjs: missing -B option, cannot find library dir"
    xs -> return ("not available", last xs)

