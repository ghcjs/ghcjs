{-# LANGUAGE CPP #-}
module Compiler.Info where

import           Data.Version     as Version

import           System.Directory (getAppUserDataDirectory)
import           System.Info

import           Data.Function    (on)
import           Data.List        (nubBy)
import           System.FilePath  ((</>))

import           Config           (cProjectVersion)
import           DynFlags         (compilerInfo)
#ifndef GHCJS_INTEGRATED
import           GHC
import qualified GHC.Paths
import           Paths_ghcjs
#endif

#ifndef GHCJS_INTEGRATED
getCompilerInfo = do
      glbDb <- getGlobalPackageDB
      df <- runGhc (Just GHC.Paths.libdir) getSessionDynFlags
      return . nubBy ((==) `on` fst) $
           [ ("Project name", "The Glorious Glasgow Haskell Compilation System for Javascript")
           , ("Global Package DB", glbDb)
           , ("Project version", getCompilerVersion)
           ] ++ compilerInfo df
#endif

getGlobalPackageBase = do
  appdir <- getAppUserDataDirectory "ghcjs"
  return (appdir </> subdir)
      where
        targetARCH = arch
        targetOS   = os
        subdir     = targetARCH ++ '-':targetOS ++ '-':getCompilerVersion

getGlobalPackageDB = fmap (</> "package.conf.d") getGlobalPackageInst

getUserPackageDB = fmap (</> "package.conf.d") getGlobalPackageBase

getGlobalPackageInst = fmap (</> "lib") getGlobalPackageBase

getGlobalCache = fmap (</> "cache") getGlobalPackageBase

-- Just the GHC version
getGhcCompilerVersion = cProjectVersion

#ifndef GHCJS_INTEGRATED
-- Just the GHCJS version
getGhcjsCompilerVersion = Version.showVersion version

-- ghcversion-ghcjsversion
getCompilerVersion = cProjectVersion ++ "-" ++ Version.showVersion version

getCompilerSubdir = "ghcjs-" ++ getCompilerVersion
#else

getCompilerVersion = cProjectVersion ++ "-0"

#endif

