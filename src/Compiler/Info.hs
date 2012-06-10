module Compiler.Info where

import Data.Version as Version
import Paths_ghcjs

import System.Directory (getAppUserDataDirectory)
import System.Info

import System.FilePath ((</>))
import Data.Function (on)
import Data.List (nubBy)

import qualified GHC.Paths
import GHC
import Config (cProjectVersion)
import DynFlags (compilerInfo)

getCompilerInfo = do
      glbDb <- getGlobalPackageDB
      df <- runGhc (Just GHC.Paths.libdir) getSessionDynFlags
      return . nubBy ((==) `on` fst) $
           [ ("Project name", "The Glorious Glasgow Haskell Compilation System for Javascript")
           , ("Global Package DB", glbDb)
           , ("Project version", getCompilerVersion)
           ] ++ compilerInfo df

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

-- Just the GHCJS version
getGhcjsCompilerVersion = Version.showVersion version

-- ghcversion-ghcjsversion
getCompilerVersion = cProjectVersion ++ "-" ++ Version.showVersion version

getCompilerSubdir = "ghcjs-" ++ getCompilerVersion

