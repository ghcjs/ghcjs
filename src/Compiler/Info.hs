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

-- fixme, cabal cannot determine the version if it contains text
getCompilerVersion = cProjectVersion ++ "." {-++ "ghcjs" -} ++ Version.showVersion version

