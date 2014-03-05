{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Info where

import           Data.Version     as Version
import           Data.Char (toLower)
import           Control.Monad
import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Control.Exception as Ex
import           System.Environment (getEnv)
import           System.Directory (getAppUserDataDirectory)
import           System.Info

import           Data.Function    (on)
import           Data.List        (nubBy)
import           System.FilePath  ((</>))

import           Config           (cProjectVersion)
import           DynFlags         (compilerInfo)
import           GHC
import qualified GHC.Paths
import           Paths_ghcjs

getCompilerInfo = do
      glbDb <- getGlobalPackageDB
      df <- runGhc (Just GHC.Paths.libdir) getSessionDynFlags
      libDir <- getGlobalPackageInst
      return . nubBy ((==) `on` fst) $
           [ ("Project name", "The Glorious Glasgow Haskell Compilation System for JavaScript")
           , ("Global Package DB", glbDb)
           , ("Project version", getCompilerVersion)
           , ("LibDir", libDir)
           ] ++ compilerInfo df

getGlobalPackageBase = do
  appdir <- getAppUserDataDirectory "ghcjs"
  return (appdir </> subdir)
      where
        targetARCH = arch
        targetOS   = os
        subdir     = targetARCH ++ '-':targetOS ++ '-':getFullCompilerVersion

getGlobalPackageDB = fmap (</> "package.conf.d") getGlobalPackageInst

getUserPackageDB = fmap (</> "package.conf.d") getGlobalPackageBase

getGlobalPackageInst = fmap (</> "lib") getGlobalPackageBase

-- Just the GHC version
getGhcCompilerVersion = cProjectVersion

-- GHCJS-GHC
getFullCompilerVersion = Version.showVersion version ++ "-" ++ getGhcCompilerVersion

-- Just the GHCJS version
getCompilerVersion = Version.showVersion version -- ++ "." ++ cProjectVersion

-- version in GHC format
getShortCompilerVersion =
  case Version.versionBranch version of
    [x]     -> show (100 * x)
    (x:y:_) -> show (100 * x + min 99 y)

getCompilerSubdir = "ghcjs-" ++ getCompilerVersion

ghcjsDataDir :: IO FilePath
ghcjsDataDir = getDataDir

