{-# LANGUAGE ScopedTypeVariables #-}
module Compiler.Info where

import           Data.Function      (on)
import           Data.List          (nubBy)
import qualified Data.Version as Version

import           System.Directory   (getAppUserDataDirectory)
import           System.FilePath    ((</>))
import           System.Info

import           Config           (cProjectVersion)
import           DynFlags         (compilerInfo)

import           GHC
import qualified GHC.Paths

import           Paths_ghcjs

getCompilerInfo :: IO [([Char], [Char])]
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

getGlobalPackageBase :: IO FilePath
getGlobalPackageBase = do
  appdir <- getAppUserDataDirectory "ghcjs"
  return (appdir </> subdir)
      where
        targetARCH = arch
        targetOS   = os
        subdir     = targetARCH ++ '-':targetOS ++ '-':getFullCompilerVersion

getGlobalPackageDB :: IO FilePath
getGlobalPackageDB = fmap (</> "package.conf.d") getGlobalPackageInst

getUserPackageDB :: IO FilePath
getUserPackageDB = fmap (</> "package.conf.d") getGlobalPackageBase

getGlobalPackageInst :: IO FilePath
getGlobalPackageInst = fmap (</> "lib") getGlobalPackageBase

-- Just the GHC version
getGhcCompilerVersion :: String
getGhcCompilerVersion = cProjectVersion

-- GHCJS-GHC
getFullCompilerVersion :: [Char]
getFullCompilerVersion = Version.showVersion version ++ "-" ++ getGhcCompilerVersion

-- Just the GHCJS version
getCompilerVersion :: String
getCompilerVersion = Version.showVersion version

-- version in GHC format
getShortCompilerVersion :: String
getShortCompilerVersion =
  case Version.versionBranch version of
    []      -> "0"
    [x]     -> show (100 * x)
    (x:y:_) -> show (100 * x + min 99 y)

getCompilerSubdir :: [Char]
getCompilerSubdir = "ghcjs-" ++ getCompilerVersion

ghcjsDataDir :: IO FilePath
ghcjsDataDir = getDataDir

