{-# LANGUAGE CPP, ScopedTypeVariables, OverloadedStrings #-}
module Compiler.Info where

import qualified Control.Exception as E

import           Data.Function      (on)
import           Data.List          (nubBy)
import qualified Data.Version as Version

import           System.Directory   (getAppUserDataDirectory)
import           System.FilePath    ((</>))
import           System.Info
import Prelude

import           Config             (cProjectVersion)
import           DynFlags

import qualified Paths_ghcjs

compilerInfo :: Bool
             -> DynFlags
             -> [(String, String)]
compilerInfo nativeToo dflags = do
      let topDir = getTopDir dflags
      nubBy ((==) `on` fst) $
           [ ("Project name"
           , "The Glorious Glasgow Haskell Compilation System for JavaScript")
           , ("Global Package DB", getGlobalPackageDB topDir)
           , ("Project version"  , getCompilerVersion)
           , ("LibDir"           , topDir)
           ] ++ DynFlags.compilerInfo dflags

-- | the directory to use if started without -B flag
{-
getDefaultTopDir :: IO FilePath
getDefaultTopDir = do
  appdir <- getAppUserDataDirectory "ghcjs"
  return (appdir </> subdir </> "ghcjs")
      where
        targetARCH = arch
        targetOS   = os
        subdir     = targetARCH ++ '-':targetOS ++ '-':getFullCompilerVersion

getDefaultLibDir :: IO FilePath
getDefaultLibDir = getDefaultTopDir

mkTopDir :: Maybe String -> IO FilePath
mkTopDir (Just x) = return x
mkTopDir _        = getDefaultTopDir

mkLibDir :: Maybe String -> IO FilePath
mkLibDir (Just x) = return x
mkLibDir _        = getDefaultLibDir
-}
getTopDir :: DynFlags -> FilePath
getTopDir = sTopDir . settings

-- | get the library directory (ghcjs --print-libdir).
getLibDir :: DynFlags -> FilePath
getLibDir = sTopDir . settings

{- | get the library directory from the unsafe global DynFlags
     throws an exception if called before a Ghc session has been started
 -}
unsafeGetLibDir :: FilePath
unsafeGetLibDir = getLibDir unsafeGlobalDynFlags

-- | find location of the global package database
getGlobalPackageDB :: FilePath
                   -> FilePath
getGlobalPackageDB libDir = libDir </> "package.conf.d"

getUserTopDir :: IO (Maybe FilePath)
getUserTopDir = fmap Just getUserTopDir' `E.catch`
                   \(E.SomeException _) -> return Nothing

getUserTopDir' :: IO FilePath
getUserTopDir' =  (</> subdir) <$> getAppUserDataDirectory "ghcjs"
  where
    targetARCH = arch
    targetOS   = os
    subdir     = targetARCH ++ '-':targetOS ++ '-':getFullCompilerVersion

-- | find location of the user package database
getUserPackageDir :: IO (Maybe FilePath)
getUserPackageDir = getUserTopDir

getUserPackageDir' :: IO FilePath
getUserPackageDir' = getUserTopDir'

getUserCacheDir :: IO (Maybe FilePath)
getUserCacheDir = fmap (</> "cache") <$> getUserTopDir

-- | Just the GHC version
getGhcCompilerVersion :: String
getGhcCompilerVersion = cProjectVersion

-- | GHCJS-GHC
getFullCompilerVersion :: [Char]
getFullCompilerVersion = Version.showVersion Paths_ghcjs.version ++
                         "-" ++
                         getGhcCompilerVersion

-- | Just the GHCJS version
getCompilerVersion :: String
getCompilerVersion = Version.showVersion Paths_ghcjs.version

-- | version in GHC format, e.g. 7.8.2 -> 708
getShortCompilerVersion :: String
getShortCompilerVersion =
  case Version.versionBranch Paths_ghcjs.version of
    []      -> "0"
    [x]     -> show (100 * x)
    (x:y:_) -> show (100 * x + min 99 y)

getCompilerSubdir :: [Char]
getCompilerSubdir = "ghcjs-" ++ getCompilerVersion

-- | find location for static data installed by ghcjs-boot
getDataDir :: FilePath
           -> FilePath
getDataDir topDir = topDir

-- | default location to get data files when booting: Cabal data directory
ghcjsBootDefaultDataDir :: IO FilePath
ghcjsBootDefaultDataDir = Paths_ghcjs.getDataDir
