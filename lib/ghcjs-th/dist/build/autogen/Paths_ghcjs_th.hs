{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_ghcjs_th (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/luite/.cabal/bin"
libdir     = "/home/luite/.cabal/lib/x86_64-linux-ghc-8.0.1/ghcjs-th-0.1.0.0-KIKWHvxLD16DJtY0EdJtfC"
datadir    = "/home/luite/.cabal/share/x86_64-linux-ghc-8.0.1/ghcjs-th-0.1.0.0"
libexecdir = "/home/luite/.cabal/libexec"
sysconfdir = "/home/luite/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "ghcjs_th_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "ghcjs_th_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "ghcjs_th_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "ghcjs_th_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "ghcjs_th_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
