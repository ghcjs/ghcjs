{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction, ExtendedDefaultRules #-}
{-
   Boot program to build the GHCJS core libraries from a configured GHC source tree

   steps:
   1. download ghc sources and extract to temp directory (only if autobooting)
   2. make stage1 compiler and tools (only if autobooting)
   3. generate inplace/bin/ghcjs and inplace/bin/ghcjs-pkg wrappers
   4. build libraries with ghcjs
   5. install libraries with ghc-cabal
   6. copy .js files for libraries to installation locations
-}

module Main where

import Prelude hiding (FilePath)
import qualified Data.Text.Lazy as T
import Data.Text.Lazy (Text)
import Shelly
import Data.Monoid (mappend)
import Control.Monad (forM, forM_)
import Compiler.Info
import qualified Data.ByteString.Lazy as L
import Filesystem.Path.CurrentOS (stripPrefix, empty)
import Data.Maybe (catMaybes)

import System.Directory
import System.Environment
import Distribution.Simple.Utils (withTempDirectory)
import Distribution.Verbosity (normal)
import Network (withSocketsDo)

import qualified Codec.Archive.Tar as Tar
import qualified Codec.Archive.Tar.Check as Tar

import Data.Conduit (($$), ($=), (=$=))
import Data.Conduit.Lazy (lazyConsume)
import Data.Conduit.BZlib
import Network.HTTP.Conduit

default (T.Text)

(<>) = mappend

ghcDownloadLocation :: String -> String
ghcDownloadLocation ver =
--    "http://server/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-src.tar.bz2"
    "http://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-src.tar.bz2"

main = withSocketsDo $ do
  args <- getArgs
  tmp <- getTemporaryDirectory
  withTempDirectory normal tmp "ghcjs-boot" $ \tmpDir -> shelly $ do
    when (any (=="--auto") args) (autoBoot $ fromString tmpDir)
    installBootPackages

fromString :: String -> FilePath
fromString = fromText . T.pack

toString :: FilePath -> String
toString = T.unpack . toTextIgnore

-- autoboots roll out
-- download and configure a fresh ghc source tree
autoBoot :: FilePath -> ShIO ()
autoBoot tmp = shelly $ do
  ghcVer <- T.unpack . T.strip <$> run "ghc" ["--numeric-version"]
  let ghcDir = "ghc-" ++ ghcVer
  cd tmp
  liftIO $ do
    putStrLn $ "fetching GHC: " ++ ghcDownloadLocation ghcVer
    request <- parseUrl (ghcDownloadLocation ghcVer)
    withManager $ \manager -> do
                          Response _ _ _ src <- http request manager
                          tar <- lazyConsume (src =$= bunzip2)
                          liftIO $ putStrLn "unpacking tar"
                          liftIO $ Tar.unpack (toString tmp)  (Tar.read $ L.fromChunks tar) -- (Tar.checkTarbomb ("ghc-" ++ ghcVer) $ Tar.read tar)
  cd (tmp </> T.pack ghcDir)
  run_ "sh" ["configure"]
  run_ "make" ["stage1"]

-- to be called from a configured and built (at least stage 1) ghc tree
installBootPackages :: ShIO ()
installBootPackages = do
  mghcjs <- which "ghcjs"
  mghcjspkg <- which "ghcjs-pkg"
  case (mghcjs, mghcjspkg) of
    (Just ghcjs, Just ghcjspkg) -> do
      echo $ "Booting: " <> toTextIgnore ghcjs <> " (" <> toTextIgnore ghcjspkg <> ")"
      initPackageDB
      addWrappers ghcjs ghcjspkg
      run_ "make" ["all_libraries", "GHC_STAGE1=inplace/bin/ghcjs", "GHC_PKG_PGM=ghcjs-pkg"] `sh_catchany` (const (return ()))
      installRts
      mapM_ (installPkg ghcjs ghcjspkg) ["ghc-prim", "integer-gmp", "base"]
    _ -> echo "Error: ghcjs and ghcjs-pkg must be in the PATH"

-- add inplace/bin/ghcjs and inplace/bin/ghcjs-pkg wrappers
addWrappers :: FilePath -> FilePath -> ShIO ()
addWrappers ghcjs ghcjspkg = do
  ghcwrapper <- readfile "inplace/bin/ghc-stage1"
  writefile "inplace/bin/ghcjs" (fixGhcWrapper ghcwrapper ghcjs)
  pkgwrapper <- readfile "inplace/bin/ghc-pkg"
  writefile "inplace/bin/ghcjs-pkg" (fixPkgWrapper pkgwrapper ghcjspkg)

fixGhcWrapper :: Text -> FilePath -> Text
fixGhcWrapper wrapper ghcjs = T.unlines . map fixLine . T.lines $ wrapper
    where
      fixLine line
          | "executablename=" `T.isPrefixOf` line = "executablename=\"" <> toTextIgnore ghcjs <> "\""
          | otherwise                             = line

fixPkgWrapper :: Text -> FilePath -> Text
fixPkgWrapper wrapper ghcjspkg = T.unlines . map fixLine . T.lines $ wrapper
    where
      fixLine line
          | "/" `T.isPrefixOf` line = T.unwords $ toTextIgnore ghcjspkg : tail (T.words line)
          | otherwise = line

installRts :: ShIO ()
installRts = do
  echo "installing RTS"
  dest <- liftIO getGlobalPackageDB
  base  <- liftIO getGlobalPackageBase
  let inc = base </> "include"
      lib = base </> "lib"
  rtsConf <- readfile "rts/package.conf.install"
  writefile (dest </> "builtin_rts.conf") $
                 fixRtsConf (toTextIgnore inc) (toTextIgnore lib) rtsConf
  run_ "ghcjs-pkg" ["recache", "--global"]
  mkdir_p inc
  sub $ cd "includes" >> cp_r "." inc
  mkdir_p lib
  sub $ cd "rts/dist/build" >> cp_r "." lib
  cp "settings" (base </> "settings")

fixRtsConf :: Text -> Text -> Text -> Text
fixRtsConf incl lib conf = T.unlines . map fixLine . T.lines $ conf
    where
      fixLine l
          | "library-dirs:" `T.isPrefixOf` l = "library-dirs: " <> lib
          | "include-dirs:" `T.isPrefixOf` l = "include-dirs: " <> incl
          | otherwise                        = l

rtsConf :: Text -> Text -> Text
rtsConf incl lib = T.unlines
            [ "name:           rts"
            , "version:        1.0"
            , "id:             builtin_rts"
            , "license:        BSD3"
            , "maintainer:     stegeman@gmail.com"
            , "exposed:        True"
            , "include-dirs:   " <> incl
            , "includes:       Stg.h"
            , "library-dirs:   " <> lib
            , "hs-libraries:   HSrts"
            ]

initPackageDB :: ShIO ()
initPackageDB = do
  base <- liftIO getGlobalPackageBase
  inst <- liftIO getGlobalPackageInst
  mkdir_p (fromString base)
  mkdir_p (fromString inst)
  run_ "ghcjs-pkg" ["initglobal"] `catchany_sh` const (return ())
  run_ "ghcjs-pkg" ["inituser"] `catchany_sh` const (return ())


installPkg :: FilePath -> FilePath -> Text -> ShIO ()
installPkg ghcjs ghcjspkg pkg = do
  echo $ "installing package: " <> pkg
  base <- liftIO getGlobalPackageBase
  dest <- liftIO getGlobalPackageInst
  db   <- liftIO getGlobalPackageDB      
  run_ "inplace/bin/ghc-cabal" [ "install"
                               , toTextIgnore ghcjs
                               , toTextIgnore ghcjspkg
                               , ":"
                               , T.pack dest
                               , "libraries/" <> pkg
                               , "dist-install"
                               , ""
                               , T.pack base
                               , T.pack dest
                               , T.pack base <> "/doc"
                               , "NO"
                               ]
  -- now install the javascript files
  dirs <- sub $ cd (fromString dest) >> lsRel
  case filter (\x -> (pkg <> "-") `T.isPrefixOf` toTextIgnore x) dirs of
    (d:_) -> do
      echo $ "found installed version: " <> toTextIgnore d
      sub $ do
        cd ("libraries" </> pkg </> "dist-install" </> "build")
        files <- findRel
        forM_ (filter isJsFile files) $ \file -> cp file (dest </> d </> file)
    _ -> errorExit $ "could not find installed package " <> pkg

isPathPrefix :: Text -> FilePath -> Bool
isPathPrefix t file = t `T.isPrefixOf` toTextIgnore file 

isJsFile :: FilePath -> Bool
isJsFile file = ".js" `T.isSuffixOf` toTextIgnore file

-- relative filepaths in the current directory
lsRel :: ShIO [FilePath]
lsRel = mkRels $ ls "."

-- find files relative to the current dir
findRel :: ShIO [FilePath]
findRel = mkRels $ find "."

mkRels :: ShIO [FilePath] -> ShIO [FilePath]
mkRels a = do
  files <- a
  d <- pwd
  return . catMaybes . map (stripPrefix (d </> empty)) $ files