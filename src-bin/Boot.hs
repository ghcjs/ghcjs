{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

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

import           Compiler.Info
import           Control.Monad             (forM, forM_)
import qualified Data.ByteString.Lazy      as L
import           Data.Maybe                (catMaybes)
import           Data.Monoid
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as T
import           Filesystem.Path.CurrentOS (empty, stripPrefix)
import           Prelude                   hiding (FilePath)
import           Shelly

import           Distribution.Simple.Utils (withTempDirectory)
import           Distribution.Verbosity    (normal)
import           Network                   (withSocketsDo)
import           System.Directory
import           System.Environment

import qualified Codec.Archive.Tar         as Tar
import qualified Codec.Archive.Tar.Check   as Tar

import           Data.Conduit              (unwrapResumable, ($$), ($=), (=$=))
import           Data.Conduit.BZlib
import           Data.Conduit.Lazy         (lazyConsume)
import           Network.HTTP.Conduit
import           System.Directory          (Permissions (..), getPermissions,
                                            setPermissions)

default (T.Text)

ghcDownloadLocation :: String -> String
ghcDownloadLocation ver =
    "http://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-src.tar.bz2"

data BootSettings = BootSettings { skipRts :: Bool
                                 , noBuild :: Bool
                                 } deriving (Ord, Eq)

bootSettings :: [String] -> BootSettings
bootSettings args = BootSettings (any (=="--skip-rts") args) (any  (=="--nobuild") args)

main = withSocketsDo $ do
  args <- getArgs
  tmp <- getTemporaryDirectory
  withTempDirectory normal tmp "ghcjs-boot" $ \tmpDir -> shelly $ do
    when (any (=="--auto") args) (autoBoot $ fromString tmpDir)
    ignoreExcep $ installBootPackages (bootSettings args)

fromString :: String -> FilePath
fromString = fromText . T.pack

toString :: FilePath -> String
toString = T.unpack . toTextIgnore

ignoreExcep a = a `catchany_sh` (\e -> echo $ "exception: " <> T.pack (show e))

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
                          (src',_) <- unwrapResumable src
                          tar <- lazyConsume (src' =$= bunzip2)
                          liftIO $ putStrLn "unpacking tar"
                          liftIO $ Tar.unpack (toString tmp)  (Tar.read $ L.fromChunks tar) -- (Tar.checkTarbomb ("ghc-" ++ ghcVer) $ Tar.read tar)
  cd (tmp </> T.pack ghcDir)
  run_ "sh" ["configure"]
  ignoreExcep $ run_ "make" ["-j4", "1"] -- for some reason this still fails after a succesful build

corePackages :: [Text]
corePackages = ["ghc-prim", "integer-gmp", "base"]
-- corePackages = ["ghc-prim", "integer-gmp", "base", "array", "deepseq", "containers", "template-haskell"]

-- to be called from a configured and built (at least stage 1) ghc tree
installBootPackages :: BootSettings -> ShIO ()
installBootPackages settings = do
  mghcjs <- which "ghcjs"
  mghcjspkg <- which "ghcjs-pkg"
  case (mghcjs, mghcjspkg) of
    (Just ghcjs, Just ghcjspkg) -> do
      echo $ "Booting: " <> toTextIgnore ghcjs <> " (" <> toTextIgnore ghcjspkg <> ")"
      ghcjsVer <- T.strip <$> run ghcjs ["--numeric-version"]
      when (not $ noBuild settings) $ do
        addWrappers ghcjs ghcjspkg
      initPackageDB
      when (not (skipRts settings || noBuild settings)) $ do
           echo "Building RTS"
           run_ "make" ["all_rts","-j4"] `catchany_sh` (const (return ()))
      when (not $ noBuild settings) $ forM_ corePackages $ \pkg -> do
        echo $ "Building package: " <> pkg
        run_ "make" ["all_libraries/"<>pkg, "-j4", "GHC_STAGE1=inplace/bin/ghcjs"]

      installRts
      mapM_ (installPkg ghcjs ghcjspkg) corePackages
    _ -> echo "Error: ghcjs and ghcjs-pkg must be in the PATH"

-- add inplace/bin/ghcjs and inplace/bin/ghcjs-pkg wrappers
addWrappers :: FilePath -> FilePath -> ShIO ()
addWrappers ghcjs ghcjspkg = do
    ghcwrapper <- readfile "inplace/bin/ghc-stage1"
    writefile "inplace/bin/ghcjs" (fixGhcWrapper ghcwrapper ghcjs)
    makeExecutable "inplace/bin/ghcjs"
    pkgwrapper <- readfile "inplace/bin/ghc-pkg"
    writefile "inplace/bin/ghcjs-pkg" (fixPkgWrapper pkgwrapper ghcjspkg)
    makeExecutable "inplace/bin/ghcjs-pkg"
 where
    makeExecutable f = liftIO $ do
        p <- getPermissions f
        setPermissions f (p {executable = True})

fixGhcWrapper :: Text -> FilePath -> Text
fixGhcWrapper wrapper ghcjs = T.unlines . concatMap fixLine . T.lines $ wrapper
    where
      exec = "executablename="
      fixLine line
          | exec `T.isPrefixOf` line =
              [ exec <> "\"" <> toTextIgnore ghcjs <> "\""
              , "export GHCJS_FALLBACK_GHC=" <> T.drop (T.length exec) line
              , "export GHCJS_FALLBACK_PLAIN=1"
              ]
          | otherwise                             = [line]

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
  rtsConf <- readfile "rts/package.conf.inplace"
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
  rm_rf . fromString =<< liftIO getGlobalPackageDB
  rm_rf . fromString =<< liftIO getUserPackageDB
  mkdir_p (fromString base)
  mkdir_p (fromString inst)
  run_ "ghcjs-pkg" ["initglobal"] `catchany_sh` const (return ())
  run_ "ghcjs-pkg" ["inituser"] `catchany_sh` const (return ())


installPkg :: FilePath -> FilePath -> Text -> ShIO ()
installPkg ghcjs ghcjspkg pkg = verbosely $ do
  echo $ "installing package: " <> pkg
  base <- liftIO getGlobalPackageBase
  dest <- liftIO getGlobalPackageInst
  db   <- liftIO getGlobalPackageDB
  run_ "inplace/bin/ghc-cabal" [ "install"
                               , toTextIgnore ghcjs
                               , toTextIgnore ghcjspkg
                               , "strip"
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
  dirs <- chdir (fromString dest) (ls "")
  case filter (\x -> (pkg <> "-") `T.isPrefixOf` toTextIgnore x) dirs of
    (d:_) -> do
      echo $ "found installed version: " <> toTextIgnore d
      sub $ do
        cd ("libraries" </> pkg </> "dist-install" </> "build")
        files <- findWhen (return.isJsFile) "."
        forM_ (filter isJsFile files) $ \file -> do
           echo $ "installing " <> toTextIgnore file
           cp file (dest </> d </> file)
    _ -> errorExit $ "could not find installed package " <> pkg
  echo "done"

isPathPrefix :: Text -> FilePath -> Bool
isPathPrefix t file = t `T.isPrefixOf` toTextIgnore file

isJsFile :: FilePath -> Bool
isJsFile file = ".js" `T.isSuffixOf` toTextIgnore file


