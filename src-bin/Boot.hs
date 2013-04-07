{-# LANGUAGE ExtendedDefaultRules      #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE CPP                       #-}

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
import           Config
import           Control.Monad             (forM, forM_)
import qualified Data.ByteString.Lazy      as L
import           Data.Char
import           Data.Maybe                (catMaybes)
import           Data.Monoid
import           Data.Text.Lazy            (Text)
import qualified Data.Text.Lazy            as T
import           Filesystem                (isFile)
import           Filesystem.Path.CurrentOS (empty, stripPrefix, encodeString, decodeString, replaceExtension)
import           Prelude                   hiding (FilePath)
import           Shelly

import           Distribution.Simple.Utils (withTempDirectory)
import           Distribution.Verbosity    (normal)
import           Network                   (withSocketsDo)
import           System.Directory
import           System.Environment

import qualified Codec.Archive.Tar         as Tar
import qualified Codec.Archive.Tar.Check   as Tar

import           Control.Monad
import           Data.Conduit              (unwrapResumable, ($$), ($=), (=$=))
import           Data.Conduit.BZlib
import           Data.Conduit.Lazy         (lazyConsume)
import           Data.Maybe (listToMaybe)
import           Network.HTTP.Conduit
import           System.Directory          (Permissions (..), getPermissions,
                                            setPermissions)

import           Options.Applicative

ghcDownloadLocation :: String -> String
ghcDownloadLocation ver =
    "http://www.haskell.org/ghc/dist/" ++ ver ++ "/ghc-" ++ ver ++ "-src.tar.bz2"

data BootSettings = BootSettings { skipRts   :: Bool -- ^ skip building the rts
                                 , noBuild   :: Bool -- ^ do not build, only reset the packages
                                 , initTree  :: Bool -- ^ initialize the source tree: configure/build native ghc first
                                 , onlyBasic :: Bool -- ^ only build basic packages: ghc-prim, integer-gmp, base
                                 , reboot    :: Bool -- ^ reboot using log of old ghcjs invocations
                                 , autoBoot  :: Bool
                                 , preBuilt  :: Bool
                                 } deriving (Ord, Eq)

optParser' :: ParserInfo BootSettings
optParser' = info (helper <*> optParser) ( fullDesc <>
                  header "GHCJS booter, build base libraries for the compiler" )

optParser :: Parser BootSettings
optParser = BootSettings
            <$> switch ( long "skipRts"   <> short 's' <>
                  help "skip building the RTS" )
            <*> switch ( long "noBuild"   <> short 'n' <>
                  help "do not build, only reinstall packages" )
            <*> switch ( long "init"      <> short 'i' <>
                  help "initialize the GHC source tree for first time build" )
            <*> switch ( long "onlyBasic" <> short 'b' <>
                  help "only build ghc-prim, integer-gmp and base" )
            <*> switch ( long "reboot"    <> short 'r' <>
                  help "reboot using the same settings as last time (can be invoked from any directory)" )
            <*> switch ( long "auto"      <> short 'a' <>
                  help "attempt fully automated boot, download GHC sources" )
            <*> switch ( long "preBuilt"  <> short 'p' <>
                  help "download prebuilt libraries for compiler (might not be available)" )

main = withSocketsDo $ do
  tmp <- getTemporaryDirectory
  withTempDirectory normal False tmp "ghcjs-boot" mainTmp

mainTmp tmpDir = execParser optParser' >>= \s -> shelly $ do
    when (autoBoot s) (doAutoBoot $ fromString tmpDir)
    when (initTree s && not (autoBoot s)) setupBuild
    ignoreExcep $ installBootPackages s

fromString :: String -> FilePath
fromString = fromText . T.pack

toString :: FilePath -> String
toString = T.unpack . toTextIgnore

ignoreExcep a = a `catchany_sh` (\e -> echo $ "exception: " <> T.pack (show e))

-- autoboots roll out
-- download and configure a fresh ghc source tree
doAutoBoot :: FilePath -> ShIO ()
doAutoBoot tmp = shelly $ do
  ghcVer <- T.unpack . T.strip <$> run "ghc" ["--numeric-version"]
  let ghcDir = "ghc-" ++ ghcVer
  cd tmp
  liftIO $ do
    putStrLn $ "fetching GHC: " ++ ghcDownloadLocation ghcVer
    request <- parseUrl (ghcDownloadLocation ghcVer)
    withManager $ \manager -> do
                          response <- http request manager
                          (src',_) <- unwrapResumable (responseBody response)
                          tar <- lazyConsume (src' =$= bunzip2)
                          liftIO $ putStrLn "unpacking tar"
                          liftIO $ Tar.unpack (toString tmp)  (Tar.read $ L.fromChunks tar) -- (Tar.checkTarbomb ("ghc-" ++ ghcVer) $ Tar.read tar)
  cd (tmp </> T.pack ghcDir)
  setupBuild

setupBuild :: ShIO ()
setupBuild = do
  mk <- readfile "mk/build.mk.sample"
  writefile "mk/build.mk" ("BuildFlavour = quick\n" <> mk)
  run_ "sh" ["configure","--enable-bootstrap-with-devel-snapshot"]
  ignoreExcep $ run_ "make" ["-j4"] -- for some reason this still fails after a succesful build

corePackages :: Bool -> [Text]
corePackages True = [ "ghc-prim", "integer-gmp", "base" ]
corePackages _ = [ "ghc-prim"
                 , "integer-gmp"
                 , "base"
                 , "array"
                 , "deepseq"
                 , "containers"
                 , "pretty"
                 , "template-haskell"
                 ]

-- to be called from a configured and built (at least stage 1) ghc tree
installBootPackages :: BootSettings -> ShIO ()
installBootPackages settings = do
  mghcjs <- which "ghcjs"
  mghcjspkg <- which "ghcjs-pkg"
  case (mghcjs, mghcjspkg) of
    (Just ghcjs, Just ghcjspkg) -> do
      echo $ "Booting: " <> toTextIgnore ghcjs <> " (" <> toTextIgnore ghcjspkg <> ")"
      p <- pwd
      ghcjsVer <- T.strip <$> run ghcjs ["--numeric-version"]
      base  <- liftIO getGlobalPackageBase
      let commandLog = base </> "boot.log"
      when (not (noBuild settings || reboot settings)) $ do
        addWrappers ghcjs (toTextIgnore commandLog) ghcjspkg
      initPackageDB
      let corePkgs = corePackages (onlyBasic settings)
      when (not (skipRts settings || noBuild settings || reboot settings)) $ do
           echo "Building RTS"
           run_ "make" ["all_rts","-j4"] `catchany_sh` (const (return ()))
           replacePrimOps
      if reboot settings
        then do
          echo "Rebooting"
          lines <- T.lines <$> readfile commandLog
          case lines of
            (wd:cmds) -> do
              cd (fromText wd)
              setenv "GHCJS_NO_NATIVE" "1"
              setenv "GHCJS_FALLBACK_PLAIN" "1"
              forM_ cmds $ \r ->
                let args = T.words r
                in  when (isRebootCmd args) $ do
                      echo ("replaying: ghcjs " <> r)
                      (run_ "ghcjs" (T.words r))
            _ -> error "can't reboot, invalid boot log"
        else do
          when (not $ noBuild settings) $ do
             rm_f commandLog
             p <- pwd
             writefile commandLog (toTextIgnore p <> "\n")
             forM_ corePkgs $ \pkg -> do
               echo $ "Building package: " <> pkg
               buildPkg pkg
      installRts
      mapM_ (installPkg ghcjs ghcjspkg) corePkgs
      installFakes
      cd p
    _ -> echo "Error: ghcjs and ghcjs-pkg must be in the PATH"

-- when rebooting, we replay the log of ghcjs calls
-- but some of them are for package configuration
-- testing features etc, filter them out here
isRebootCmd :: [Text] -> Bool
isRebootCmd args
  | any (`elem` args)
        [ "-M"
        , "--info"
        , "--numeric-version"
        , "--print-libdir"
        , "--supported-languages"
        ] = False
  | any isTmpCSource args = False
  | otherwise = True
  where
    isTmpCSource a = "/tmp/" `T.isPrefixOf` a &&
                     ".c" `T.isSuffixOf` a

-- add inplace/bin/ghcjs and inplace/bin/ghcjs-pkg wrappers
addWrappers :: FilePath -> Text -> FilePath -> ShIO ()
addWrappers ghcjs log ghcjspkg = do
    ghcwrapper <- readfile "inplace/bin/ghc-stage1"
    writefile "inplace/bin/ghcjs" (fixGhcWrapper ghcwrapper log ghcjs)
    makeExecutable "inplace/bin/ghcjs"
    pkgwrapper <- readfile "inplace/bin/ghc-pkg"
    writefile "inplace/bin/ghcjs-pkg" (fixPkgWrapper pkgwrapper ghcjspkg)
    makeExecutable "inplace/bin/ghcjs-pkg"
 where
    makeExecutable f = liftIO $ do
        p <- getPermissions f
        setPermissions f (p {executable = True})

fixGhcWrapper :: Text -> Text -> FilePath -> Text
fixGhcWrapper wrapper log ghcjs = T.unlines . concatMap fixLine . T.lines $ wrapper
    where
      exec = "executablename="
      fixLine line
          | exec `T.isPrefixOf` line =
              [ exec <> "\"" <> toTextIgnore ghcjs <> "\""
              , "export GHCJS_FALLBACK_GHC=" <> T.drop (T.length exec) line
              , "export GHCJS_FALLBACK_PLAIN=1"
              , "export GHCJS_NO_NATIVE=1"
              , "export GHCJS_FAKE_NATIVE=1"
              , "export GHCJS_LOG_COMMANDLINE=1"
              , "export GHCJS_LOG_COMMANDLINE_NAME=" <> log
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
  cp "inplace/lib/platformConstants" (base </> "platformConstants")
  cp "inplace/lib/platformConstants" (base </> lib </> "platformConstants")


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

-- | make fake, empty packages to keep the build system happy
installFakes :: ShIO ()
installFakes = do
  base <- T.pack <$> liftIO getGlobalPackageBase
  db   <- T.pack <$> liftIO getGlobalPackageDB
  installed <- T.words <$> run "ghc-pkg" ["list", "--simple-output"]
  dumped <- T.lines <$> run "ghc-pkg" ["dump"]
  forM_ fakePkgs $ \pkg ->
    case filter ((pkg<>"-") `T.isPrefixOf`) installed of
      [] -> error (T.unpack $ "required package " <> pkg <> " not found in host GHC")
      (x:_) -> do
        let version = T.drop 1 (T.dropWhile (/='-') x)
        case findPkgId dumped pkg version of
          Nothing -> error (T.unpack $ "cannot find package id of " <> pkg <> "-" <> version)
          Just pkgId -> do
            let conf = fakeConf base base pkg version pkgId
            writefile (db </> (pkgId <.> "conf")) conf
  run_ "ghcjs-pkg" ["recache", "--global"]

findPkgId:: [Text] -> Text -> Text -> Maybe Text
findPkgId dump pkg version =
  listToMaybe (filter (pkgVer `T.isPrefixOf`) ids)
    where
      pkgVer = pkg <> "-" <> version <> "-"
      ids = map (T.dropWhile isSpace . T.drop 3) $ filter ("id:" `T.isPrefixOf`) dump

fakePkgs = [ "Cabal" ]

fakeConf :: Text -> Text -> Text -> Text -> Text -> Text
fakeConf incl lib name version pkgId = T.unlines
            [ "name:           " <> name
            , "version:        " <> version
            , "id:             " <> pkgId
            , "license:        BSD3"
            , "maintainer:     stegeman@gmail.com"
            , "import-dirs:    " <> incl
            , "include-dirs:   " <> incl
            , "library-dirs:   " <> lib
            , "exposed:        False"
            ]

initPackageDB :: ShIO ()
initPackageDB = do
  echo "creating package databases"
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
  run_ "inplace/bin/ghc-cabal" [ "copy"
                               , "strip"
                               , "libraries/" <> pkg
                               , "dist-install"
                               , ""
                               , T.pack base
                               , T.pack dest
                               , T.pack base <> "/doc"
                               ]
  run_ "inplace/bin/ghc-cabal" [ "register"
                               , toTextIgnore ghcjs     -- ghc
                               , toTextIgnore ghcjspkg  -- ghcpkg
                               , T.pack dest -- topdir
                               , "libraries/" <> pkg -- directory
                               , "dist-install" -- distDir
                               , "" -- myDestDir
                               , T.pack base -- myPrefix
                               , T.pack dest -- myLibDir
                               , T.pack base <> "/doc" -- myDocDir
                               , "NO" -- relocatablebuild
                               ]
  -- now install the javascript files
  dirs <- chdir (fromString dest) (ls "")
  case filter (\x -> (pkg <> "-") `T.isPrefixOf` toTextIgnore x) dirs of
    (d:_) -> do
      echo $ "found installed version: " <> toTextIgnore d
      sub $ do
        cd ("libraries" </> pkg </> "dist-install" </> "build")
        files <- findWhen (return . isGhcjsFile) "."
        forM_ files $ \file -> do
           echo $ "installing " <> toTextIgnore file
           cp file (dest </> d </> file)
    _ -> errorExit $ "could not find installed package " <> pkg
  echo "done"

isPathPrefix :: Text -> FilePath -> Bool
isPathPrefix t file = t `T.isPrefixOf` toTextIgnore file

isGhcjsFile :: FilePath -> Bool
isGhcjsFile file = any (`T.isSuffixOf` toTextIgnore file) [".js", ".ji", ".native_hi", ".js_o", ".js_hi"]

-- | make sure primops.txt is the one from our data, configured for
--   JavaScript building
replacePrimOps :: ShIO ()
replacePrimOps = do
   base <- liftIO (decodeString <$> ghcjsDataDir)
   mapM_ (cp (base </> ("include/prim/primops-" ++ cProjectVersionInt) <.> "txt")) targets
   where
     -- not sure which of these are really necessary
     targets = [ "compiler/stage1/build/primops.txt"
               , "compiler/stage2/build/primops.txt"
               , "compiler/prelude/primops.txt"
               ]

buildPkg :: Text -> ShIO ()
buildPkg pkg = do
   patchPkg pkg
   cleanPkg pkg
   run_ "make" ["all_libraries/" <> pkg, "GHC_STAGE1=inplace/bin/ghcjs"]

-- | some packages need to be patched to run with GHCJS, for GHC 7.8.1:
--    try packagename-708_1.patch first, then packagename-708.patch
patchPkg :: Text -> ShIO ()
patchPkg pkg = do
  mfile <- findPatchFile pkg $ map T.pack
                                [ cProjectVersionInt ++ "_" ++ cProjectPatchLevel
                                , cProjectVersionInt
                                ]
  case mfile of
    Nothing -> return () -- no patch
    Just p -> sub $ do
      echo $ "applying patch: " <> toTextIgnore p
      cd ("libraries" </> pkg)
      readfile p >>= setStdin
      ignoreExcep $ run_ "patch" ["-p1", "-N"] -- ignore already applied patches

findPatchFile :: Text -> [Text] -> ShIO (Maybe FilePath)
findPatchFile pkg variants = liftIO $ do
  base <- decodeString <$> ghcjsDataDir
  listToMaybe <$> filterM (doesFileExist . encodeString)
    (map (\x -> base </> "patch" </> (pkg <> "-" <> x) <.> "patch") variants)

-- | remove files that we want to regenerate, backup .hi to make that things get rebuilt
cleanPkg :: Text -> ShIO ()
cleanPkg pkg = do
  findWhen (return . hasExt "hi") pkgDir >>= mapM_
    (\file -> mvNoOverwrite file $ replaceExtension file "backup_hi")
  findWhen (return . hasExt "o") pkgDir >>= mapM_
    (\file -> mvNoOverwrite file $ replaceExtension file "backup_o")
  findWhen isRemovedFile pkgDir >>= mapM_ rm
  where
    pkgDir = "libraries" </> pkg </> "dist-install" </> "build"
    isRemovedFile file = return $ any (`hasExt` file) ["hs", "lhs", "p_hi", "dyn_hi"]

mvNoOverwrite :: FilePath -> FilePath -> ShIO ()
mvNoOverwrite from to = do
  to' <- absPath to
  e <- liftIO (isFile to')
  if e
    then rm from
    else mv from to
