{-# OPTIONS_GHC -O0 #-}

{- |
    The ghcjs-boot program installs the libraries and runtime system for GHCJS

    You can explicitly set the boot source location with the -s option. It
    can either be a directory containing the boot.yaml file or a boot.tar
    archive.

    if the -s option is not set, the following locations are tried in order:

      1. the current directory if it contains boot.yaml
      2. the boot.tar file installed by cabal install in the GHCJS
         data directory

    ghcjs-boot installs the libraries into the GHCJS library directory,
    which is set by the `ghcjs' and `ghcjs-pkg' scripts, which pass the
    library path with the -B option to the underlying binary executable.

    modify the scripts to change the installation location
 -}

{-# LANGUAGE ExtendedDefaultRules,
             OverloadedStrings,
             ScopedTypeVariables,
             TemplateHaskell,
             LambdaCase,
             FlexibleInstances,
             DeriveDataTypeable,
             GeneralizedNewtypeDeriving,
             NoMonomorphismRestriction,
             FlexibleContexts,
             RankNTypes,
             TupleSections
  #-}
module Main (main) where

import           Prelude
  hiding (elem, mapM, mapM_, any, all, concat, concatMap)

-- import qualified Prelude

import qualified Distribution.Simple.Utils       as Cabal
import qualified Distribution.Verbosity          as Cabal


import qualified Codec.Archive.Tar               as Tar
import qualified Codec.Archive.Tar.Entry         as Tar

import           Control.Applicative
import qualified Control.Exception               as Ex
import           Control.Lens                    hiding ((<.>))
import           Control.Monad.Reader            (MonadReader, ReaderT(..), ask )
import           Control.Monad.State

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import           Data.Data
import           Data.Data.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (intercalate, transpose)
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import qualified Data.Vector                     as V
import           Data.Yaml                       ((.:))
import qualified Data.Yaml                       as Yaml
import           GHC.IO.Encoding
  (setLocaleEncoding, setForeignEncoding, utf8)

import           Options.Applicative             hiding (info)
import qualified Options.Applicative             as O

import           System.Directory
import           System.Environment              (lookupEnv, setEnv)
import           System.Environment.Executable   (getExecutablePath)
import           System.Exit
  (exitSuccess, exitFailure, ExitCode(..))
import qualified System.FilePath                 as FP
import           System.FilePath --  hiding ((<.>), (</>))

import           System.IO
  (hPutStrLn, hSetBuffering, stderr, stdout, BufferMode(..), hClose)
import           System.PosixCompat.Files        (setFileMode)
import           System.Process                  (readCreateProcess, createProcess, waitForProcess, {-CreateProcess(..),-} proc)

-- import           Shelly                          ((<.>), fromText)
-- import qualified Shelly                          as Sh

import           Compiler.GhcjsProgram           (printVersion)
import qualified Compiler.Info                   as Info
import qualified Compiler.Utils                  as Utils
import           Compiler.Settings               (NodeSettings(..))

-- import qualified Paths_ghcjs                     as Paths

import           System.IO.Error                 (isDoesNotExistError)
import           Compiler.Platform               (isWindows)

-- fixme force overwrite?

default (Text)

newtype Verbosity = Verbosity Int deriving (Eq, Ord, Data, Typeable)

trace, info, warn, err :: Verbosity
trace = Verbosity 3
info  = Verbosity 2
warn  = Verbosity 1
err   = Verbosity 0

data BootSettings = BootSettings
  { _bsShowVersion  :: Bool       -- ^show the version and exit
  , _bsJobs         :: Maybe Int  -- ^number of parallel jobs
  , _bsDebug        :: Bool       {- ^build debug version of the libraries
                                      (GHCJS records the STG in the object
                                      files for easier inspection) -}
  , _bsProf         :: Bool       -- ^build profiling version of the libraries
  , _bsHaddock      :: Bool       -- ^build documentation
  , _bsVerbosity    :: Verbosity  -- ^verbosity level 0..3, 2 is default
  , _bsWithCabal    :: Maybe Text {- ^location of cabal (cabal-install)
                                      executable, must have GHCJS support -}
  -- , _bsBindir       :: Maybe Text -- ^ bin directory to install GHCJS programs
  -- , _bsLibexecDir :: Maybe Text -- ^ location of GHCJS executables
  -- , _bsGhcjsLibdir  :: Maybe Text -- ^ location to install GHCJS boot libraries
  , _bsWithEmsdk    :: Maybe Text
  , _bsWithGhc      :: Maybe Text {- ^location of GHC compiler (must have a
                                      GHCJS-compatible Cabal library
                                      installed. ghcjs-boot copies some files
                                      from this compiler) -}
  , _bsWithGhcPkg   :: Maybe Text -- ^location of ghc-pkg program
  , _bsWithNode     :: Maybe Text -- ^location of the node.js program
  , _bsWithNodePath :: Maybe Text -- ^ NODE_PATH to use when running node.js Template Haskell or REPL
                                  --      (if unspecified, GHCJS uses bundled packages)
  , _bsNodeExtraArgs :: Maybe Text -- ^ extra node arguments
  , _bsSourceDir    :: Maybe Text -- ^source directory (can be a tar file)
  } deriving (Ord, Eq, Data, Typeable)


{- | Stage configuration file: packages to install in each stage

     see boot.yaml for more information
 -}
data BootStages = BootStages
  { _bstStage1a   :: Stage
  , _bstStage1b   :: Stage
  , _bstPretend   :: [Package] {- ^packages we pretend to have in stage one,
                                   but actually hand off to GHC -}
  , _bstCabal     :: Package   {- ^installed between 1b and 2,
                                   only when doing a full boot -}
  , _bstGhcjsTh   :: Package   -- ^installed between 1b and 2
  , _bstGhcjsPrim :: Package   -- ^installed between 1a and 1b
  , _bstGhcPrim   :: Package   -- ^installed before stage 1a
  } deriving (Data, Typeable)

type Stage   = [Package]

type Package = Text {- ^just the package name/location, unversioned

                        can be a directory name
                        (starting with ./ relative to the ghcjs-boot root),
                        a url or a plain package name
                     -}

data BootLocations = BootLocations
  { _blSourceDir   :: FilePath
  , _blBuildDir    :: FilePath
  , _blGhcjsTopDir :: FilePath
  , _blGlobalDB    :: FilePath -- ^global package database
  -- , _blLibexecDir  :: FilePath
  , _blEmsdk       :: FilePath
  } deriving (Data, Typeable, Show)

data Program = Program
  { _pgmName    :: Text           -- ^program name for messages
  , _pgmSearch  :: Text           {- ^name searched for when configuring the
                                      program (from command line or config file)
                                   -}
  , _pgmVersion :: Maybe Text     -- ^version if known
  , _pgmLoc     :: Maybe FilePath -- ^absolute path to the program
  , _pgmArgs    :: [Text]         -- ^extra arguments to pass to the program
  } deriving (Data, Typeable, Show)

-- | configured programs, fail early if any of the required programs is missing
data BootPrograms = BootPrograms { _bpGhc        :: Program
                                 , _bpGhcPkg     :: Program
                                 , _bpCabal      :: Program
                                 , _bpNode       :: Program
                                 } deriving (Data, Typeable, Show)

data BootEnv = BootEnv { _beSettings  :: BootSettings
                       , _beLocations :: BootLocations
                       , _bePrograms  :: BootPrograms
                       , _beStages    :: BootStages
                       }

data BootConfigFile = BootConfigFile BootStages BootPrograms
  deriving (Data, Typeable)

makeLenses ''Program
makeLenses ''BootSettings
makeLenses ''BootLocations
makeLenses ''BootPrograms
makeLenses ''BootStages
makeLenses ''BootEnv

main :: IO ()
main = do
    settings <- execParser optParser'
    when (settings ^. bsShowVersion) (printVersion >> exitSuccess)
    hSetBuffering stdout LineBuffering
    setLocaleEncoding utf8
    setForeignEncoding utf8
    env <- initBootEnv settings
    printBootEnvSummary False env
    r <- runReaderT ((actions >> pure Nothing)
                        `catchAny` (pure . Just)) env
    maybe exitSuccess Ex.throwIO r
  where
    actions :: B ()
    actions = do
      e <- ask
      liftIO $ setEnv "GHC_CHARENC" "UTF-8"
      cleanCache
      prepareLibDir
      let base = libDir (e ^. beLocations)
      liftIO $ setEnv "CFLAGS" ("-I" <> (base </> "include"))
      installFakes
      installStage1
      removeFakes
      installCabal
      when (e ^. beSettings . bsHaddock) buildDocIndex
      liftIO . printBootEnvSummary True =<< ask

instance Yaml.FromJSON BootPrograms where
  parseJSON (Yaml.Object v) = BootPrograms
    <$> v ..: "ghc"   <*> v ..: "ghc-pkg"
    <*> v ..: "cabal" <*> v ..: "node"
    where
      o ..: p = ((\t -> Program p t Nothing Nothing []) <$> o .: p) <|>
                (withArgs p =<< o .: p)
      withArgs :: Text -> Yaml.Value -> Yaml.Parser Program
      withArgs p (Yaml.Object o) | [(k,v)] <- HM.toList o =
        Program p k Nothing Nothing <$> Yaml.parseJSON v
      withArgs _ _ =
        mempty
  parseJSON _ = mempty

instance Yaml.FromJSON BootStages where
  parseJSON (Yaml.Object v) = BootStages
    <$> v ..: "stage1a"
    <*> v ..: "stage1b"
    <*> v .:: "stage1PretendToHave"
    <*> v  .: "cabal"
    <*> v  .: "ghcjs-th"
    <*> v  .: "ghcjs-prim"
    <*> v  .: "ghc-prim"
    where
      o .:: p = ((:[])<$>o.:p) <|> o.:p
      o ..: p = pkgs =<< o .: p
      pkgs (Yaml.String t) =
        pure [t]
      pkgs (Yaml.Array v) =
        concat <$> mapM pkgs (V.toList v)
      pkgs __ =
        mempty
  parseJSON _ = mempty

instance Yaml.FromJSON BootConfigFile where
  parseJSON (Yaml.Object v) = BootConfigFile
    <$> v .: "packages" <*> v .: "programs"
  parseJSON _ = mempty

optParser' :: ParserInfo BootSettings
optParser' =
  O.info (helper <*> optParser)
         (fullDesc <>
          header "GHCJS booter, build base libraries for the compiler" <>
          progDesc description)

description :: String
description = unlines
  [ "ghcjs-boot builds an initial set of libraries for GHCJS."
  ]

optParser :: Parser BootSettings
optParser =
  BootSettings
    <$> switch
        (long "version" <>
        help "show the ghcjs-boot version")
    <*> (optional . option auto)
           (long "jobs" <>
            short 'j' <>
            metavar "JOBS" <>
            help "number of jobs to run in parallel")
    <*> switch
          (long "debug" <>
           short 'd' <>
           help "build debug libraries with extra checks")
    <*> (fmap not . switch)
          (long "no-prof" <>
           help "don't generate profiling version of the libraries")
    <*> (fmap not . switch)
          (long "no-haddock" <>
           help "don't generate documentation")
    <*> (fmap Verbosity . option auto)
          (long "verbosity" <>
           short 'v' <>
           value 2 <>
           help "verbose output")
    <*> (optional . fmap T.pack . strOption)
          (long "with-cabal" <> metavar "PROGRAM" <>
           help "cabal program to use")
    <*> (optional . fmap T.pack . strOption)
          (long "with-emsdk" <>
           metavar "DIR" <>
           help "location of Emscripten SDK")
    <*> (optional . fmap T.pack . strOption)
          (long "with-ghc" <>
           metavar "PROGRAM" <>
           help "ghc program to use")
    <*> (optional . fmap T.pack . strOption)
          (long "with-ghc-pkg" <> metavar "PROGRAM" <>
           help "ghc-pkg program to use")
    <*> (optional . fmap T.pack . strOption)
          (long "with-node" <>
           metavar "PROGRAM" <>
           help "node.js program to use")
    <*> (optional . fmap T.pack . strOption)
          (long "with-node-path" <> metavar "PATH" <>
           help "value of NODE_PATH environment variable when running Template Haskell or GHCJSi")
    <*> (optional . fmap T.pack . strOption)
          (long "extra-node-args" <> metavar "ARGS" <>
           help "extra arguments to pass to node.js")
    <*> (optional . fmap T.pack . strOption)
          (long "source-dir" <>
           short 's' <>
           metavar "DIR" <>
           help "location of GHCJS library sources")

initPackageDB :: B ()
initPackageDB = do
  msg info "creating package databases"
  initDB "--global" <^> beLocations . blGlobalDB
  where
    initDB dbName db = do
      rm_rf db
      ghcjs_pkg_ ["init", T.pack db] `catchAny_` return ()
      ghcjs_pkg_ ["recache", dbName]

cleanCache :: B ()
cleanCache =
  liftIO Info.getUserCacheDir >>= \case
    Just p -> rm_rf p `catchAny_` return ()
    Nothing -> return ()

prepareLibDir :: B ()
prepareLibDir = do
  msg info "preparing GHCJS library directory"
  locs <- view beLocations
  pgms <- view bePrograms
  liftIO $ print (locs, pgms)
  ver <- liftIO $ ghcjsVersion pgms
  let globalDB = locs ^. blGlobalDB
      ghcjsLib = libDir locs
      ghcjsTop = locs ^. blGhcjsTopDir
      rtsConfFile   = "rts.conf"
      wrapperSrcDir = (locs ^. blSourceDir) </> "input" </> "wrappers"
      wenv = WrapperEnv { weTopDir     = T.pack (libDir locs)
                        , weBinDir     = T.pack ((locs ^. blGhcjsTopDir) </> "bin")
                        , weLibexecDir = T.pack (libDir locs </> "bin")
                        , weVersion    = ver
                        , weGhcVersion = ver
                        , weEmsdk      = T.pack (locs ^. blEmsdk)
                        }
  rtsConf <- replacePlaceholders wenv <$>
             liftIO (T.readFile ((locs ^. blSourceDir) </> "input" </> "rts" <.> "conf"))
  mkdir_p (ghcjsLib </> "bin")

  msg info "copying GHCJS executables"

  -- copy the executables from the package to the libraries directory
  -- (the executables in the package have been prefixed with `private-ghcjs-`
  -- as a workaround for cabal-install incorrectly generating symlinks for
  -- private executables)
  let cpExe name = do
        let srcName = ghcjsTop </> "bin" </> exe (T.unpack $ "private-ghcjs-" <> name)
            tgtName = ghcjsLib </> "bin" </> exe (T.unpack name)
        cp srcName tgtName
        liftIO . Cabal.setFileExecutable $ tgtName

  mapM_ cpExe [ "unlit", "run" ]

  when isWindows (mapM_ cpExe ["touchy", "wrapper"])

  msg info "creating emscripten wrapper"

  liftIO $ copyWrapper "emcc"
                       Nothing
                       wenv
                       (ghcjsLib </> "bin")
                       wrapperSrcDir
                       (ghcjsLib </> "bin")

  msg info "adding RTS package"
  initPackageDB
  liftIO $ T.writeFile (globalDB </> rtsConfFile) rtsConf
  ghcjs_pkg_ ["recache", "--global", "--no-user-package-db"]
  cp_r ((locs ^. blSourceDir) </> "shims") (ghcjsLib </> "shims")
  cp_r ((locs ^. blSourceDir) </> "data")  ghcjsLib
  liftIO . T.writeFile (ghcjsLib </> "settings") =<< generateSettings
  -- workaround for hardcoded check of mingw dir in ghc when building on Windows
  when isWindows (mkdir_p $ ghcjsTop </> "mingw")
  prepareNodeJs
  unless isWindows $ do
    let runSh = ghcjsLib </> "run" <.> "sh"
    liftIO (T.writeFile runSh "#!/bin/sh\nCOMMAND=$1\nshift\n\"$COMMAND\" \"$@\"\n")
    liftIO (Cabal.setFileExecutable =<< makeAbsolute runSh)
  -- make an empty object file for
  subLib $ do
    liftIO (T.writeFile "empty.c" "")
    ghc_ ["-c", "empty.c"]
  msg info "RTS prepared"

generateSettings :: B Text
generateSettings = do
  emsdk <- view (beLocations . blEmsdk)
  -- do we also need emar, emranlib?
  let opts :: [(String, String)]
      opts = [("GCC extra via C opts", " -fwrapv -fno-builtin"),
              ("C compiler command", emsdk </> "upstream" </> "bin" </> "clang" {-"asmjs-none-gcc"-} {-T.unpack emccProgram-}),
              ("C compiler flags", ""),
              ("C compiler link flags", " "),
              ("C compiler supports -no-pie", "NO"),
              ("Haskell CPP command", emsdk </> "upstream" </> "bin" </> "clang"),
              ("Haskell CPP flags","-E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs"),
              ("ld command", "ld"),
              ("ld flags", ""),
              ("ld supports compact unwind", "YES"),
              ("ld supports build-id", "NO"),
              ("ld supports filelist", "YES"),
              ("ld is GNU ld", "NO"),
              ("ar command", emsdk </> "upstream" </> "emscripten" </> "emar"),
              ("ar flags", "qcls"),
              ("ar supports at file", "NO"),
              ("ranlib command", emsdk </> "upstream" </> "emscripten" </> "emranlib"),
              ("touch command", if isWindows then "$topdir/bin/touchy.exe" else "touch"),
              ("dllwrap command", "/bin/false"),
              ("windres command", "/bin/false"),
              ("libtool command", "libtool"),
              ("perl command", "/usr/bin/perl"), -- fixme windows?
              ("cross compiling", "YES"),
              ("target os", "OSUnknown"),
              ("target arch", "ArchJavaScript"),
              ("Target platform", T.unpack ghcjsTriple),
              ("target word size", "4"),
              ("target has GNU nonexec stack", "False"),
              ("target has .ident directive", "True"),
              ("target has subsections via symbols", "True"),
              ("target has RTS linker", "YES"),
              ("Unregisterised", "YES"),
              ("LLVM llc command", "llc"),
              ("LLVM opt command", "opt"),
              ("LLVM clang command", "clang")
              ]
  pure $ T.pack ("[" ++ intercalate ",\n" (map show opts) ++ "\n]")


prepareNodeJs :: B ()
prepareNodeJs = do
  ghcjsLib <- libDir <$> view beLocations
  buildDir <- view (beLocations . blBuildDir)
  nodeProgram <- view (bePrograms . bpNode . pgmLoc . to (maybe "-" T.pack))
  mbNodePath  <- view (beSettings . bsWithNodePath)
  extraArgs   <- view (beSettings . bsNodeExtraArgs)
  -- If no setting for NODE_PATH is specified, we use the libraries bundled
  -- with the ghcjs-boot submodule. We must run "npm rebuild" to build
  -- any sytem-specific components.
  when (isNothing mbNodePath) $ do
    subLib (mkdir_p "ghcjs-node")
    liftIO $ unpackTar False
                       True
                       ghcjsLib
                       (buildDir </> "ghcjs-node.tar")
    subLib' "ghcjs-node" $ npm_ ["rebuild"]
  -- write nodeSettings.json file
  let nodeSettings = NodeSettings
       { nodeProgram         = T.unpack nodeProgram
       , nodePath            = mbNodePath
       , nodeExtraArgs       = maybeToList extraArgs
       , nodeKeepAliveMaxMem = 536870912
       }
  liftIO $ BL.writeFile (T.unpack . T.pack $ ghcjsLib </> "nodeSettings.json")
                        (Aeson.encode $ Aeson.toJSON nodeSettings)

exe :: FilePath -> FilePath
exe = bool isWindows (<.>"exe") id

buildDocIndex :: B ()
buildDocIndex = subTop' "doc" $ do
  haddockFiles <- findWhen (return . isExtensionOf "haddock") "."
  haddock_ $ ["--gen-contents"
             ,"--gen-index"
             , "-o"
             , "html"
             , "--title=GHCJS Libraries"
             ] ++
             map (\p -> "--read-interface=../" <>
                        T.pack (takeDirectory p) <>
                        "," <>
                        T.pack p)
                 haddockFiles

installCabal :: B ()
installCabal = subBuild $ do
  msg info "installing Cabal library"
  removeFakes
  cabalPkg <- view (beStages . bstCabal)
  preparePackage cabalPkg
  cabalInstall [cabalPkg]

installGhcjsPrim :: B ()
installGhcjsPrim = do
  msg info "installing ghcjs-prim"
  prim <- view (beStages . bstGhcjsPrim)
  preparePackage prim
  cabalStage1 [prim]

installGhcjsTh :: B ()
installGhcjsTh = do
  msg info "installing ghcjs-th"
  ghcjsTh <- view (beStages . bstGhcjsTh)
  preparePackage ghcjsTh
  cabalStage1 [ghcjsTh]

installStage1 :: B ()
installStage1 = subBuild $ do
  prim <- view (beStages . bstGhcPrim)
  installStage "0" [prim]
  -- fixGhcPrim
  installStage "1a" =<< stagePackages bstStage1a
  installGhcjsPrim
  installStage "1b" =<< stagePackages bstStage1b
  installGhcjsTh
  resolveWiredInPackages
    where
      installStage name s = do
        msg info ("installing stage " <> name)
        forM_ s preparePackage >> cabalStage1 s

resolveWiredInPackages :: B ()
resolveWiredInPackages = subLib $ do
  wips <- readBinary ("wiredinpkgs" <.> "yaml")
  case Yaml.decodeEither' wips of
   Left err   -> failWith $
     "error parsing wired-in packages file wiredinpkgs.yaml\n" <> T.pack (show err)
   Right pkgs -> do
     pkgs' <- forM pkgs $ \p ->
       (p,) . T.strip <$> ghcjs_pkg [ "--simple-output"
                                    , "field"
                                    , p
                                    , "key"
                                    ]
     liftIO (T.writeFile ("wiredinkeys" <.> "yaml") (
       T.unlines ("# resolved wired-in packages" :
                  map (\(p,k) -> p <> ": " <> k) pkgs')))

preparePackage :: Package -> B ()
preparePackage pkg
  | "./" `T.isPrefixOf` pkg || "../" `T.isPrefixOf` pkg = sub $ do
    msg trace ("preparing package " <> pkg)
    cd (T.unpack pkg)
    rm_rf "dist"
  | otherwise = return ()

-- | register fake, empty packages to be able to build packages
--   that depend on Cabal
installFakes :: B ()
installFakes = do
  fakes <- view (beStages . bstPretend)
  forM_ fakes $ \pkg -> do
    latest <- T.lines <$> ghc_pkg ["--global", "latest", pkg]
    case latest of
      [pkgVer] -> do
        let getField field = ghc_pkg [ "--global"
                                     , "--simple-output"
                                     , "field"
                                     , pkgVer
                                     , field
                                     ]
            version = T.drop (T.length pkg + 1) pkgVer
        globalDB <- view (beLocations . blGlobalDB)
        pkgAbi <- getField "abi"
        pkgId  <- getField "id"
        libDir <- libDir <$> view beLocations
        let conf = fakeConf libDir libDir pkg version pkgId pkgAbi
        liftIO $ T.writeFile (globalDB </> T.unpack pkgId <.> "conf") conf
      _ -> failWith ("required package " <> pkg <> " not found in host GHC")
  ghcjs_pkg_ ["recache", "--global", "--no-user-package-db"]

fakeConf :: FilePath -> FilePath -> Text -> Text -> Text -> Text -> Text
fakeConf incl lib name version pkgId pkgAbi = T.unlines
            [ "name:           " <> name
            , "version:        " <> version
            , "id:             " <> pkgId
            , "key:            " <> pkgId
            , "abi:            " <> pkgAbi
            , "license:        BSD3"
            , "maintainer:     stegeman@gmail.com"
            , "import-dirs:    " <> T.pack incl
            , "include-dirs:   " <> T.pack incl
            , "library-dirs:   " <> T.pack lib
            , "exposed:        False"
            ]

-- | remove the fakes after we're done with them
removeFakes :: B ()
removeFakes = do
  fakes <- map (<>"-") <$> view (beStages . bstPretend)
  pkgs <- T.words <$> ghcjs_pkg [ "list"
                                , "--simple-output"
                                , "--no-user-package-db"
                                ]
  forM_ pkgs $ \p ->
    when (any (`T.isPrefixOf` p) fakes)
         (msg info ("unregistering " <> p) >>
          ghcjs_pkg_ ["unregister", p, "--no-user-package-db"])

-- | subshell in path relative to build dir
{-
subBuild' :: FilePath -> B a -> B a
subBuild' p a = subBuild (cd p >> a)
-}

subBuild :: B a -> B a
subBuild a = sub (view (beLocations . blBuildDir) >>= cd >> a)

-- | subshell in path relative to top installation dir
subTop' :: FilePath -> B a -> B a
subTop' p a = subTop (cd p >> a)

subTop :: B a -> B a
subTop a = sub (view (beLocations . blGhcjsTopDir) >>= cd >> a)

subLib' :: FilePath -> B a -> B a
subLib' p a = subLib (cd p >> a)

subLib :: B a -> B a
subLib a = sub ((libDir <$> view beLocations) >>= cd >> a)

{- |unpack a tar file (does not support compression)
    only supports files, does not try to emulate symlinks -}
unpackTar :: Bool             -- ^strip the first directory component?
          -> Bool             -- ^preserve symbolic links?
          -> Prelude.FilePath -- ^destination to unpack to
          -> Prelude.FilePath -- ^the tar file
          -> IO ()
unpackTar stripFirst preserveSymlinks dest tarFile = do
  createDirectoryIfMissing True dest
  entries <- Tar.read . BL.fromStrict <$> B.readFile tarFile
  void $ Tar.foldEntries (\e -> (>>=checkExtract e))
                         (return Nothing)
                         (\e -> failWith $ "error unpacking tar: " <> showT e)
                         entries
    where
      failSec e msg = failWith $ "tar security check, " <>
                                 msg <>
                                 ": " <>
                                 T.pack (Tar.entryPath e)
      checkExtract e Nothing
        | (p:_) <- FP.splitDirectories (Tar.entryPath e)
            = checkExtract e (Just p)
        | otherwise = failSec e $
            "no path"
      checkExtract e je@(Just expected)
        | FP.isAbsolute ep = failSec e $
            "absolute path"
        | elem ".." epd              = failSec e $
            "'..' in path"
        | listToMaybe epd /= je &&
          isSupportedEntry (Tar.entryContent e) = failSec e $
            "tar bomb, expected path component: " <> T.pack expected
        | otherwise                     = do
            (extractEntry e $ dest FP.</>
                   (FP.joinPath
                                (drop (if stripFirst then 1 else 0) epd)))
            return je
        where ep  = Tar.entryPath e
              epd = FP.splitDirectories ep
      isSupportedEntry Tar.NormalFile{} = True
      isSupportedEntry Tar.Directory{}  = True
      isSupportedEntry _                = False
      extractEntry :: Tar.Entry -> Prelude.FilePath -> IO ()
      extractEntry e tgt
        | Tar.NormalFile bs _size <- Tar.entryContent e = do
            createDirectoryIfMissing True (FP.dropFileName tgt)
            BL.writeFile tgt bs
            setPermissions (Tar.entryPermissions e) tgt
        | Tar.Directory <- Tar.entryContent e = do
            createDirectoryIfMissing True tgt
            setPermissions (Tar.entryPermissions e) tgt
        | Tar.SymbolicLink _linkTgt <- Tar.entryContent e
        , preserveSymlinks =
            createDirectoryIfMissing True (FP.dropFileName tgt)
        | otherwise = hPutStrLn stderr $
            "ignoring unexpected entry type in tar. " <>
            "only normal files and directories (no links) " <>
            "are supported:\n    " <> tgt
      setPermissions mode tgt = do
        absTgt <- makeAbsolute tgt
        setFileMode absTgt mode

run_inplace :: Text -> [Text] -> B Text
run_inplace pgm args = do
  ghcjsTop <- view (beLocations . blGhcjsTopDir)
  run'' (ghcjsTop </> "bin" </> T.unpack pgm) "" args

run_inplace_ :: Text -> [Text] -> B ()
run_inplace_ pgm args = do
  ghcjsTop <- view (beLocations . blGhcjsTopDir)
  run''_ (ghcjsTop </> "bin" </> T.unpack pgm) args

ghc_ :: [Text] -> B ()
ghc_         = runE_ bpGhc

ghc_pkg :: [Text] -> B Text
ghc_pkg      = runE  bpGhcPkg

ghcjs_pkg :: [Text] -> B Text
ghcjs_pkg = run_inplace "ghcjs-pkg"

ghcjs_pkg_ :: [Text] -> B ()
ghcjs_pkg_   = run_inplace_ "ghcjs-pkg"

haddock_ :: [Text] -> B ()
haddock_     = run_inplace_ "haddock"

cabal_ :: [Text] -> B ()
cabal_       = runE_ bpCabal

npm_ :: [Text] -> B ()
npm_ args = do
  nodePgm <- view (bePrograms . bpNode)
  case nodePgm ^. pgmLoc of
    Nothing -> failWith "nodejs program not configured when trying to run npm"
    Just loc ->
      if isWindows
      then do
        sysRoot <- fromMaybe "C:\\WINDOWS" <$> liftIO (Utils.getEnvMay "SYSTEMROOT")
        let cmdLoc = sysRoot </> "system32" </> "cmd" <.> "exe"
            npmLoc = FP.takeDirectory loc </> "npm" <.> "cmd"
        run''_ cmdLoc ("/C" : T.pack npmLoc : args)
      else do
        let npmLoc = FP.takeDirectory loc </> "npm"
        run''_ npmLoc args

runE :: ((Program -> Const Program Program)
     -> BootPrograms -> Const Program BootPrograms)
     -> [Text] -> B Text
runE  g a = view (bePrograms . g) >>= \p -> run p "" a

runE_ :: ((Program -> Const Program Program)
      -> BootPrograms -> Const Program BootPrograms)
      -> [Text] -> B ()
runE_ g a = view (bePrograms . g) >>= \p -> run_ p a

{- | stage 1 cabal install: boot mode, hand off to GHC if GHCJS
     cannot yet compile it -}
cabalStage1 :: [Text] -> B ()
cabalStage1 pkgs = sub $ do
  ghc <- requirePgmLoc =<< view (bePrograms . bpGhc)
  liftIO $ do
    setEnv "GHCJS_BOOTING" "1"
    setEnv "GHCJS_BOOTING_STAGE1" "1"
    setEnv "GHCJS_WITH_GHC" ghc
  let configureOpts = []
  globalFlags <- cabalGlobalFlags
  flags <- cabalInstallFlags (length pkgs == 1)
  cmd <- cabalInstallCommand
  let args = globalFlags ++ (cmd : pkgs) ++
             [ "--allow-boot-library-installs"
             ] ++ map ("--configure-option="<>) configureOpts ++ flags
  cabal_ args

-- | regular cabal install for GHCJS
cabalInstall :: [Package] -> B ()
cabalInstall [] = do
  msg info "cabal-install: no packages, nothing to do"
  return ()
cabalInstall pkgs = do
  globalFlags <- cabalGlobalFlags
  flags <- cabalInstallFlags (length pkgs == 1)
  cmd <- cabalInstallCommand
  liftIO $ setEnv "GHCJS_BOOTING" "1"
  let args = globalFlags ++ cmd : pkgs ++ flags
  cabal_ args

cabalInstallCommand :: B Text
cabalInstallCommand = pure "v1-install"

cabalGlobalFlags :: B [Text]
cabalGlobalFlags = do
  instDir  <- libDir <$> view beLocations
  return ["--config-file"
         ,T.pack (instDir </> "cabalBootConfig")
         ,"--ignore-sandbox"
         ]

cabalInstallFlags :: Bool -> B [Text]
cabalInstallFlags parmakeGhcjs = do
  debug    <- view (beSettings . bsDebug)
  v        <- view (beSettings . bsVerbosity)
  j        <- view (beSettings . bsJobs)
  instDir  <- libDir <$> view beLocations
  prof     <- view (beSettings . bsProf)
  haddock  <- view (beSettings . bsHaddock)
  locs     <- view beLocations
  let binDir        = (locs ^. blGhcjsTopDir) </> "bin"
      privateBinDir = instDir </> "bin"
  return $ [ "--global"
           , "--ghcjs"
           , "--one-shot"
           , "--avoid-reinstalls"
           , "--builddir",      "dist"
           , "--with-compiler", (T.pack $ binDir </> "ghcjs")
           , "--with-hc-pkg",   (T.pack $ binDir </> "ghcjs-pkg")
           , "--with-haddock",  (T.pack $ binDir </> "haddock")
           , "--with-gcc",      (T.pack $ privateBinDir </> "emcc")
           , "--prefix",        T.pack (locs ^. blGhcjsTopDir)
           , bool haddock "--enable-documentation" "--disable-documentation"
           , "--configure-option", "--host=wasm32-unknown-none"
           , "--ghcjs-options=-fwrite-ide-info"
           , "--haddock-html"
           , "--haddock-hoogle"
           , "--haddock-hyperlink-source"
           , "--enable-debug-info"
           , "--disable-library-stripping"
           , "--disable-executable-stripping"
           , "-fghci"
           , bool prof
                  "--enable-profiling"
                  "--disable-profiling"
           ] ++
           -- don't slow down Windows builds too much,
           -- on other platforms we get this more
           -- or less for free, thanks to dynamic-too
           bool isWindows [] ["--enable-shared"] ++
           catMaybes [ (((bool parmakeGhcjs
                               "--ghcjs-options=-j"
                               "-j")<>) . showT) <$> j
                     , bj debug "--ghcjs-options=-debug"
                     , bj (v > info) "-v2"
                     ]

stagePackages :: Getter BootStages Stage -> B [Package]
stagePackages l = view (beStages . l)

failWith :: MonadIO m => Text -> m a
failWith err = liftIO (T.putStrLn ("fatal: " <> err) >> exitFailure)

{- |initialize our boot environment by reading the configuration files,
    finding all programs -}
initBootEnv :: BootSettings -> IO BootEnv
initBootEnv bs = do
  BootConfigFile stgs pgms1 <- readBootConfigFile bs
  pgms2 <- configureBootPrograms bs pgms1
  locs  <- configureBootLocations bs pgms2
  -- installWrappers ((locs ^. blGhcjsTopDir) </> "bin") locs pgms2
  return (BootEnv bs locs pgms2 stgs)

-- the platform triple that GHCJS reports
-- this is in practice mostly used for the toolchain
ghcjsTriple :: Text
ghcjsTriple = "wasm32-unknown-none"

-- this assumes that the ghcjs-boot version is correct. perhaps we should query the ghcjs executable to verify?
ghcjsVersion :: BootPrograms -> IO Text
ghcjsVersion _ = pure (T.pack Info.getCompilerVersion)

libSubDir :: FilePath
libSubDir = "lib"

libDir :: BootLocations -> FilePath
libDir locs = (locs ^. blGhcjsTopDir) </> libSubDir

data WrapperEnv = WrapperEnv { weTopDir     :: Text
                             , weBinDir     :: Text
                             , weLibexecDir :: Text
                             , weVersion    :: Text
                             , weGhcVersion :: Text
                             , weEmsdk      :: Text
                             } deriving (Show)
--

copyWrapper :: Text
            -> Maybe Text
            -> WrapperEnv
            -> FilePath
            -> FilePath
            -> FilePath
            -> IO ()
copyWrapper = if isWindows then copyWrapperW else copyWrapperU

{- |
     on Windows we can't run shell scripts, so we don't install wrappers
     just copy program.exe to program-{version}-{ghcversion}.exe

     the programs read a program-{version}-{ghcversion}.exe.options file from
     the same directory, which contains the command line arguments to prepend

     installation does not overwrite existing .options files
 -}

copyWrapperW :: Text
             -> Maybe Text
             -> WrapperEnv
             -> FilePath
             -> FilePath
             -> FilePath
             -> IO ()
copyWrapperW programName mbVer env libexecDir wrapperSrcDir binDir = do
    createDirectoryIfMissing False binDir
    Cabal.installExecutableFile verbosity srcExe tgtExe
    whenVer (Cabal.installExecutableFile verbosity srcExe tgtExeVersioned)
    options <- replacePlaceholders env <$> T.readFile (wrapperSrcDir </> T.unpack programName <.> "exe" <.> "options")
    createDirectoryIfMissing False binDir
    Cabal.withTempFile binDir "ghcjs-options-XXXXXX.tmp" $ \tmp h -> do
        T.hPutStr h options
        hClose h
        Cabal.installOrdinaryFile verbosity tmp tgtOptions
        whenVer (Cabal.installOrdinaryFile verbosity tmp tgtOptionsVersioned)
  where
    verbosity           = Cabal.verbose
    whenVer x           = when (isJust mbVer) x
    srcExe              = libexecDir </> "wrapper" <.> "exe"

    tgtPrefix           = binDir </> T.unpack programName
    tgtPrefixVersioned  = binDir </> T.unpack (programName <> "-" <> ver)

    tgtExe              = tgtPrefix <.> "exe"
    tgtOptions          = tgtExe <.> "options"
    tgtExeVersioned     = tgtPrefixVersioned <.> "exe"
    tgtOptionsVersioned = tgtExeVersioned <.> "options"
    ver = fromMaybe "0.0.0" mbVer

{- |
     on non-Windows we copy shell scripts that pass the -B flag to ghcjs,
     ghcjs-pkg etc

     installation updates the symlink, but does not overwrite the wrapper
     scripts if they already exist

     if no wrapper is required, we simply symlink to the executable in the
     libexec directory
 -}

copyWrapperU :: Text
             -> Maybe Text
             -> WrapperEnv
             -> FilePath
             -> FilePath
             -> FilePath
             -> IO ()
copyWrapperU programName mbVer env libexecDir wrapperSrcDir binDir = do
  putStrLn ("installing wrapper for: " ++ show (programName, mbVer, libexecDir, wrapperSrcDir, binDir))
  createDirectoryIfMissing False binDir
  wrapperScript <- replacePlaceholders env <$> T.readFile (wrapperSrcDir </> T.unpack programName <.> "sh")
  Cabal.withTempFile binDir "ghcjs-wrapper-XXXXXX.tmp" $
      (\tmp h -> do T.hPutStr h wrapperScript
                    hClose h
                    Cabal.installExecutableFile Cabal.normal tmp (binDir </> tgt))
  when (isJust mbVer) (linkFileU Cabal.normal binDir tgt tgtPlain)
  where
    tgtPlain = T.unpack programName
    tgt      = maybe tgtPlain
                     (\ver -> T.unpack (programName <> "-" <> ver))
                     mbVer

{- |
     create a symlink, overwriting the target. unix only.

     it looks like the Cabal library does not have this functionality,
     and since we shouldn't use system-specific libraries here,
     we use a shell command instead.

     the symlink is relative if both files are in the same directory
  -}
linkFileU :: Cabal.Verbosity -> FilePath -> FilePath -> FilePath -> IO ()
linkFileU _v workingDir src dest = do
  -- putStrLn ("link file: " ++ show (workingDir, src, dest))
  let ignoreDoesNotExist :: IOError -> IO ()
      ignoreDoesNotExist e | isDoesNotExistError e = return ()
                           | otherwise             = Ex.throw e
  removeFile (workingDir </> dest) `Ex.catch` ignoreDoesNotExist
  wd <- getCurrentDirectory
  setCurrentDirectory workingDir
  createFileLink src dest
  setCurrentDirectory wd

-- | replace placeholders in a wrapper script or options file
replacePlaceholders :: WrapperEnv -> Text -> Text
replacePlaceholders env xs =
  foldl (\ys (p,r) -> T.replace p (r env) ys) xs
    [ ("{topdir}",     weTopDir)
    , ("{bindir}",     weBinDir)
    , ("{libexecdir}", weLibexecDir)
    , ("{version}",    weVersion)
    , ("{ghcversion}", weGhcVersion)
    , ("{emsdk}",      weEmsdk)
    ]

{- | try to find the emsdk directory
       1. check --with-emsdk option
       2. check EMSDK environment variable
       3. find emcc executable in PATH
 -}
configureEmsdkDir :: BootSettings
                  -> IO FilePath
configureEmsdkDir bs
  | Just dir <- bs ^. bsWithEmsdk = pure (T.unpack dir)
  | otherwise = do
    e <- lookupEnv "EMSDK"
    case e of
      Just edir -> pure edir
      _         -> do
        mbEmccExe <- findExecutable "emcc"
        case mbEmccExe of
          Just emccExe ->
            {- if we found the `emcc' executable, we just assume
               that it's two directories deeper than the emsdk root
               path, e.g. fastcomp/bin, fastcomp/emscripten, upstream/bin

               perhaps we should change this to check for the emsdk python
               script itself.
             -}
            let emccDirParts = FP.splitPath (FP.takeDirectory emccExe)
            in  pure (concat . reverse . drop 2 . reverse $ emccDirParts)
          _ -> notFound
  where
    notFound = failWith "emscripten emsdk not found (use --with-emsdk)"

-- | configure the locations
configureBootLocations :: BootSettings
                       -> BootPrograms
                       -> IO BootLocations
configureBootLocations bs _pgms = do
  -- take the location of the ghcjs-boot program and configure the
  -- installation directory relative to that
  bootPath <- getExecutablePath
  ghcjsTopDir <- canonicalizePath (FP.dropFileName bootPath </> "..")
  let ghcjsLibDir = ghcjsTopDir </> libSubDir
      globalDB = ghcjsLibDir </> "package.conf.d"

  -- the source directory is specified by the user
  sourceDir <- bootSourceDir bs
  buildDir <- prepareBuildDir sourceDir ghcjsLibDir
  emsdkDir <- configureEmsdkDir bs
  pure $ BootLocations sourceDir
                        buildDir
                        ghcjsTopDir
                        globalDB
                        emsdkDir

prepareBuildDir :: Prelude.FilePath
                -> Prelude.FilePath
                -> IO Prelude.FilePath
prepareBuildDir srcDir ghcjsLibDir = do
  e <- doesFileExist srcDir
  if e then do let bootDir = ghcjsLibDir FP.</> "boot"
               bootExists <- doesDirectoryExist  bootDir
               when bootExists (removeDirectoryRecursive bootDir)
               unpackTar False False ghcjsLibDir srcDir
               pure bootDir
       else do
         d <- doesDirectoryExist srcDir
         if d then do
                e <- doesFileExist (srcDir FP.</> "boot" FP.<.> "yaml")
                if e then pure srcDir else err
              else err
   where
     err = failWith "source location must contain boot.yaml or be a tar file"

-- |build the program configuration and do some sanity checks
configureBootPrograms :: BootSettings    -- ^command line settings
                      -> BootPrograms    -- ^default programs from config file
                      -> IO BootPrograms -- ^configured programs
configureBootPrograms bs pgms0 = do
  -- first replace all defaults with the overrides from the command line
  let
      r l   = maybe id (pgmSearch .~) (bs ^. l)
      tp    = template :: Traversal' BootPrograms Program
      pgms2 = pgms0 & bpCabal    %~ r bsWithCabal
                    & bpNode     %~ r bsWithNode
  -- resolve all programs
  pgms3 <- mapMOf tp (resolveProgram bs)
             =<< mapMOf tp (resolveProgram bs) pgms2
  traverseOf_ tp (reportProgramLocation bs) pgms3
  return pgms3

resolveProgram :: BootSettings
               -> Program
               -> IO Program
resolveProgram _bs pgm = do
  let search' = pgm ^. pgmSearch . to T.unpack
  absSearch <- (</> search') <$> getCurrentDirectory
  let searchPaths = catMaybes [ Just search'
                              , bj (isRelative search' &&
                                    length (splitDirectories search') > 1)
                                   absSearch
                              ]
  fmap catMaybes (mapM findExecutable searchPaths) >>= \case
      (p':_) -> (\cp -> pgm & pgmLoc ?~ cp) <$> return p'
      _      -> return (pgm & pgmLoc .~ Nothing)

-- | report location of a configured program
reportProgramLocation :: BootSettings -> Program -> IO ()
reportProgramLocation bs p
  | Just l <- p ^. pgmLoc = msg' bs info $ "program " <>
                                           p ^. pgmName <>
                                           " found at " <>
                                           T.pack l
  | otherwise             = msg' bs info $ "program " <>
                                           p ^. pgmName <>
                                           " NOT found, searched for " <>
                                           p ^. pgmSearch

-- | read the boot configuration yaml file
readBootConfigFile :: BootSettings -> IO BootConfigFile
readBootConfigFile bs = do
  bf <- bootConfigFile bs
  case Yaml.decodeEither' bf of
    Left err  -> failWith $
      "error parsing boot.yaml configuration file\n" <>
      T.pack (show err)
    Right bss -> return bss

printBootEnvSummary :: Bool -> BootEnv -> IO ()
printBootEnvSummary after be = do
  section "Boot libraries installation for GHCJS" $ do
    bootLoc  <- getExecutablePath
    bootMod  <- getModificationTime bootLoc
    bootSrc  <- bootSourceDir (be ^. beSettings)
    curDir   <- getCurrentDirectory
    p $ bool after
          ["ghcjs-boot has installed the libraries and runtime system for GHCJS"]
          ["ghcjs-boot will install the libraries and runtime system for GHCJS"]
    h "boot program"
    t "rl" [["ghcjs-boot program version", Info.getCompilerVersion]
           ,["file location", bootLoc]
           ,["last modified", show bootMod],[]
           ,["boot source location", bootSrc]
           ,["current directory", curDir]
           ]
    h "boot configuration"
    t "rl" [["installation directory", path $ beLocations . blGhcjsTopDir]
           ,["global package DB", path $ beLocations . blGlobalDB]
           ,["GHC version", ver "<unknown>" bpGhc]
           ,["location", loc bpGhc]
           ,["cabal-install version", ver "<unknown>" bpCabal]
           ,["location", loc bpCabal],[]
           ]
    h "packages"
    p ["stage 1a"] >> l (stg bstStage1a)
    p ["ghcjs-prim:  " ++ be ^. beStages . bstGhcjsPrim . to str]
    p ["stage 1b"] >> l (stg bstStage1b)
    p ["Cabal:  " ++ be ^. beStages . bstCabal . to str]
  section "Configured programs" $
    t "hlll" $ ["program", "version", "location"] :
      be ^.. bePrograms .
             (template :: Traversal' BootPrograms Program) .
             to pgm
  when after $ do
     p ["GHCJS has been booted."]
     p ["You can now add the binaries directory to your PATH, or create"]
     p ["symbolic to the ghcjs and ghcjs-pkg executables"]
     p ["the binaries have been installed in"]
     p [path $ beLocations . blGhcjsTopDir . to (</> "bin")]

  where
    stg s       = be ^.. beStages . s . traverse . to str
    h xs        = b >>
                  mapM_ (putStrLn . indent 2)
                        [xs, replicate (length xs) '-'] >> b
    p xs        = mapM_ (putStrLn . indent 3) xs >> b
    l xs        = mapM_ (putStrLn . indent 3 . ("- "++)) xs >> b
    t :: String -> [[String]] -> IO ()
    t aln xxs   = let colWidths = map (foldl' (\m xs -> max m (length xs)) 0)
                                      (transpose xxs)
                      (colAlign,hdr) = case aln of
                        ('h':a) -> (a, True)
                        a       -> (a, False)
                      cell w a xs = let pad = sp (w - length xs)
                                    in if a == 'r' then pad ++ xs
                                                   else xs ++ pad
                      cols xs   = sp 3 ++ intercalate (sp 3) xs
                      row xs    = cols (zipWith3 cell colWidths colAlign xs)
                  in case (xxs, hdr) of
                      (x:ys, True) -> putStrLn (row x) >>
                                      putStrLn (cols $ map sp colWidths) >>
                                      mapM_ (putStrLn . row) ys >> b
                      _            -> mapM_ (putStrLn . row) xxs
    b           = putStrLn ""
    sp n        = replicate n ' '
    indent n xs = sp n ++ xs
    sep         = putStrLn (replicate 75 '=')
    section :: String -> IO () -> IO ()
    section t a = b >> b >> sep >> b >> p [t] >> sep >> b >> a >> b
    ver d l     = be ^. bePrograms . l . pgmVersion . to (maybe d T.unpack)
    loc l       = be ^. bePrograms . l . pgmLocString
    path l      = be ^. l
    str         = T.unpack
    pgm x       = [ x ^. pgmName . to str
                  , maybe "-" T.unpack (x ^. pgmVersion)
                  , x ^. pgmLocString
                  ]

-- | boot.yaml
bootConfigFile :: BootSettings -> IO B.ByteString
bootConfigFile bs = do
  sourceDir <- bootSourceDir bs
  let sourceDir' = sourceDir
  e <- doesFileExist sourceDir'
  if e then do -- it's a tar file
         entries <- Tar.read . BL.fromStrict <$>
           B.readFile sourceDir'
         pure (BL.toStrict $ getBootYaml entries)
       else B.readFile (sourceDir </> "boot" <.> "yaml")
  where
    getBootYaml (Tar.Next e es)
      | Tar.entryPath e == "boot/boot.yaml"
      , Tar.NormalFile contents _size <- Tar.entryContent e = contents
      | otherwise = getBootYaml es
    getBootYaml Tar.Done     = error "boot/boot.yaml file not found in archive"
    getBootYaml (Tar.Fail e) = error $ "error reading boot archive: " ++ show e

bootSourceDir :: BootSettings -> IO FilePath
bootSourceDir bs
  | Just dd <- bs ^. bsSourceDir = makeAbsolute (T.unpack dd)
  | otherwise                    = do
      workingDirectory <- getCurrentDirectory
      let configFile =  workingDirectory </> "boot" <.> "yaml"
      configInCurrentDir <- doesFileExist configFile
      if configInCurrentDir
        then pure workingDirectory
        else do
          dataDir <- Info.ghcjsBootDefaultDataDir
          let bootArchive = dataDir </> "boot" <.> "tar"
          bootArchiveExists <- doesFileExist bootArchive
          pure $ if bootArchiveExists then bootArchive else workingDirectory

-- | our boot monad, we wrap around shelly but with a config environment
--   shelly commands are wrapped with logging
type B = ReaderT BootEnv IO

msg' :: BootSettings -> Verbosity -> Text -> IO ()
msg' bs v t = when (bs ^. bsVerbosity >= v) (T.putStrLn t)

msg :: Verbosity -> Text -> B ()
msg v t =
  view beSettings >>= \s -> when (s ^. bsVerbosity >= v) $ lift (T.putStrLn t)

-- | log a message printing the current directory
msgD :: Verbosity -> Text -> B ()
msgD v t = pwd >>= \p -> msg v (T.pack p <> "$ " <> t)

msgD' :: BootSettings -> Verbosity -> Text -> IO ()
msgD' bs v t = getCurrentDirectory >>= \p -> msg' bs v (T.pack p <> "$ " <> t)

{-
  lifted versions of the shelly operations we need. everything that
  makes externally visible changes is logged at the info (-v2)
  verbosity level.

  internal changes and file reads are logged at trace (-v3) level.
 -}

mkdir_p :: FilePath -> B ()
mkdir_p p      = msgD info ("mkdir_p " <> T.pack p) >> lift (createDirectoryIfMissing True p)

cp :: FilePath -> FilePath -> B ()
cp f t         = msgD info ("cp "      <> T.pack f <> " -> " <> T.pack t) >>
                 lift (copyFile f t)

cp_r :: FilePath -> FilePath -> B ()
cp_r src tgt = do
  liftIO (createDirectoryIfMissing True tgt)
  mapM_ f =<< liftIO (listDirectory src)
  where
    f entry = do
      let se = src </> entry
          te = tgt </> entry
      isDir <- liftIO (doesDirectoryExist se)
      if isDir then cp_r se te else liftIO (copyFile se te)

rm_rf :: FilePath -> B ()
rm_rf p        = msgD info ("rm_rf "   <> T.pack p) >> lift (removePathForcibly p)

cd :: FilePath -> B ()
cd p = do
  msgD trace ("cd " <> T.pack p)
  cur <- lift getCurrentDirectory
  lift (setCurrentDirectory (cur </> p))


sub :: B a -> B a
sub x = do
  workingDir <- liftIO getCurrentDirectory
  r <- x
  liftIO (setCurrentDirectory workingDir)
  pure r

run :: Program -> Text -> [Text] -> B Text
run p stdin xs
  = traceDir >>
    msgD info (traceRun p xs) >>
                 requirePgmLoc p >>=
                 \loc ->
                   let pc = proc loc (map T.unpack $ p ^. pgmArgs ++ xs)
                   in T.pack <$> lift (readCreateProcess pc (T.unpack stdin))

run_ :: Program -> [Text] -> B ()
run_ p xs = do
  traceDir
  msgD info (traceRun p xs)
  requirePgmLoc p >>= \loc -> do
    let pc = proc loc (map T.unpack $ p ^. pgmArgs ++ xs)
    (_, _, _, ph) <- liftIO (createProcess pc)
    ex <- liftIO (waitForProcess ph)
    when (ex /= ExitSuccess)
       (failWith $ "program " <>
                   p ^. pgmLocText <>
                   " returned a nonzero exit code")

run'' :: FilePath -> Text -> [Text] -> B Text
run'' loc stdin xs = do
  msgD info (traceRun' loc xs)
  let pc = proc loc (map T.unpack xs)
  T.pack <$> lift (readCreateProcess pc (T.unpack stdin))

run''_ :: FilePath -> [Text] -> B ()
run''_ loc xs = do
  msgD info (traceRun' loc xs)
  let pc = proc loc (map T.unpack xs)
  (_, _, _, ph) <- liftIO (createProcess pc)
  ex <- liftIO (waitForProcess ph)
  when (ex /= ExitSuccess)
      (failWith $ "program " <>
                  T.pack loc <>
                  " returned a nonzero exit code")

readBinary :: FilePath -> B B.ByteString
readBinary path = do
  msgD trace ("reading " <> T.pack path)
  liftIO (B.readFile path)

pwd :: B FilePath
pwd = lift getCurrentDirectory


findWhen :: (FilePath -> B Bool) -> FilePath -> B [FilePath]
findWhen pred path = fmap concat (mapM f =<< (liftIO $ listDirectory path))
  where
    f :: FilePath -> B [FilePath]
    f entry = do
      let pe = path </> entry
      d <- liftIO (doesDirectoryExist pe)
      if d then findWhen pred pe
           else do
             pp <- pred pe
             if pp then pure [pe] else pure []

catchAny :: B b -> (Ex.SomeException -> B b) -> B b
catchAny a h = ask >>=
               \e -> lift (Ex.catch (runReaderT a e)
                                    (\ex -> runReaderT (h ex) e))

catchAny_ :: B b -> B b -> B b
catchAny_ a h  = catchAny a (\_ -> h)

traceDir :: B ()
traceDir = ask >>= \e -> lift (traceDir' (e ^. beSettings))

traceDir' :: BootSettings -> IO ()
traceDir' bs = do
  cur <- getCurrentDirectory
  msgD' bs info ("current directory: " <> T.pack cur)


traceRun :: Program -> [Text] -> Text
traceRun p xs = "[" <>
                p ^. pgmName <>
                "]: " <>
                p ^. pgmLocText <>
                " " <>
                T.intercalate " " (map (showT . T.unpack) xs)


traceRun' :: FilePath -> [Text] -> Text
traceRun' p xs = {- "[" <>
                  T.pack p <>
                "]: " <> -}
                T.pack p <>
                " " <>
                T.intercalate " " (map (showT . T.unpack) xs)

requirePgmLoc :: Program -> B FilePath
requirePgmLoc p
  | Just loc <- p ^. pgmLoc = return loc
  | otherwise = do
        {- search in original path, where we configured the programs.
           the shelly path might be local -}
        path <- fromMaybe "" <$> liftIO (Utils.getEnvMay "PATH")
        failWith $ "program " <>
                   p ^. pgmName <>
                   " is required but was not found\n" <>
                   "  name searched for (from boot.yaml or command line): " <>
                   p ^. pgmSearch <>
                   "\n" <>
                   "  searched in PATH:\n" <>
                   T.pack path

{-
run' :: BootSettings -> Program -> [Text] -> IO Text
run' bs p xs = do
  traceDir' bs
  msgD' bs info (traceRun p xs)
  (e, out, _err) <- readProcessWithExitCode (p ^. pgmLocString)
                                            (map T.unpack xs)
                                            ""
  when (e /= ExitSuccess)
       (failWith $ "program " <>
                   p ^. pgmLocText <>
                   " returned a nonzero exit code")
  return (T.pack out)
-}

pgmLocText :: Getter Program Text
pgmLocText = pgmLoc . to (maybe "<not found>" T.pack)

pgmLocString :: Getter Program String
pgmLocString = pgmLocText . to T.unpack

showT :: Show a => a -> Text
showT = T.pack . show

bool :: Bool -> a -> a -> a
bool b t f = if b then t else f

bj :: Bool -> a -> Maybe a
bj b v = if b then Just v else Nothing

infixl 2 <^>
(<^>) :: MonadReader s m
      => (a -> m b)
      -> Getting a s a
      -> m b
(<^>) m l = m =<< view l
