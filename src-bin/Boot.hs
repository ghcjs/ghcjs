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
module Main where

import           Prelude
  hiding (FilePath, elem, mapM, mapM_, any, all, concat, concatMap)

import qualified Prelude

import qualified Distribution.Simple.Utils       as Cabal
import qualified Distribution.Verbosity          as Cabal


import qualified Codec.Archive.Tar               as Tar
import qualified Codec.Archive.Tar.Entry         as Tar

import           Control.Applicative
import qualified Control.Exception               as Ex
import           Control.Lens                    hiding ((<.>))
import           Control.Monad
  (void, when, unless, join)
import           Control.Monad.Reader
  (MonadReader, ReaderT(..), MonadIO, ask, local, lift, liftIO)

import qualified Data.Aeson                      as Aeson
import qualified Data.ByteString                 as B
import qualified Data.ByteString.Lazy            as BL
import           Data.Char
import           Data.Data
import           Data.Data.Lens
import           Data.Foldable
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (intercalate, transpose)
import           Data.Maybe
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Data.Time.Clock
import           Data.Traversable
import qualified Data.Vector                     as V
-- import qualified Data.Version                    as Version
import           Data.Yaml                       ((.:))
import qualified Data.Yaml                       as Yaml
import           Filesystem
  (getWorkingDirectory, getModified, getSize)
import           Filesystem.Path
  hiding ((<.>), (</>), null, concat)
import           Filesystem.Path.CurrentOS       (encodeString)

import           GHC.IO.Encoding
  (setLocaleEncoding, setForeignEncoding, utf8)

import           Options.Applicative             hiding (info)
import qualified Options.Applicative             as O

import           System.Directory
import           System.Environment              (lookupEnv)
import           System.Environment.Executable   (getExecutablePath)
import           System.Exit
  (exitSuccess, exitFailure, ExitCode(..))
import qualified System.FilePath                 as FP

import           System.IO
  (hPutStrLn, hSetBuffering, stderr, stdout, BufferMode(..), hClose)
import           System.PosixCompat.Files        (setFileMode)
import           System.Process                  (readProcessWithExitCode)

import           Shelly                          ((<.>), fromText)
import qualified Shelly                          as Sh

import           Compiler.GhcjsProgram           (printVersion)
import qualified Compiler.Info                   as Info
import qualified Compiler.Utils                  as Utils
import           Compiler.Settings               (NodeSettings(..))

import qualified Paths_ghcjs                     as Paths

import           System.IO.Error       (IOError, isDoesNotExistError)
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
  , _bsLibexecDir :: Maybe Text -- ^ location of GHCJS executables
  , _bsGhcjsLibdir  :: Maybe Text -- ^ location to install GHCJS boot libraries
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
  , _blLibexecDir  :: FilePath
  , _blEmsdk       :: FilePath
  } deriving (Data, Typeable)

data Program a = Program
  { _pgmName    :: Text           -- ^program name for messages
  , _pgmSearch  :: Text           {- ^name searched for when configuring the
                                      program (from command line or config file)
                                   -}
  , _pgmVersion :: Maybe Text     -- ^version if known
  , _pgmLoc     :: Maybe FilePath -- ^absolute path to the program
  , _pgmArgs    :: [Text]         -- ^extra arguments to pass to the program
  } deriving (Data, Typeable)

data Required = Required deriving (Data, Typeable)
data Optional = Optional deriving (Data, Typeable)

class MaybeRequired a where isRequired :: a -> Bool
instance MaybeRequired (Program Optional) where isRequired = const False
instance MaybeRequired (Program Required) where isRequired = const True

-- | configured programs, fail early if any of the required programs is missing
data BootPrograms = BootPrograms { _bpGhc        :: Program Required
                                 , _bpGhcPkg     :: Program Required
                                 , _bpCabal      :: Program Required
                                 , _bpNode       :: Program Required
                                 } deriving (Data, Typeable)

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

-- |all packages from all stages that can be built on this machine
allPackages :: B [Package]
allPackages = p <$> view beStages
  where
    p s = [s ^. bstGhcjsPrim, s ^. bstCabal, s ^. bstGhcPrim] ++
          (s ^. bstStage1a) ++ (s ^. bstStage1b)

main :: IO ()
main = do
    settings <- execParser optParser'
    when (settings ^. bsShowVersion) (printVersion >> exitSuccess)
    hSetBuffering stdout LineBuffering
    setLocaleEncoding utf8
    setForeignEncoding utf8
    env <- initBootEnv settings
    printBootEnvSummary False env
    r <- Sh.shelly $ runReaderT ((actions >> pure Nothing)
                        `catchAny` (pure . Just)) env
    maybe exitSuccess Ex.throwIO r
  where
    actions :: B ()
    actions = verbosely . tracing False $ do
      e <- ask
      setenv "GHC_CHARENC" "UTF-8"
      cleanCache
      prepareLibDir
      let base = libDir (e ^. beLocations)
      setenv "CFLAGS" $ "-I" <> toTextI (base </> "include")
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
      withArgs :: Text -> Yaml.Value -> Yaml.Parser (Program a)
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


-- convert C:\x\y to /c/x/y (only on Windows)
msysPath :: FilePath -> Text
msysPath p
  | isWindows =
      let p' = toTextI p
          backToForward '\\' = '/'
          backToForward x    = x
          isRel = "." `T.isPrefixOf` p' -- fixme
      in bool isRel "" "/" <> T.map backToForward (T.filter (/=':') p')
  | otherwise = toTextI p

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
          (long "libexecdir" <>
           metavar "DIR" <>
           help "libexec dir with GHCJS programs (this must contain the ghcjs and ghcjs-pkg executables)")
    <*> (optional . fmap T.pack . strOption)
          (long "with-ghcjs-libdir" <>
           metavar "DIR" <>
           help "directory to install GHCJS boot libraries")
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
  -- traverseOf_ _Just initUser <^> beLocations . blUserDBDir
  where
    {-initUser dir = do
      rm_f (dir </> "package.conf")
      initDB "--user" (dir </> "package.conf.d")-}
    initDB dbName db = do
      rm_rf db -- >> mkdir_p db
      ghcjs_pkg_ ["init", toTextI db] `catchAny_` return ()
      ghcjs_pkg_ ["recache", dbName]

cleanCache :: B ()
cleanCache =
  liftIO Info.getUserCacheDir >>= \case
    Just p -> rm_rf (fromString p) `catchAny_` return ()
    Nothing -> return ()

prepareLibDir :: B ()
prepareLibDir = subBuild $ do
  msg info "preparing GHCJS library directory"
  locs <- view beLocations
  pgms <- view bePrograms
  ver <- liftIO $ ghcjsVersion pgms
  let globalDB = locs ^. blGlobalDB
      ghcjsLib = libDir locs
      ghcjsTop = locs ^. blGhcjsTopDir
      libexecDir  = locs ^. blLibexecDir
      rtsConfFile   = "rts.conf"
      wrapperSrcDir = (locs ^. blSourceDir) </> "input" </> "wrappers"
      wenv = WrapperEnv { weTopDir     = toTextI (libDir locs)
                        , weBinDir     = toTextI ((locs ^. blGhcjsTopDir) </> "bin")
                        , weLibexecDir = toTextI (libDir locs </> "bin")
                        , weVersion    = ver
                        , weGhcVersion = ver
                        , weEmsdk      = toTextI (locs ^. blEmsdk)
                        }
  rtsConf <- replacePlaceholders wenv <$>
             readfile ("input" </> "rts" <.> "conf")
  mkdir_p (ghcjsLib </> "bin")
  -- mkdir_p globalDB

  msg info "copying GHCJS executables"

  -- copy the executables from the package to the libraries directory
  -- (the executables in the package have been prefixed with `private-ghcjs-`
  -- as a workaround for cabal-install incorrectly generating symlinks for
  -- private executables)
  let cpExe name = do
        let srcName = libexecDir </> exe (fromText $ "private-ghcjs-" <> name)
            tgtName = ghcjsLib </> "bin" </> exe (fromText name)
        cp srcName tgtName
        liftIO . Cabal.setFileExecutable . toStringI $ tgtName

  mapM_ cpExe [ "unlit"
              , "run"
              , "ghcjs"
              , "ghcjs-pkg"
              , "haddock"
              , "hsc2hs"
              ]

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
  writefile (globalDB </> rtsConfFile) rtsConf
  ghcjs_pkg_ ["recache", "--global", "--no-user-package-db"]
  cp_r "shims" ghcjsLib
  sub $ cd "data" >> cp_r "." ghcjsLib
  writefile (ghcjsLib </> "settings") =<< generateSettings
  -- workaround for hardcoded check of mingw dir in ghc when building on Windows
  when isWindows (mkdir_p $ ghcjsTop </> "mingw")
  prepareNodeJs
  unless isWindows $ do
    let runSh = ghcjsLib </> "run" <.> "sh"
    writefile runSh "#!/bin/sh\nCOMMAND=$1\nshift\n\"$COMMAND\" \"$@\"\n"
    liftIO . Cabal.setFileExecutable . toStringI =<< absPath runSh
  -- make an empty object file for
  subLib $ do
    writefile "empty.c" ""
    ghc_ ["-c", "empty.c"]
  msg info "RTS prepared"

generateSettings :: B Text
generateSettings = do
  emsdk <- view (beLocations . blEmsdk)
  -- do we also need emar, emranlib?
  let opts :: [(String, String)]
      opts = [("GCC extra via C opts", " -fwrapv -fno-builtin"),
              ("C compiler command", toStringI (emsdk </> "upstream" </> "bin" </> "clang") {-"asmjs-none-gcc"-} {-T.unpack emccProgram-}),
              ("C compiler flags", ""),
              ("C compiler link flags", " "),
              ("C compiler supports -no-pie", "NO"),
              ("Haskell CPP command", toStringI (emsdk </> "upstream" </> "bin" </> "clang")),
              ("Haskell CPP flags","-E -undef -traditional -Wno-invalid-pp-token -Wno-unicode -Wno-trigraphs"),
              ("ld command", "ld"),
              ("ld flags", ""),
              ("ld supports compact unwind", "YES"),
              ("ld supports build-id", "NO"),
              ("ld supports filelist", "YES"),
              ("ld is GNU ld", "NO"),
              ("ar command", toStringI (emsdk </> "upstream" </> "emscripten" </> "emar")),
              ("ar flags", "qcls"),
              ("ar supports at file", "NO"),
              ("ranlib command", toStringI (emsdk </> "upstream" </> "emscripten" </> "emranlib")),
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
  nodeProgram <- view (bePrograms . bpNode . pgmLoc . to (maybe "-" toTextI))
  mbNodePath  <- view (beSettings . bsWithNodePath)
  extraArgs   <- view (beSettings . bsNodeExtraArgs)
  -- If no setting for NODE_PATH is specified, we use the libraries bundled
  -- with the ghcjs-boot submodule. We must run "npm rebuild" to build
  -- any sytem-specific components.
  when (isNothing mbNodePath) $ do
    subLib (mkdir_p "ghcjs-node")
    liftIO $ unpackTar False
                       True
                       (toStringI $ ghcjsLib)
                       (toStringI $ buildDir </> "ghcjs-node.tar")
    subLib' "ghcjs-node" $ npm_ ["rebuild"]
  -- write nodeSettings.json file
  let nodeSettings = NodeSettings
       { nodeProgram         = T.unpack nodeProgram
       , nodePath            = mbNodePath
       , nodeExtraArgs       = maybeToList extraArgs
       , nodeKeepAliveMaxMem = 536870912
       }
  liftIO $ BL.writeFile (T.unpack . toTextI $ ghcjsLib </> "nodeSettings.json")
                        (Aeson.encode $ Aeson.toJSON nodeSettings)

exe :: FilePath -> FilePath
exe = bool isWindows (<.>"exe") id

{-
-- I think we don't need this anymore now that we have a generic wrapper
#ifdef WINDOWS
  -- compile the resources we need for the runner to prevent Windows from
  -- trying to detect programs that require elevated privileges
  ghcjsTop <- view (beLocations . blGhcjsTopDir)
  let windres = Program "windres"
                        "windres"
                        Nothing
                        (Just $ ghcjsTop </>
                                  ".." </>
                                  "mingw" </>
                                  "bin" </>
                                  "windres.exe")
                        []
  subTop $ run_ windres ["runner.rc", "-o", "runner-resources.o"]
#endif
-}
buildDocIndex :: B ()
buildDocIndex = subTop' "doc" $ do
  haddockFiles <- findWhen (return . flip hasExtension "haddock") "."
  haddock_ $ ["--gen-contents"
             ,"--gen-index"
             , "-o"
             , "html"
             , "--title=GHCJS Libraries"
             ] ++
             map (\p -> "--read-interface=../" <>
                        toTextI (directory p) <>
                        "," <>
                        toTextI p)
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
  wips <- readBinary (("wiredinpkgs"::Text) <.> "yaml")
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
     writefile (("wiredinkeys"::Text) <.> "yaml") $
       T.unlines ("# resolved wired-in packages" :
                  map (\(p,k) -> p <> ": " <> k) pkgs')

preparePackage :: Package -> B ()
preparePackage pkg
  | "./" `T.isPrefixOf` pkg || "../" `T.isPrefixOf` pkg = sub $ do
    msg trace ("preparing package " <> pkg)
    cd (fromText pkg)
    rm_rf "dist"
  | otherwise = return ()

fixRtsConf :: Text -> Text -> Text -> Text
fixRtsConf incl lib conf = T.unlines . map fixLine . T.lines $ conf
    where
      fixLine l
        | "library-dirs:" `T.isPrefixOf` l = "library-dirs: " <> lib
        | "include-dirs:" `T.isPrefixOf` l = "include-dirs: " <> incl
        | otherwise                        = l

-- | register fake, empty packages to be able to build packages
--   that depend on Cabal
installFakes :: B ()
installFakes = silently $ do
  installed <- T.words <$> ghc_pkg ["list", "--simple-output"]
  dumped <- T.lines <$> ghc_pkg ["dump"]
  fakes <- view (beStages . bstPretend)
  forM_ fakes $ \pkg ->
    case reverse (filter ((==pkg<>"-") . fst . T.breakOnEnd "-") installed) of
      [] -> failWith ("required package " <> pkg <> " not found in host GHC")
      (x:_) -> do
        let version = T.drop 1 (T.dropWhile (/='-') x)
        case findPkgId dumped pkg version of
          Nothing -> failWith $
            "cannot find package id of " <> pkg <> "-" <> version
          Just pkgId -> do
            globalDB <- view (beLocations . blGlobalDB)
            libDir   <- libDir <$> view beLocations
            pkgAbi <- findPkgAbi pkgId
            let conf = fakeConf libDir libDir pkg version pkgId pkgAbi
            writefile (globalDB </> fromText pkgId <.> "conf") conf
  ghcjs_pkg_ ["recache", "--global", "--no-user-package-db"]

findPkgId:: [Text] -> Text -> Text -> Maybe Text
findPkgId dump pkg version =
  listToMaybe (filter (\i -> i == pkgVer || pkgVer' `T.isPrefixOf` i) ids)
    where
      pkgVer  = pkg <> "-" <> version
      pkgVer' = pkgVer <> "-"
      ids = map (T.dropWhile isSpace . T.drop 3)
                (filter ("id:" `T.isPrefixOf`) dump)

findPkgAbi :: Text -> B Text
findPkgAbi pkgId = do
  dumped <- T.lines <$> ghc_pkg ["field", pkgId, "abi"]
  case mapMaybe (T.stripPrefix "abi:") dumped of
    (x:_) -> return (T.strip x)
    _     -> failWith ("cannot find abi hash of package " <> pkgId)

fakeConf :: FilePath -> FilePath -> Text -> Text -> Text -> Text -> Text
fakeConf incl lib name version pkgId pkgAbi = T.unlines
            [ "name:           " <> name
            , "version:        " <> version
            , "id:             " <> pkgId
            , "key:            " <> pkgId
            , "abi:            " <> pkgAbi
            , "license:        BSD3"
            , "maintainer:     stegeman@gmail.com"
            , "import-dirs:    " <> toTextI incl
            , "include-dirs:   " <> toTextI incl
            , "library-dirs:   " <> toTextI lib
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
subBuild' :: FilePath -> B a -> B a
subBuild' p a = subBuild (cd p >> a)

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

writeBinary :: FilePath -> BL.ByteString -> B ()
writeBinary file bs = do
  msgD info ("writing file " <> toTextI file)
  file' <- absPath file
  liftIO $ BL.writeFile (toStringI file') bs

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
        , preserveSymlinks = do
            createDirectoryIfMissing True (FP.dropFileName tgt)
            -- fileExists <- doesFileExist tgt
            -- dirExists  <- doesDirectoryExist tgt
            -- when fileExists (removeFile tgt)
            -- when dirExists  (removeDirectoryRecursive tgt)
            -- createSymbolicLink (Tar.fromLinkTarget linkTgt) tgt
            pure ()
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
  run'' (ghcjsTop </> "bin" </> fromText pgm) args

run_inplace_ :: Text -> [Text] -> B ()
run_inplace_ pgm args = do
  ghcjsTop <- view (beLocations . blGhcjsTopDir)
  run''_ (ghcjsTop </> "bin" </> fromText pgm) args

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

cabal :: [Text] -> B Text
cabal        = runE  bpCabal

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
        let cmdLoc = fromString sysRoot </> "system32" </> "cmd" <.> "exe"
            npmLoc = fromString (FP.takeDirectory (toStringI loc)) </> "npm" <.> "cmd"
        run''_ cmdLoc ("/C" : toTextI npmLoc : args)
      else do
        let npmLoc = fromString (FP.takeDirectory (toStringI loc)) </> "npm"
        run''_ npmLoc args

runE :: ((Program a -> Const (Program a) (Program a))
     -> BootPrograms -> Const (Program a) BootPrograms)
     -> [Text] -> ReaderT BootEnv Sh.Sh Text
runE  g a = view (bePrograms . g) >>= flip run  a

runE_ :: ((Program a -> Const (Program a) (Program a))
      -> BootPrograms -> Const (Program a) BootPrograms)
      -> [Text] -> ReaderT BootEnv Sh.Sh ()
runE_ g a = view (bePrograms . g) >>= flip run_ a

{- | stage 1 cabal install: boot mode, hand off to GHC if GHCJS
     cannot yet compile it -}
cabalStage1 :: [Text] -> B ()
cabalStage1 pkgs = sub $ do
  ghc <- requirePgmLoc =<< view (bePrograms . bpGhc)
  setenv "GHCJS_BOOTING" "1"
  setenv "GHCJS_BOOTING_STAGE1" "1"
  setenv "GHCJS_WITH_GHC" (toTextI ghc)
  let configureOpts = []
  globalFlags <- cabalGlobalFlags
  flags <- cabalInstallFlags (length pkgs == 1)
  cmd <- cabalInstallCommand
  let args = globalFlags ++ (cmd : pkgs) ++
             [ "--allow-boot-library-installs"
             ] ++ map ("--configure-option="<>) configureOpts ++ flags
  cabal_ args

-- | regular cabal install for GHCJS
cabalInstall :: [Package] -> ReaderT BootEnv Sh.Sh ()
cabalInstall [] = do
  msg info "cabal-install: no packages, nothing to do"
  return ()
cabalInstall pkgs = do
  globalFlags <- cabalGlobalFlags
  flags <- cabalInstallFlags (length pkgs == 1)
  cmd <- cabalInstallCommand
  setenv "GHCJS_BOOTING" "1"
  let args = globalFlags ++ cmd : pkgs ++ flags
  cabal_ args

cabalInstallCommand :: B Text
cabalInstallCommand = pure "v1-install"

cabalGlobalFlags :: B [Text]
cabalGlobalFlags = do
  instDir  <- libDir <$> view beLocations
  return ["--config-file"
         ,toTextI (instDir </> "cabalBootConfig")
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
           , "--with-compiler", (toTextI $ binDir </> "ghcjs")
           , "--with-hc-pkg",   (toTextI $ binDir </> "ghcjs-pkg")
           , "--with-haddock",  (toTextI $ binDir </> "haddock")
           , "--with-gcc",      (toTextI $ privateBinDir </> "emcc")
           , "--prefix",        toTextI (locs ^. blGhcjsTopDir)
           , bool haddock "--enable-documentation" "--disable-documentation"
           , "--configure-option", "--host=wasm32-unknown-none"
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

ignoreExcep :: B () -> B ()
ignoreExcep a = a `catchAny`
  (\e -> msg info $ "ignored exception: " <> showT e)

stagePackages :: Getter BootStages Stage -> B [Package]
stagePackages l = view (beStages . l)

whenM :: Monad m => m Bool -> m () -> m ()
whenM c m = c >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c m = c >>= flip unless m

make :: FilePath   -- ^target, build this file if not exists
     -> [FilePath] {- ^also build if any of these is newer than the
                       target (ignored if they don't exist) -}
     -> B ()       -- ^action to run for building
     -> B ()
make tgt deps m = mtime tgt >>= \case
  Nothing -> m
  Just tm -> whenM (any (>tm) . catMaybes <$> mapM mtime deps) m

failWith :: MonadIO m => Text -> m a
failWith err = liftIO (T.putStrLn ("fatal: " <> err) >> exitFailure)

mtime :: FilePath -> B (Maybe UTCTime)
mtime p = do
  p' <- absPath p
  fmap Just (liftIO $ getModified p') `catchAny_` return Nothing

filesize :: FilePath -> B Integer
filesize file = do
  absFile <- absPath file
  liftIO (getSize absFile)

{- |initialize our boot environment by reading the configuration files,
    finding all programs -}
initBootEnv :: BootSettings -> IO BootEnv
initBootEnv bs = do
  BootConfigFile stgs pgms1 <- readBootConfigFile bs
  pgms2 <- configureBootPrograms bs pgms1
  locs  <- configureBootLocations bs pgms2
  installWrappers ((locs ^. blGhcjsTopDir) </> "bin") locs pgms2
  return (BootEnv bs locs pgms2 stgs)

-- the platform triple that GHCJS reports
-- this is in practice mostly used for the toolchain
ghcjsTriple :: Text
ghcjsTriple = "wasm32-unknown-none"

-- this assumes that the ghcjs-boot version is correct. perhaps we should query the ghcjs executable to verify?
ghcjsVersion :: BootPrograms -> IO Text
ghcjsVersion _ = pure (T.pack Info.getCompilerVersion)

getDefaultTopDir :: BootPrograms -> IO FilePath
getDefaultTopDir pgms = do
  appdir <- getAppUserDataDirectory "ghcjs"
  version <- ghcjsVersion pgms
  return (fromString appdir </>
          fromText (ghcjsTriple <> "-" <> version))

libSubDir :: FilePath
libSubDir = "lib"

libDir :: BootLocations -> FilePath
libDir locs = (locs ^. blGhcjsTopDir) </> libSubDir

installWrappers :: FilePath -> BootLocations -> BootPrograms -> IO ()
installWrappers binDir locs pgms = do
  createDirectoryIfMissing True (toStringI binDir)
  ver <- ghcjsVersion pgms
  mapM_ (installWrapper ver) (wrappedPrograms ver)
  where
    installWrapper ver (pgmName, pgm) =
      (if isWindows then copyWrapperW else copyWrapperU)
        pgmName pgm (wenv ver) libexecDir wrapperSrcDir binDir
    libexecDir = libDir locs </> "bin"
    wrapperSrcDir = (locs ^. blSourceDir) </> "input" </> "wrappers"
    wenv ver = WrapperEnv { weTopDir     = toTextI (libDir locs)
                      , weBinDir     = toTextI ((locs ^. blGhcjsTopDir) </> "bin")
                      , weLibexecDir = toTextI libexecDir
                      , weVersion    = ver
                      , weGhcVersion = ver
                      , weEmsdk      = toTextI (locs ^. blEmsdk)
                      }
    wrappedPrograms ver = [ ("ghcjs",     Just ver)
                          , ("ghcjs-pkg", Just ver)
                          , ("hsc2hs",    Nothing)
                          , ("haddock",   Nothing)]

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
    createDirectoryIfMissing False (toStringI binDir)
    Cabal.installExecutableFile verbosity (toStringI srcExe) (toStringI tgtExe)
    whenVer (Cabal.installExecutableFile verbosity (toStringI srcExe) (toStringI tgtExeVersioned))
    options <- replacePlaceholders env <$> T.readFile (toStringI $ wrapperSrcDir </> fromText programName <.> "exe" <.> "options")
    createDirectoryIfMissing False (toStringI binDir)
    Cabal.withTempFile (toStringI binDir) "ghcjs-options-XXXXXX.tmp" $ \tmp h -> do
        T.hPutStr h options
        hClose h
        Cabal.installOrdinaryFile verbosity tmp (toStringI tgtOptions)
        whenVer (Cabal.installOrdinaryFile verbosity tmp (toStringI tgtOptionsVersioned))
  where
    verbosity           = Cabal.verbose -- Cabal.normal
    whenVer x           = when (isJust mbVer) x
    srcExe              = libexecDir </> "wrapper" <.> "exe"

    tgtPrefix           = binDir </> fromText programName
    tgtPrefixVersioned  = binDir </> fromText (programName <> "-" <> ver)

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
  createDirectoryIfMissing False (toStringI binDir)
  wrapperScript <- replacePlaceholders env <$> T.readFile (toStringI $ wrapperSrcDir </> fromText programName <.> "sh")
  Cabal.withTempFile (toStringI binDir) "ghcjs-wrapper-XXXXXX.tmp" $
      (\tmp h -> do T.hPutStr h wrapperScript
                    hClose h
                    Cabal.installExecutableFile Cabal.normal tmp (toStringI (binDir </> tgt)))
  when (isJust mbVer) (linkFileU Cabal.normal binDir tgt tgtPlain)
  where
    tgtPlain = fromText programName
    tgt      = maybe tgtPlain
                     (\ver -> fromText (programName <> "-" <> ver))
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
  removeFile (toStringI $ workingDir </> dest) `Ex.catch` ignoreDoesNotExist
  wd <- getCurrentDirectory
  setCurrentDirectory (toStringI workingDir)
  createFileLink (toStringI src) (toStringI dest)
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

trim :: String -> String
trim = let f = dropWhile isSpace . reverse in f . f

{- | try to find the emsdk directory
       1. check --with-emsdk option
       2. check EMSDK environment variable
       3. find emcc executable in PATH
 -}
configureEmsdkDir :: BootSettings
                  -> IO FilePath
configureEmsdkDir bs
  | Just dir <- bs ^. bsWithEmsdk = pure (fromText dir)
  | otherwise = do
    e <- lookupEnv "EMSDK"
    case e of
      Just edir -> pure (fromString edir)
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
            in  pure (fromString . concat . reverse . drop 2 . reverse $ emccDirParts)
            {-
              res <- findM emsdkScript (map (\x -> drop x (reverse dirParts)) [1..4])
              case res of
                Just r ->
                  -}
          _ -> notFound
  where
    -- emsdkScript parts = doesFileExist (reverse parts)
    -- findM p (x:xs) = p x >>= \r -> if r then pure (Just x) else findM p xs
    -- findM _ _      = pure Nothing
    notFound = failWith "emscripten emsdk not found (use --with-emsdk)"

-- | configure the locations
configureBootLocations :: BootSettings
                       -> BootPrograms
                       -> IO BootLocations
configureBootLocations bs pgms = do
  defaultTopDir <- getDefaultTopDir pgms
  let ghcjsTopDir = maybe defaultTopDir fromText (bs ^. bsGhcjsLibdir)
      ghcjsLibDir = ghcjsTopDir </> libSubDir
      globalDB = ghcjsLibDir </> "package.conf.d"
  sourceDir <- bootSourceDir bs
  libexecDir <- maybe (fromString <$> Paths.getLibexecDir)
                      (pure . fromText)
                      (bs ^. bsLibexecDir)
  buildDir <- fromString <$> prepareBuildDir (toStringI sourceDir)
                                              (toStringI ghcjsLibDir)
  emsdkDir <- configureEmsdkDir bs
  pure $ BootLocations sourceDir
                        buildDir
                        ghcjsTopDir
                        globalDB
                        libexecDir
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
      tpo   = template :: Traversal' BootPrograms (Program Optional)
      tpr   = template :: Traversal' BootPrograms (Program Required)
      pgms2 = pgms0 & bpCabal    %~ r bsWithCabal
                    & bpNode     %~ r bsWithNode
  -- resolve all programs
  pgms3 <- mapMOf tpo (resolveProgram bs)
             =<< mapMOf tpr (resolveProgram bs) pgms2
  traverseOf_ tpr (reportProgramLocation bs) pgms3
  traverseOf_ tpo (reportProgramLocation bs) pgms3
  return pgms3

resolveProgram :: MaybeRequired (Program a)
               => BootSettings
               -> Program a
               -> IO (Program a)
resolveProgram _bs pgm = do
  let search' = pgm ^. pgmSearch . to fromText
  absSearch <- (</> search') <$> getWorkingDirectory
  let searchPaths = catMaybes [ Just search'
                              , bj (relative search' &&
                                    length (splitDirectories search') > 1)
                                   absSearch
                              ]
  fmap catMaybes (mapM (findExecutable . encodeString) searchPaths) >>= \case
      (p':_) -> (\cp -> pgm & pgmLoc ?~ cp) <$> return (fromString p')
      _      | isRequired pgm -> return (pgm & pgmLoc .~ Nothing) {-failWith $
                  "program " <>
                  pgm ^. pgmName <>
                  " is required but could not be found at " <>
                  pgm ^. pgmSearch -}
             | otherwise      -> return (pgm & pgmLoc .~ Nothing)

-- | report location of a configured program
reportProgramLocation :: BootSettings -> Program a -> IO ()
reportProgramLocation bs p
  | Just l <- p ^. pgmLoc = msg' bs info $ "program " <>
                                           p ^. pgmName <>
                                           " found at " <>
                                           toTextI l
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
    bootMod  <- getModified (fromString bootLoc)
    bootSrc  <- bootSourceDir (be ^. beSettings)
    curDir   <- getWorkingDirectory
    p $ bool after
          ["ghcjs-boot has installed the libraries and runtime system for GHCJS"]
          ["ghcjs-boot will install the libraries and runtime system for GHCJS"]
    h "boot program"
    t "rl" [["ghcjs-boot program version", Info.getCompilerVersion]
           ,["file location", bootLoc]
           ,["last modified", show bootMod],[]
           ,["boot source location", toStringI bootSrc]
           ,["current directory", toStringI curDir]
           ]
    h "boot configuration"
    t "rl" [["installation directory", path $ beLocations . blGhcjsTopDir]
           ,["private binaries directory", path $ beLocations . blLibexecDir]
           ,["global package DB", path $ beLocations . blGlobalDB]
           ,["library path", path $ beLocations . blGhcjsTopDir]
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
             (template :: Traversal' BootPrograms (Program Required)) .
             to pgm ++
      be ^.. bePrograms .
             (template :: Traversal' BootPrograms (Program Optional)) .
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
    path l      = be ^. l . to toStringI
    str         = T.unpack
    pgm x       = [ x ^. pgmName . to str
                  , maybe "-" T.unpack (x ^. pgmVersion)
                  , x ^. pgmLocString
                  ]

-- | boot.yaml
bootConfigFile :: BootSettings -> IO B.ByteString
bootConfigFile bs = do
  sourceDir <- bootSourceDir bs
  let sourceDir' = toStringI sourceDir
  e <- doesFileExist sourceDir'
  if e then do -- it's a tar file
         entries <- Tar.read . BL.fromStrict <$>
           B.readFile sourceDir'
         pure (BL.toStrict $ getBootYaml entries)
       else B.readFile (toStringI $ sourceDir </> "boot" <.> "yaml")
  where
    getBootYaml (Tar.Next e es)
      | Tar.entryPath e == "boot/boot.yaml"
      , Tar.NormalFile contents _size <- Tar.entryContent e = contents
      | otherwise = getBootYaml es
    getBootYaml Tar.Done     = error "boot/boot.yaml file not found in archive"
    getBootYaml (Tar.Fail e) = error $ "error reading boot archive: " ++ show e

bootSourceDir :: BootSettings -> IO FilePath
bootSourceDir bs
  | Just dd <- bs ^. bsSourceDir = return (fromText dd)
  | otherwise                    = do
      workingDirectory <- getWorkingDirectory
      let configFile =  workingDirectory </> "boot" <.> "yaml"
      configInCurrentDir <- doesFileExist (toStringI configFile)
      if configInCurrentDir
        then pure workingDirectory
        else do
          dataDir <- Info.ghcjsBootDefaultDataDir
          let bootArchive = fromString dataDir </> "boot" <.> "tar"
          bootArchiveExists <- doesFileExist (toStringI bootArchive)
          pure $ if bootArchiveExists then bootArchive else workingDirectory

-- | our boot monad, we wrap around shelly but with a config environment
--   shelly commands are wrapped with logging
type B = ReaderT BootEnv Sh.Sh

runB :: BootEnv -> B a -> Sh.Sh a
runB e b = runReaderT b e

msg' :: BootSettings -> Verbosity -> Text -> IO ()
msg' bs v t = when (bs ^. bsVerbosity >= v) (T.putStrLn t)

msg :: Verbosity -> Text -> B ()
msg v t =
  view beSettings >>= \s -> when (s ^. bsVerbosity >= v) $ lift (Sh.echo t)

-- | log a message printing the current directory
msgD :: Verbosity -> Text -> B ()
msgD v t = pwd >>= \p -> msg v (toTextI p <> "$ " <> t)

msgD' :: BootSettings -> Verbosity -> Text -> IO ()
msgD' bs v t = getWorkingDirectory >>= \p -> msg' bs v (toTextI p <> "$ " <> t)

(</>) :: FilePath -> FilePath -> FilePath
(</>) = (Sh.</>)

{-
  lifted versions of the shelly operations we need. everything that
  makes externally visible changes is logged at the info (-v2)
  verbosity level.

  internal changes and file reads are logged at trace (-v3) level.
 -}
ls :: FilePath -> B [FilePath]
ls             = lift . Sh.ls

mkdir :: FilePath -> ReaderT BootEnv Sh.Sh ()
mkdir p        = msgD info ("mkdir "   <> toTextI p) >> lift (Sh.mkdir p)

mkdir_p :: FilePath -> ReaderT BootEnv Sh.Sh ()
mkdir_p p      = msgD info ("mkdir_p " <> toTextI p) >> lift (Sh.mkdir_p p)

cp :: FilePath -> FilePath -> ReaderT BootEnv Sh.Sh ()
cp f t         = msgD info ("cp "      <> toTextI f <> " -> " <> toTextI t) >>
                 lift (Sh.cp f t)

cp_r :: FilePath -> FilePath -> ReaderT BootEnv Sh.Sh ()
cp_r f t       = msgD info ("cp_r "    <> toTextI f <> " -> " <> toTextI t) >>
                 lift (Sh.cp_r f t)

rm_f :: FilePath -> ReaderT BootEnv Sh.Sh ()
rm_f p         = msgD info ("rm_f "    <> toTextI p) >> lift (Sh.rm_f p)

rm_rf :: FilePath -> ReaderT BootEnv Sh.Sh ()
rm_rf p        = msgD info ("rm_rf "   <> toTextI p) >> lift (Sh.rm_rf p)

cd :: FilePath -> ReaderT BootEnv Sh.Sh ()
cd p           = msgD trace ("cd "     <> toTextI p) >> lift (Sh.cd p)


sub :: B a -> B a
sub            = liftE  Sh.sub

test_d :: FilePath -> B Bool
test_d         = lift . Sh.test_d

test_f :: FilePath -> B Bool
test_f         = lift . Sh.test_f

test_s :: FilePath -> B Bool
test_s         = lift . Sh.test_s

run :: Program a -> [Text] -> B Text
run p xs       = msgD info (traceRun p xs) >>
                 requirePgmLoc p >>=
                 \loc -> lift (Sh.run  loc (p ^. pgmArgs ++ xs))

run_ :: Program a -> [Text] -> B ()
run_ p xs      = msgD info (traceRun p xs) >>
                requirePgmLoc p >>=
                \loc -> lift (Sh.run_ loc (p ^. pgmArgs ++ xs))

run'' :: FilePath -> [Text] -> B Text
run'' p xs       = msgD info (traceRun' p xs) >>
                   lift (Sh.run p xs)

run''_ :: FilePath -> [Text] -> B ()
run''_ p xs      = msgD info (traceRun' p xs) >>
                   lift (Sh.run_ p xs)


readBinary :: FilePath -> ReaderT BootEnv Sh.Sh B.ByteString
readBinary p   = msgD trace ("reading " <> toTextI p) >> lift (Sh.readBinary p)

canonic :: FilePath -> B FilePath
canonic        = lift . Sh.canonic

absPath :: FilePath -> B FilePath
absPath        = lift . Sh.absPath

pwd :: B FilePath
pwd            = lift   Sh.pwd

silently :: B a -> B a
silently       = liftE  Sh.silently

verbosely :: B a -> B a
verbosely      = liftE  Sh.verbosely

tracing :: Bool -> B a -> B a
tracing b      = liftE  (Sh.tracing b)

findWhen :: (FilePath -> B Bool) -> FilePath -> B [FilePath]
findWhen f p   = ask >>= \e -> lift (Sh.findWhen (runB e . f) p)

errorExit :: Text -> B a
errorExit      = lift . Sh.errorExit

writefile :: FilePath -> Text -> B ()
writefile p t  = msgD info  ("writing " <> toTextI p) >>
                 lift (Sh.writefile p t)

appendfile :: FilePath -> Text -> B ()
appendfile p t = msgD info ("appending " <> toTextI p) >>
                 lift (Sh.appendfile p t)

readfile :: FilePath -> B Text
readfile p     = msgD trace ("reading " <> toTextI p) >>
                 lift (Sh.readfile p)

withTmpDir :: (FilePath -> B b) -> B b
withTmpDir     = liftE2 Sh.withTmpDir

catchAny :: B b -> (Ex.SomeException -> B b) -> B b
catchAny a h   = ask >>=
                 \e -> lift (Sh.catchany_sh (runReaderT a e)
                                            (\ex -> runReaderT (h ex) e))

catchAny_ :: B b -> B b -> B b
catchAny_ a h  = catchAny a (\_ -> h)

setenv :: Text -> Text -> B ()
setenv e v     = lift (Sh.setenv e v)

get_env :: Text -> B (Maybe Text)
get_env        = lift . Sh.get_env

setStdin :: Text -> B ()
setStdin       = lift . Sh.setStdin

canonicalize :: FilePath -> B FilePath
canonicalize   = lift . Sh.canonicalize

relativeTo :: FilePath -> FilePath -> B FilePath
relativeTo e   = lift . (Sh.relativeTo e)

liftE :: (Sh.Sh a -> Sh.Sh a) -> B a -> B a
liftE s m = ask >>= \e -> lift (s $ runB e m)

liftE2 :: ((a -> Sh.Sh b) -> Sh.Sh b) -> (a -> B b) -> B b
liftE2 s f = ask >>= \e -> lift (s $ runB e . f)

traceRun :: Program a -> [Text] -> Text
traceRun p xs = "[" <>
                p ^. pgmName <>
                "]: " <>
                p ^. pgmLocText <>
                " " <>
                T.intercalate " " (map (showT . T.unpack) xs)


traceRun' :: FilePath -> [Text] -> Text
traceRun' p xs = {- "[" <>
                  toTextI p <>
                "]: " <> -}
                toTextI p <>
                " " <>
                T.intercalate " " (map (showT . T.unpack) xs)

requirePgmLoc :: Program a -> B FilePath
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

run' :: BootSettings -> Program a -> [Text] -> IO Text
run' bs p xs = do
  msgD' bs info (traceRun p xs)
  (e, out, _err) <- readProcessWithExitCode (p ^. pgmLocString)
                                            (map T.unpack xs)
                                            ""
  when (e /= ExitSuccess)
       (failWith $ "program " <>
                   p ^. pgmLocText <>
                   " returned a nonzero exit code")
  return (T.pack out)

-- | reduces verbosity of the action to the specified level
quieter :: Verbosity -> B a -> B a
quieter v = local $ over (beSettings . bsVerbosity) (min v)

toTextI :: FilePath -> Text
toTextI = Sh.toTextIgnore

fromString :: String -> FilePath
fromString = fromText . T.pack

toStringI :: FilePath -> String
toStringI = T.unpack . toTextI

pgmLocText :: Getter (Program a) Text
pgmLocText = pgmLoc . to (maybe "<not found>" toTextI)

pgmLocString :: Getter (Program a) String
pgmLocString = pgmLocText . to T.unpack

showT :: Show a => a -> Text
showT = T.pack . show

bool :: Bool -> a -> a -> a
bool b t f = if b then t else f

cond :: a -> a -> Bool -> a
cond t f b = bool b t f

bj :: Bool -> a -> Maybe a
bj b v = if b then Just v else Nothing

infixl 2 <^>
(<^>) :: MonadReader s m
      => (a -> m b)
      -> Getting a s a
      -> m b
(<^>) m l = m =<< view l

infixl 3 <*^>
(<*^>) :: (Applicative m, MonadReader s m)
       => (m (a -> b))
       -> Getting a s a -> m b
(<*^>) f l = f <*> view l

infixl 3 <<*^>
(<<*^>) :: (Applicative m, MonadReader s m)
        => (m (a -> m b))
        -> Getting a s a
        -> m b
(<<*^>) f l = join (f <*> view l)

infixl 4 <$^>
(<$^>) :: (Functor m, MonadReader s m) => (a -> b) -> Getting a s a -> m b
(<$^>) f l = f <$> view l
