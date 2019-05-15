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

{-# LANGUAGE CPP,
             ExtendedDefaultRules,
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
import           Data.Monoid
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as T
import           Data.Time.Clock
import           Data.Traversable
import qualified Data.Vector                     as V
import qualified Data.Version                    as Version
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
import           System.Environment.Executable   (getExecutablePath)
import           System.Exit
  (exitSuccess, exitFailure, ExitCode(..))
import qualified System.FilePath
import qualified System.FilePath                 as FP

import           System.IO
  (hPutStrLn, hSetBuffering, stderr, stdout, BufferMode(..))
import           System.PosixCompat.Files
  (setFileMode, createSymbolicLink)
import           System.Process                  (readProcessWithExitCode)

import           Shelly                          ((<.>), fromText)
import qualified Shelly                          as Sh

import           Text.Read                       (readEither, readMaybe)
import           Text.ParserCombinators.ReadP    (readP_to_S)

--
import           Compiler.GhcjsProgram           (printVersion)
import qualified Compiler.Info                   as Info
import qualified Compiler.Utils                  as Utils
import           Compiler.Settings               (NodeSettings(..))

default (Text)

isWindows :: Bool
#ifdef WINDOWS
isWindows = True
#else
isWindows = False
#endif

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
  , _bsWithGhcjsBin :: Maybe Text -- ^bin directory for GHCJS programs
  , _bsWithGhcjs    :: Maybe Text -- ^location of GHCJS compiler
  , _bsWithGhcjsPkg :: Maybe Text -- ^location of ghcjs-pkg program
  , _bsWithGhcjsRun :: Maybe Text -- ^location of ghcjs-run program
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

type Stage   = [CondPackage]

type Package = Text {- ^just the package name/location, unversioned

                        can be a directory name
                        (starting with ./ relative to the ghcjs-boot root),
                        a url or a plain package name
                     -}

data PlatformCond = Windows
                  | Unix
  deriving (Eq, Ord, Enum, Data, Typeable)

data CondPackage = CondPackage
  { _cpPlatform :: Maybe PlatformCond
  , _cpPackage  :: Package
  } deriving (Data, Typeable)

data BootLocations = BootLocations
  { _blSourceDir   :: FilePath
  , _blBuildDir    :: FilePath
  , _blGhcjsTopDir :: FilePath       -- ^install to here
  , _blGhcjsLibDir :: FilePath
  , _blGhcLibDir   :: FilePath       -- ^copy GHC files from here
  , _blGlobalDB    :: FilePath       -- ^global package database
  , _blUserDBDir   :: Maybe FilePath -- ^user package database location
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
data BootPrograms = BootPrograms { _bpGhcjs      :: Program Required
                                 , _bpGhcjsPkg   :: Program Required
                                 , _bpGhcjsRun   :: Program Required
                                 , _bpGhc        :: Program Required
                                 , _bpGhcPkg     :: Program Required
                                 , _bpCabal      :: Program Required
                                 , _bpNode       :: Program Required
                                 , _bpHaddock    :: Program Required
                                 , _bpNpm        :: Program Optional
                                 } deriving (Data, Typeable)

data BootEnv = BootEnv { _beSettings  :: BootSettings
                       , _beLocations :: BootLocations
                       , _bePrograms  :: BootPrograms
                       , _beStages    :: BootStages
                       }

data BootConfigFile = BootConfigFile BootStages BootPrograms
  deriving (Data, Typeable)

makeLenses ''Program
makeLenses ''CondPackage
makeLenses ''BootSettings
makeLenses ''BootLocations
makeLenses ''BootPrograms
makeLenses ''BootStages
makeLenses ''BootEnv

resolveConds :: [CondPackage] -> [Package]
resolveConds stage =
  let excluded cp = cp ^. cpPlatform ==
                    Just (if isWindows then Unix else Windows)
  in  map (view cpPackage) (filter (not . excluded) stage)

-- |all packages that can be built on this host
resolveCondsHost :: [CondPackage] -> [Package]
resolveCondsHost stage =
  let excluded cp = cp ^. cpPlatform ==
                    Just (if isWindows then Unix else Windows)
  in  map (view cpPackage) (filter (not . excluded) stage)

-- |all packages from all stages that can be built on this machine
allPackages :: B [Package]
allPackages = p <$> view beStages
  where
    p s = [s ^. bstGhcjsPrim, s ^. bstCabal, s ^. bstGhcPrim] ++
          resolveCondsHost ((s ^. bstStage1a) ++ (s ^. bstStage1b))

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
      removeCompleted
      initPackageDB
      cleanCache
      prepareLibDir
      let base = e ^. beLocations . blGhcjsLibDir
      setenv "CFLAGS" $ "-I" <> toTextI (base </> "include")
      installFakes
      installStage1
      removeFakes
      installCabal
      when (e ^. beSettings . bsHaddock) buildDocIndex
      liftIO . printBootEnvSummary True =<< ask
      addCompleted

instance Yaml.FromJSON BootPrograms where
  parseJSON (Yaml.Object v) = BootPrograms
    <$> v ..: "ghcjs" <*> v ..: "ghcjs-pkg" <*> v ..: "ghcjs-run"
    <*> v ..: "ghc"   <*> v ..: "ghc-pkg"
    <*> v ..: "cabal" <*> v ..: "node"      <*> v ..: "haddock-ghcjs"
    <*> v ..: "npm"
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
      o ..: p = pkgs Nothing =<< o .: p
      pkgs plc (Yaml.Object o) | [(k,v)] <- HM.toList o =
        matchCond plc k v
      pkgs plc (Yaml.String t) =
        pure [CondPackage plc t]
      pkgs plc (Yaml.Array v) =
        concat <$> mapM (pkgs plc) (V.toList v)
      pkgs _   _ =
        mempty
      matchCond plc k v
        | k == "IfWindows" && plc /= Just Unix    = pkgs (Just Windows) v
        | k == "IfUnix"    && plc /= Just Windows = pkgs (Just Unix)    v
        | otherwise                               = mempty
  parseJSON _ = mempty

instance Yaml.FromJSON BootConfigFile where
  parseJSON (Yaml.Object v) = BootConfigFile
    <$> v .: "packages" <*> v .: "programs"
  parseJSON _ = mempty


-- convert C:\x\y to /c/x/y (only on Windows)
msysPath :: Text -> Text
msysPath p
  | isWindows =
      let backToForward '\\' = '/'
          backToForward x    = x
          withoutSlash = "." `T.isPrefixOf` p || "\\" `T.isPrefixOf` p
      in bool withoutSlash "" "/" <> T.map backToForward (T.filter (/=':') p)
  | otherwise = p

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
          (long "with-ghcjs-bin" <>
           metavar "DIR" <>
           help "bin directory for GHCJS programs")
    <*> (optional . fmap T.pack . strOption)
          (long "with-ghcjs" <>
           metavar "PROGRAM" <>
           help "ghcjs program to use")
    <*> (optional . fmap T.pack . strOption)
          (long "with-ghcjs-pkg" <>
           metavar "PROGRAM" <>
           help "ghcjs-pkg program to use")
    <*> (optional . fmap T.pack . strOption)
          (long "with-ghcjs-run" <>
           metavar "PROGRAM" <>
           help "ghcjs-run program to use")
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
  traverseOf_ _Just initUser <^> beLocations . blUserDBDir
  where
    initUser dir = do
      rm_f (dir </> "package.conf")
      initDB "--user" (dir </> "package.conf.d")
    initDB dbName db = do
      rm_rf db
      -- ghcjs-pkg init throws error in windows if dir already exists
      when (not isWindows) (mkdir_p db)
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
  globalDB <- view (beLocations . blGlobalDB)
  ghcLib   <- view (beLocations . blGhcLibDir)
  ghcjsLib <- view (beLocations . blGhcjsLibDir)
  ghcjsTop <- view (beLocations . blGhcjsTopDir)
  let inc       = ghcjsLib </> "include"
      incNative = ghcjsLib </> "include_native"
      rtsConfFile   = "rts.conf"
      rtsLib    = ghcjsLib </> "rts"
  rtsConf <- readfile (ghcLib </> "package.conf.d" </> rtsConfFile)
  writefile (globalDB </> rtsConfFile)
            (fixRtsConf (toTextI inc) (toTextI rtsLib) rtsConf)
  ghcjs_pkg_ ["recache", "--global", "--no-user-package-db"]
  forM_ [ghcjsLib, inc, incNative] mkdir_p
  cp_r "shims" ghcjsLib
  sub $ cd "data" >> cp_r "." ghcjsLib
  sub $ cd (ghcLib </> "include") >> cp_r "." incNative
  sub $ cd (ghcLib </> "rts") >> cp_r "." rtsLib
  sub $ cd ("data" </> "include") >> installPlatformIncludes inc incNative
  mapM_ (\file -> cp (ghcLib </> file) (ghcjsLib </> file))
        ["settings", "platformConstants", "llvm-passes", "llvm-targets"]
  let unlitDest    = ghcjsLib </> "bin" </> exe "unlit"
      ghcjsRunDest = ghcjsLib </> exe "ghcjs-run"
  ghcjsRunSrc <- view (bePrograms . bpGhcjsRun . pgmLoc . to fromJust)
  mkdir_p (ghcjsLib </> "bin")
  cp (ghcLib </> "bin" </> exe "unlit") unlitDest
  cp ghcjsRunSrc ghcjsRunDest
  mapM_ (liftIO . Cabal.setFileExecutable . toStringI)
        [unlitDest, ghcjsRunDest]
  prepareNodeJs
  when (not isWindows) $ do
    let runSh = ghcjsLib </> "run" <.> "sh"
    writefile runSh "#!/bin/sh\nCOMMAND=$1\nshift\n\"$COMMAND\" \"$@\"\n"
    liftIO . Cabal.setFileExecutable . toStringI =<< absPath runSh
  -- required for integer-gmp
  subTop $ do
    writefile "empty.c" ""
    ghc_ ["-c", "empty.c"]
  when isWindows $ do
    cp (ghcLib </> "bin" </> exe "touchy")
       (ghcjsLib </> "bin" </> exe "touchy")
    rm_rf (ghcjsLib </> ".." </> "mingw")
    cp_r (ghcLib </> ".." </> "mingw")
         (ghcjsLib </> "..")
  writefile (ghcjsLib </> "ghc_libdir") (toTextI ghcLib)
  msg info "RTS prepared"

prepareNodeJs :: B ()
prepareNodeJs = do
  ghcjsLib <- view (beLocations . blGhcjsLibDir)
  buildDir <- view (beLocations . blBuildDir)
  nodeProgram <- view (bePrograms . bpNode . pgmLoc . to (maybe "-" toTextI))
  mbNodePath  <- view (beSettings . bsWithNodePath)
  extraArgs   <- view (beSettings . bsNodeExtraArgs)
  -- If no setting for NODE_PATH is specified, we use the libraries bundled
  -- with the ghcjs-boot submodule. We must run "npm rebuild" to build
  -- any sytem-specific components.
  when (isNothing mbNodePath) $ do
    npmProgram <- view (bePrograms . bpNpm)
    subTop (mkdir_p "ghcjs-node")
    liftIO $ unpackTar False
                       True
                       (toStringI $ ghcjsLib)
                       (toStringI $ buildDir </> "ghcjs-node.tar")
    subTop' "ghcjs-node" $ npm_ ["rebuild"]
  -- write nodeSettings.json file
  let nodeSettings = NodeSettings
       { nodeProgram         = T.unpack nodeProgram
       , nodePath            = mbNodePath
       , nodeExtraArgs       = maybeToList extraArgs
       , nodeKeepAliveMaxMem = 536870912
       }
  liftIO $ BL.writeFile (T.unpack . toTextI $ ghcjsLib </> "nodeSettings.json")
                        (Aeson.encode $ Aeson.toJSON nodeSettings)

installPlatformIncludes :: FilePath -> FilePath -> B ()
installPlatformIncludes inc incNative = do
  pw <- pwd
  cp_r "." inc
  nativeHeaders <- findWhen (return . isHeaderFile) incNative
  forM_ nativeHeaders $ \h -> do
    h' <- relativeTo incNative h
    e <- test_f ("." </> h')
    when (not e) $ do
      mkdir_p (directory $ inc </> h')
      writefile (inc </> h') (wrappedHeader h')
  where
    isHeaderFile = (`hasExtension` "h")
    wrappedHeader file =
      let pathParts = length (splitDirectories file)
          included  = iterate (".." </>) ("include_native" </> file) !! pathParts
      in T.unlines [ "#ifndef ghcjs_HOST_OS"
                   , "#include \"" <> toTextI included <> "\""
                   , "#endif"
                   ]

exe :: FilePath -> FilePath
exe = bool isWindows (<.>"exe") id

-- fixme this part will fail to compile
-- #ifdef WINDOWS
--   -- compile the resources we need for the runner to prevent Windows from
--   -- trying to detect programs that require elevated privileges
--   ghcjsTop <- view (beLocations . blGhcjsTopDir)
--   let windres = Program "windres"
--                         "windres"
--                         Nothing
--                         (Just $ ghcjsTop </>
--                                   ".." </>
--                                   "mingw" </>
--                                   "bin" </>
--                                   "windres.exe")
--                         []
--   subTop $ run_ windres ["runner.rc", "-o", "runner-resources.o"]
-- #endif

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
  s <- ask
  installGhcjsPrim
  installStage "1b" =<< stagePackages bstStage1b
  installGhcjsTh
  resolveWiredInPackages
    where
      fixGhcPrim = do
        descr <- T.lines <$> ghcjs_pkg [ "describe"
                                       , "ghc-prim"
                                       , "--no-user-package-db"
                                       ]
        setStdin (T.unlines $ map fixGhcPrimDescr descr)
        ghcjs_pkg_ ["update", "-", "--global", "--no-user-package-db"]
      -- add GHC.Prim to exposed-modules
      fixGhcPrimDescr line
        | "GHC.PrimopWrappers" `T.isInfixOf` line = line <> " GHC.Prim"
        | otherwise                               = line
      installStage name s = do
        msg info ("installing stage " <> name)
        forM_ s preparePackage >> cabalStage1 s

resolveWiredInPackages :: B ()
resolveWiredInPackages = subTop $ do
  wips <- readBinary ("wiredinpkgs" <.> "yaml")
  case Yaml.decodeEither wips of
   Left err   -> failWith $
     "error parsing wired-in packages file wiredinpkgs.yaml\n" <> T.pack err
   Right pkgs -> do
     pkgs' <- forM pkgs $ \p ->
       (p,) . T.strip <$> ghcjs_pkg [ "--simple-output"
                                    , "field"
                                    , p
                                    , "key"
                                    ]
     writefile ("wiredinkeys" <.> "yaml") $
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
            libDir   <- view (beLocations . blGhcjsLibDir)
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
  case catMaybes (map (T.stripPrefix "abi:") dumped) of
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
      dropComps = if stripFirst then 1 else 0
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
        | any (=="..") epd              = failSec e $
            "'..' in path"
        | listToMaybe epd /= je &&
          isSupportedEntry (Tar.entryContent e) = failSec e $
            "tar bomb, expected path component: " <> T.pack expected
        | otherwise                     = do
            --view (beSettings . bsVerbosity) >>= \v ->
              -- this gets chatty, reduce verbosity for
              -- file writes / directory creates here
              -- unless we're at trace level
            --  (if v < trace then quieter warn else id)
            (extractEntry e $ dest FP.</>
                   (FP.joinPath
                                (drop (if stripFirst then 1 else 0) epd)))
            return je
        where ep  = Tar.entryPath e
              epd = FP.splitDirectories ep
      isSupportedEntry (Tar.NormalFile{}) = True
      isSupportedEntry (Tar.Directory{})  = True
      isSupportedEntry _                  = False
      extractEntry :: Tar.Entry -> Prelude.FilePath -> IO ()
      extractEntry e tgt
        | Tar.NormalFile bs size <- Tar.entryContent e = do
            createDirectoryIfMissing True (FP.dropFileName tgt)
            BL.writeFile tgt bs
            setPermissions (Tar.entryPermissions e) tgt
        | Tar.Directory <- Tar.entryContent e = do
            createDirectoryIfMissing True tgt
            setPermissions (Tar.entryPermissions e) tgt
        | Tar.SymbolicLink linkTgt <- Tar.entryContent e
        , preserveSymlinks = do
            createDirectoryIfMissing True (FP.dropFileName tgt)
            fileExists <- doesFileExist tgt
            dirExists  <- doesDirectoryExist tgt
            when fileExists (removeFile tgt)
            when dirExists  (removeDirectoryRecursive tgt)
            createSymbolicLink (Tar.fromLinkTarget linkTgt) tgt
        | otherwise = hPutStrLn stderr $
            "ignoring unexpected entry type in tar. " <>
            "only normal files and directories (no links) " <>
            "are supported:\n    " <> tgt
      -- setPermissions :: FileMode -> Prelude.FilePath -> IO ()
      setPermissions mode tgt = do
        absTgt <- makeAbsolute tgt
        setFileMode tgt mode
        -- absTgt <- absPath tgt
    {-    msgD trace $ "setting permissions of " <>
                     toTextI tgt <>
                     " to " <>
                     showT mode -}
                     {-
        let tgt  = toStringI absTgt
            tgt' = bool (last tgt `elem` ['/','\\']) (init tgt) tgt
        liftIO (setFileMode tgt' mode) -}


ghc_         = runE_ bpGhc
ghc_pkg      = runE  bpGhcPkg
ghcjs_pkg    = runE  bpGhcjsPkg
ghcjs_pkg_   = runE_ bpGhcjsPkg
haddock_     = runE_ bpHaddock
cabal  args  = runE  bpCabal ( cabalArgs args )
cabal_ args  = runE_ bpCabal ( cabalArgs args )
npm_         = runE_ bpNpm

cabalArgs args = args ++ bool isWindows ["-fghcjs_windows"] []

runE  g a = view (bePrograms . g) >>= flip run  a
runE_ g a = view (bePrograms . g) >>= flip run_ a

{- | stage 1 cabal install: boot mode, hand off to GHC if GHCJS
     cannot yet compile it -}
cabalStage1 :: [Text] -> B ()
cabalStage1 pkgs = sub $ do
  ghc <- requirePgmLoc =<< view (bePrograms . bpGhc)
  s   <- view beSettings
  p   <- pwd
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
  checkInstallPlan pkgs args
  cabal_ args

-- | regular cabal install for GHCJS
cabalInstall [] = do
  msg info "cabal-install: no packages, nothing to do"
  return ()
cabalInstall pkgs = do
  globalFlags <- cabalGlobalFlags
  flags <- cabalInstallFlags (length pkgs == 1)
  cmd <- cabalInstallCommand
  setenv "GHCJS_BOOTING" "1"
  let args = globalFlags ++ cmd : pkgs ++ flags
  checkInstallPlan pkgs args
  cabal_ args

cabalInstallCommand :: B Text
cabalInstallCommand = do
  cabalVer <- view (bePrograms . bpCabal . pgmVersion)
  case filter (null . snd) $
       readP_to_S Version.parseVersion (maybe "" T.unpack cabalVer) of
    [(ver,_)] | ver >= Version.makeVersion [2,5] -> pure "v1-install"
    _                                            -> pure "install"

-- check that Cabal is only going to install the packages we specified
-- uses somewhat fragile parsing of --dry-run output, find a better way
checkInstallPlan :: [Package] -> [Text] -> B ()
checkInstallPlan pkgs opts = do
  plan <- cabal (opts ++ ["-v2", "--dry-run"])
  when (any ($ plan) [hasReinstalls, {- hasUnexpectedInstalls, -} hasNewVersion])
       (err plan)
  where
    -- reject reinstalls
    hasReinstalls = T.isInfixOf "(reinstall)"

    -- only allow one version of each package during boot
    hasNewVersion = T.isInfixOf "(new version)"

    hasUnexpectedInstalls plan =
      let ls = filter ("(new package)" `T.isInfixOf`) (T.lines plan)
      in  length ls /= length pkgs || not (all isExpected ls)

    isExpected l
      | (w:_) <- T.words l, ps@(_:_) <- T.splitOn "-" w =
          any (T.intercalate "-" (init ps) `T.isInfixOf`) pkgs
      | otherwise = False

    err plan = failWith $
      "unacceptable install plan, expecting exactly the following " <>
      "list of packages to be installed,\n" <>
      "without reinstalls and only one version " <>
      "of each package in the database:\n\n" <>
      T.unlines (map ("  - " <>) pkgs) <>
      "\nbut got:\n\n" <>
      plan

cabalGlobalFlags :: B [Text]
cabalGlobalFlags = do
  instDir  <- view (beLocations . blGhcjsTopDir)
  return ["--config-file"
         ,toTextI (instDir </> "cabalBootConfig")
         ,"--ignore-sandbox"
         ]

cabalInstallFlags :: Bool -> B [Text]
cabalInstallFlags parmakeGhcjs = do
  debug    <- view (beSettings . bsDebug)
  v        <- view (beSettings . bsVerbosity)
  j        <- view (beSettings . bsJobs)
  ghcjs    <- view (bePrograms . bpGhcjs)
  ghcjsPkg <- view (bePrograms . bpGhcjsPkg)
  instDir  <- view (beLocations . blGhcjsTopDir)
  prof     <- view (beSettings . bsProf)
  haddock  <- view (beSettings . bsHaddock)
  return $ [ "--global"
           , "--ghcjs"
           , "--one-shot"
           , "--avoid-reinstalls"
           , "--builddir",      "dist"
           , "--with-compiler", ghcjs ^. pgmLocText
           , "--with-hc-pkg",   ghcjsPkg ^. pgmLocText
           , "--prefix",        toTextI instDir
           , bool haddock "--enable-documentation" "--disable-documentation"
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

ignoreExcep a = a `catchAny`
  (\e -> msg info $ "ignored exception: " <> showT e)

stagePackages :: Getter BootStages Stage -> B [Package]
stagePackages l = do
  condPkgs <- view (beStages . l)
  return (resolveConds condPkgs)

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
  pgms2 <- configureBootPrograms bs (addCmdToNpm pgms1)
  locs  <- configureBootLocations bs pgms2
  return (BootEnv bs locs pgms2 stgs)

  where addCmdToNpm = bool isWindows (bpNpm . pgmSearch <>~ ".cmd") id
  
-- | configure the locations
configureBootLocations :: BootSettings
                       -> BootPrograms
                       -> IO BootLocations
configureBootLocations bs pgms = do
  ghcLibDir    <- fromText . T.strip <$> run' bs (pgms ^. bpGhc)
                    ["--print-libdir"]
  ghcjsLibDir  <- fromText . T.strip <$> run' bs (pgms ^. bpGhcjs)
                    ["--ghcjs-booting-print", "--print-libdir"]
  ghcjsTopDir  <- fromText . T.strip <$> run' bs (pgms ^. bpGhcjs)
                    ["--ghcjs-booting-print", "--print-topdir"]
  globalDB     <- fromText . T.strip <$> run' bs (pgms ^. bpGhcjs)
                    ["--ghcjs-booting-print", "--print-global-db"]
  userDBT      <- T.strip <$> run' bs (pgms ^. bpGhcjs)
                    ["--ghcjs-booting-print", "--print-user-db-dir"]
  when (T.null (toTextI ghcjsLibDir)) $
    failWith ("Could not determine GHCJS library installation path.\n" <>
              "Make sure that the ghcjs wrapper script " <>
              "(or options file on Windows) " <>
              "has been set up correctly.")
  sourceDir <- bootSourceDir bs
  buildDir <- fromString <$> prepareBuildDir (toStringI sourceDir)
                                             (toStringI ghcjsLibDir)
  pure $ BootLocations sourceDir
                       buildDir
                       ghcjsTopDir
                       ghcjsLibDir
                       ghcLibDir
                       globalDB
                       (bool (userDBT == "<none>")
                             Nothing
                             (Just $ fromText userDBT))

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
  let r l   = maybe id (pgmSearch .~) (bs ^. l)
      tpo   = template :: Traversal' BootPrograms (Program Optional)
      tpr   = template :: Traversal' BootPrograms (Program Required)
      binPrefix pgms pfx =
        let addBin p = p . pgmSearch . iso fromText toTextI %~ (pfx</>)
        in  pgms & addBin bpGhcjs
                 & addBin bpGhcjsPkg
                 & addBin bpGhcjsRun
      pgms1 = maybe pgms0 (binPrefix pgms0 . fromText) (bs ^. bsWithGhcjsBin)
      pgms2 = pgms1 & bpGhcjs    %~ r bsWithGhcjs
                    & bpGhcjsPkg %~ r bsWithGhcjsPkg
                    & bpGhcjsRun %~ r bsWithGhcjsRun
                    & bpGhc      %~ r bsWithGhc
                    & bpGhcPkg   %~ r bsWithGhcPkg
                    & bpCabal    %~ r bsWithCabal
                    & bpNode     %~ r bsWithNode
  -- resolve all programs
  pgms3 <- mapMOf tpo (resolveProgram bs)
             =<< mapMOf tpr (resolveProgram bs) pgms2
  traverseOf_ tpr (reportProgramLocation bs) pgms3
  traverseOf_ tpo (reportProgramLocation bs) pgms3
  pgms4 <- checkProgramVersions bs pgms3
  -- checkCabalSupport bs pgms4
  return pgms4

resolveProgram :: MaybeRequired (Program a)
               => BootSettings
               -> Program a
               -> IO (Program a)
resolveProgram bs pgm = do
  let search' = pgm ^. pgmSearch . to fromText
  absSearch <- (</> search') <$> getWorkingDirectory
  let searchPaths = catMaybes [ Just search'
                              , bj (relative search' &&
                                    length (splitDirectories search') > 1)
                                   absSearch
                              ]
  fmap catMaybes (mapM (findExecutable . encodeString) searchPaths) >>= \case
      (p':_) -> (\cp -> pgm & pgmLoc .~ Just cp) <$> return (fromString p')
      _      | isRequired pgm -> failWith $
                  "program " <>
                  pgm ^. pgmName <>
                  " is required but could not be found at " <>
                  pgm ^. pgmSearch
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

-- | check that the GHC, ghcjs and ghcjs-pkg we're using are the correct version
checkProgramVersions :: BootSettings -> BootPrograms -> IO BootPrograms
checkProgramVersions bs pgms = do
  pgms' <- foldrM verifyVersion pgms
    [ ( view bpGhcjs, set bpGhcjs
      , "--numeric-version"
      , Just Info.getCompilerVersion
      , True
      )
    , ( view bpGhcjs
      , set bpGhcjs
      , "--numeric-ghc-version"
      , Just Info.getGhcCompilerVersion
      , False
      )
-- fixme check ghc version again, but only major version?
    -- , (view bpGhc, set bpGhc,      "--numeric-version",       Just Info.getGhcCompilerVersion, True)
    , ( view bpGhcjsPkg
      , set bpGhcjsPkg
      , "--numeric-ghcjs-version"
      , Nothing
      , True
      )
    -- , (view bpGhcjsPkg, set bpGhcjsPkg, "--numeric-ghc-version",   Just Info.getGhcCompilerVersion, False)
    , ( view bpCabal, set bpCabal
      , "--numeric-version"
      , Nothing
      , True
      )
    , ( view bpNode, set bpNode
      , "--version"
      , Nothing
      , True
      )
    ]
  verifyNotProfiled
  verifyNodeVersion pgms'
  where
    verifyNotProfiled :: IO ()
    verifyNotProfiled = return ()
    --  res <- T.strip <$> run' bs (pgms ^. bpGhcjs) ["--ghcjs-booting-print", "--print-rts-profiled"]
    --  when (res /= "False") $ failWith ("GHCJS program " <> pgms ^. bpGhcjs . pgmLocText <>
    --                                    " has been installed with executable profiling.\n" <>
      --                                    "you need a non-profiled executable to boot")
--    verifyVersion :: (Lens' BootPrograms (Program Required), Text, Maybe String, Bool) -> BootPrograms -> IO BootPrograms
    verifyVersion :: forall a. ( BootPrograms -> Program a
                               , Program a -> BootPrograms -> BootPrograms
                               , Text, Maybe String
                               , Bool
                               ) -> BootPrograms -> IO BootPrograms
    verifyVersion (g, s, arg :: Text, expected :: Maybe String, update :: Bool) ps = do
      res <- T.strip <$> run' bs (g ps) [arg]
      case expected of
        Nothing -> return ()
        Just exp -> when (res /= T.pack exp) $
          failWith ("version mismatch for program " <> (g ps) ^. pgmName <> " at " <> (g ps ^. pgmLocText)
                      <> ", expected " <> T.pack exp <> " but got " <> res)
      return $
        if update then s (pgmVersion .~ Just res $ g ps) ps
                  else ps
    verifyNodeVersion pgms = do
      let verTxt = fromMaybe "-" (pgms ^. bpNode . pgmVersion)
          v      = mapM (readMaybe . T.unpack . T.dropWhile (== 'v')) . T.splitOn "." . T.takeWhile (/='-') $ verTxt :: Maybe [Integer]
      case v of
        Just (x:y:z:_)
          | x >= 6 -> return pgms
          | otherwise -> failWith ("minimum required version for node.js is 6.0.0, found: " <> verTxt)
        _             -> failWith ("unrecognized version for node.js: " <> verTxt)

-- | read the boot configuration yaml file
readBootConfigFile :: BootSettings -> IO BootConfigFile
readBootConfigFile bs = do
  bf <- bootConfigFile bs
  -- b <- B.readFile (toStringI bf)
  case Yaml.decodeEither bf of
    Left err  -> failWith $
      "error parsing boot.yaml configuration file\n" <>
      T.pack err
    Right bss -> return bss

printBootEnvSummary :: Bool -> BootEnv -> IO ()
printBootEnvSummary after be = do
  section "Boot libraries installation for GHCJS" $ do
    bootLoc  <- getExecutablePath
    bootMod  <- getModified (fromString bootLoc)
    bootSrc  <- bootSourceDir (be ^. beSettings)
    ghcjsMod <- maybe (return "<unknown>")
                      (fmap show . getModified)
                      (be ^. bePrograms . bpGhcjs . pgmLoc)
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
           ,["global package DB", path $ beLocations . blGlobalDB]
           ,["user package DB location"
              , path $ beLocations . blUserDBDir . to (fromMaybe "<none>")],[]
           ,["GHCJS version", ver "<unknown>" bpGhcjs]
           ,["program location", loc bpGhcjs]
           ,["library path", path $ beLocations . blGhcjsLibDir]
           ,["last modified", ghcjsMod],[]
           ,["GHC version", ver "<unknown>" bpGhc]
           ,["location", loc bpGhc]
           ,["library path", path $ beLocations . blGhcLibDir],[]
           ,["cabal-install version", ver "<unknown>" bpCabal]
           ,["location", loc bpCabal],[]
           ,["ghcjs-pkg version", ver "<unknown>" bpGhcjsPkg]
           ,["location", loc bpGhcjsPkg],[]
           ]
    h "packages"
    p ["stage 1a"] >> l (stg bstStage1a)
    p ["ghcjs-prim:  " ++ be ^. beStages . bstGhcjsPrim . to str]
    p ["stage 1b"] >> l (stg bstStage1b)
    -- p ["ghcjs-th:  " ++ be ^. beStages . bstGhcjsTh . to str]
    p ["Cabal:  " ++ be ^. beStages . bstCabal . to str]
  section "Configured programs" $ do
    t "hlll" $ ["program", "version", "location"] :
      be ^.. bePrograms .
             (template :: Traversal' BootPrograms (Program Required)) .
             to pgm ++
      be ^.. bePrograms .
             (template :: Traversal' BootPrograms (Program Optional)) .
             to pgm
  where
    stg s       = be ^.. beStages . s . to resolveConds . traverse . to str
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
                      colSep    = replicate 3 ' '
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
    y b         = if b then "Yes" else "No"
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
    bootYamlFP = "boot" FP.</> "boot.yaml"
    getBootYaml (Tar.Next e es)
      | Tar.entryPath e == bootYamlFP
      , Tar.NormalFile contents _size <- Tar.entryContent e = contents
      | otherwise = getBootYaml es
    getBootYaml Tar.Done     =
      error $ show bootYamlFP ++ " file not found in archive"
    getBootYaml (Tar.Fail e) =
      error $ "error reading boot archive: " ++ show e

bootSourceDir :: BootSettings -> IO FilePath
bootSourceDir bs
  | Just dd <- bs ^. bsSourceDir = return (fromText dd)
  | otherwise                    = do
      workingDirectory <- getWorkingDirectory
      let configFile =  workingDirectory </> "boot" <.> "yaml"
      configInCurrentDir <- doesFileExist (toStringI configFile)
      if configInCurrentDir
        then pure ( workingDirectory)
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
ls             = lift . Sh.ls
mkdir p        = msgD info ("mkdir "   <> toTextI p) >> lift (Sh.mkdir p)
mkdir_p p      = msgD info ("mkdir_p " <> toTextI p) >> lift (Sh.mkdir_p p)
cp f t         = msgD info ("cp "      <> toTextI f <> " -> " <> toTextI t) >>
                 lift (Sh.cp f t)
cp_r f t       = msgD info ("cp_r "    <> toTextI f <> " -> " <> toTextI t) >>
                 lift (Sh.cp_r f t)
rm_f p         = msgD info ("rm_f "    <> toTextI p) >> lift (Sh.rm_f p)
rm_rf p        = msgD info ("rm_rf "   <> toTextI p) >> lift (Sh.rm_rf p)
cd p           = msgD trace ("cd "     <> toTextI p) >> lift (Sh.cd p)
sub            = liftE  Sh.sub
test_d         = lift . Sh.test_d
test_f         = lift . Sh.test_f
test_s         = lift . Sh.test_s
run p xs       = msgD info (traceRun p xs) >>
                 requirePgmLoc p >>=
                 \loc -> lift (Sh.run  loc (p ^. pgmArgs ++ xs))
run_ p xs      = msgD info (traceRun p xs) >>
                requirePgmLoc p >>=
                \loc -> lift (Sh.run_ loc (p ^. pgmArgs ++ xs))
readBinary p   = msgD trace ("reading " <> toTextI p) >> lift (Sh.readBinary p)
canonic        = lift . Sh.canonic
absPath        = lift . Sh.absPath
pwd            = lift   Sh.pwd
silently       = liftE  Sh.silently
verbosely      = liftE  Sh.verbosely
tracing b      = liftE  (Sh.tracing b)
findWhen f p   = ask >>= \e -> lift (Sh.findWhen (runB e . f) p)
errorExit      = lift . Sh.errorExit
writefile p t  = msgD info  ("writing " <> toTextI p) >>
                 lift (Sh.writefile p t)
appendfile p t = msgD info ("appending " <> toTextI p) >>
                 lift (Sh.appendfile p t)
readfile p     = msgD trace ("reading " <> toTextI p) >>
                 lift (Sh.readfile p)
withTmpDir     = liftE2 Sh.withTmpDir
catchAny a h   = ask >>=
                 \e -> lift (Sh.catchany_sh (runReaderT a e)
                                            (\ex -> runReaderT (h ex) e))
catchAny_ a h  = catchAny a (\_ -> h)
setenv e v     = lift (Sh.setenv e v)
get_env        = lift . Sh.get_env
setStdin       = lift . Sh.setStdin
canonicalize   = lift . Sh.canonicalize
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

addCompleted :: B ()
addCompleted = do
  f <- completedFile
  writefile f "full"

removeCompleted :: B ()
removeCompleted = rm_f =<< completedFile

completedFile :: B FilePath
completedFile =
  absPath . (</> ("ghcjs_boot" <.> "completed")) =<<
     view (beLocations . blGhcjsTopDir)

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
