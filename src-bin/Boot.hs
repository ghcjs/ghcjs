{-# OPTIONS_GHC -O0 #-}

{- |
    The ghcjs-boot program installs the libraries and runtime system for GHCJS

    There are two types of installation:

    - release (default):
        install ghcjs-boot and shims from the tar cache archives included
        in the package

    - development:
        install ghcjs-boot and shims from their git repository

    You can customize the boot configuration in boot.yaml and override some
    of the options on the command line.

    If you want to install to a different directory, set the ghcjs and ghcjs-pkg
    programs to wrapper scripts that pass the correct -B flag to the executable
    (see lib/etc/ghcjs.sh and lib/etc/ghcjs-pkg.sh in the GHCJS data dir)
 -}

{-# LANGUAGE CPP, ExtendedDefaultRules, OverloadedStrings, ScopedTypeVariables,
             TemplateHaskell, LambdaCase, FlexibleInstances, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, NoMonomorphismRestriction, FlexibleContexts,
             ImpredicativeTypes, TupleSections
  #-}
module Main where

import           Prelude                         hiding (FilePath, forM_, elem, mapM, mapM_, any, all, concat, concatMap)

import qualified Distribution.Simple.Utils       as Cabal

import qualified Codec.Archive.Tar               as Tar
import qualified Codec.Archive.Tar.Entry         as Tar

import           Control.Applicative
import qualified Control.Exception               as Ex
import           Control.Lens                    hiding ((<.>))
import           Control.Monad                   (void, when, unless, mplus, join)
import           Control.Monad.Reader            (MonadReader, ReaderT(..), MonadIO, ask, local, lift, liftIO)

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
import qualified Data.Text.Encoding              as T
import qualified Data.Text.IO                    as T
import           Data.Time.Clock
import           Data.Traversable
import           Data.Typeable
import qualified Data.Vector                     as V
import           Data.Yaml                       ((.:))
import qualified Data.Yaml                       as Yaml

import           Filesystem                      (getWorkingDirectory, getModified, getSize
                                                 ,canonicalizePath)
import           Filesystem.Path                 hiding ((<.>), (</>), null, concat)
import           Filesystem.Path.CurrentOS       (encodeString)

import           GHC.IO.Encoding                 (setLocaleEncoding, setForeignEncoding, utf8)

import qualified Network.Browser                 as Br
import           Network.HTTP                    (mkRequest, RequestMethod(..), Response(..))
import           Network.URI                     (parseURI, URI(..))

import           Options.Applicative             hiding (info, (&))
import qualified Options.Applicative             as O

import           System.Directory                (findExecutable)
import           System.Environment              (getEnvironment, getArgs)
import           System.Environment.Executable   (getExecutablePath)
import           System.Exit                     (exitSuccess, exitFailure, ExitCode(..))
import qualified System.FilePath
import           System.IO                       (hSetBuffering, stdout, BufferMode(..))
import           System.PosixCompat.Files        (setFileMode)
import           System.Process                  (readProcessWithExitCode)

import           Shelly                          ((<.>),{-(</>),-} fromText)
import qualified Shelly                          as Sh

import           Text.Read                       (readEither, readMaybe)

--
import           Compiler.GhcjsProgram           (printVersion)
import qualified Compiler.Info                   as Info
import           Compiler.Utils                  as Utils

default (Text)

isWindows :: Bool
#ifdef WINDOWS
isWindows = True
#else
isWindows = False
#endif

newtype Verbosity = Verbosity Int deriving (Eq, Ord, Data, Typeable)
trace = Verbosity 3
info  = Verbosity 2
warn  = Verbosity 1
err   = Verbosity 0

data BootSettings = BootSettings { _bsClean        :: Bool       -- ^ remove existing tree first
                                 , _bsShowVersion  :: Bool       -- ^ show the version and exit
                                 , _bsQuick        :: Bool       -- ^ don't install the Cabal library and stage2 packages
                                 , _bsDev          :: Bool       -- ^ do a development boot
                                 , _bsJobs         :: Maybe Int  -- ^ number of parallel jobs
                                 , _bsDebug        :: Bool       -- ^ build debug version of the libraries (GHCJS records the STG in the object files for easier inspection)
                                 , _bsProf         :: Bool       -- ^ build profiling version of the libraries
                                 , _bsHaddock      :: Bool       -- ^ build documentation
                                 , _bsVerbosity    :: Verbosity  -- ^ verbosity level 0..3, 2 is default
                                 , _bsIconvInclude :: Maybe Text -- ^ directory containing iconv.h
                                 , _bsIconvLib     :: Maybe Text -- ^ directory containing iconv library
                                 , _bsGmpInclude   :: Maybe Text -- ^ directory containing gmp.h
                                 , _bsGmpLib       :: Maybe Text -- ^ directory containing gmp library
                                 , _bsGmpFramework :: Bool       -- ^ with-gmp-framework-preferred
                                 , _bsGmpInTree    :: Bool       -- ^ force using the in-tree GMP
                                 , _bsWithCabal    :: Maybe Text -- ^ location of cabal (cabal-install) executable, must have GHCJS support
                                 , _bsWithGhcjsBin :: Maybe Text -- ^ bin directory for GHCJS programs
                                 , _bsWithGhcjs    :: Maybe Text -- ^ location of GHCJS compiler
                                 , _bsWithGhcjsPkg :: Maybe Text -- ^ location of ghcjs-pkg program
                                 , _bsWithGhcjsRun :: Maybe Text -- ^ location of ghcjs-run program
                                 , _bsWithGhc      :: Maybe Text -- ^ location of GHC compiler (must have a GHCJS-compatible Cabal library installed. ghcjs-boot copies some files from this compiler)
                                 , _bsWithNode     :: Maybe Text -- ^ location of the node.js program
                                 , _bsWithDataDir  :: Maybe Text -- ^ override data dir
                                 , _bsWithConfig   :: Maybe Text -- ^ installation source configuration (default: lib/etc/boot-sources.yaml in data dir)
                                 , _bsShimsDevRepo   :: Maybe Text -- ^ override shims repository
                                 , _bsShimsDevBranch :: Maybe Text -- ^ override shims branch or commit
                                 , _bsBootDevRepo    :: Maybe Text -- ^ override ghcjs-boot repository
                                 , _bsBootDevBranch  :: Maybe Text -- ^ override ghcjs-boot branch or commit
                                 , _bsStage1Unbooted :: Bool       -- ^ build stage1 (like --quick) but leave the compiler in unbooted state with the Cabal package still registered
                                 } deriving (Ord, Eq, Data, Typeable)

{- | locations to get installation files from

     files may have multiple locations, they're tried in order until one succeeds

     locations are typically read from boot-sources.yaml, customize the defaults
     in lib/etc/boot-sources.yaml in the installed data dir, or use
     the --sources or --datadir options
 -}
data BootSources = BootSources { _bsrcShims                 :: [Text]
                               , _bsrcBoot                  :: [Text]
                               , _bsrcTest                  :: [Text]
                               , _bsrcEtc                   :: [Text]
                               , _bsrcDoc                   :: [Text]
                               , _bsrcGhcjsPrim             :: [Text]
                               , _bsrcInclude               :: [Text]
                               , _bsrcShimsDev              :: [Text]
                               , _bsrcShimsDevBranch        :: Text
                               , _bsrcBootDev               :: [Text]
                               , _bsrcBootDevBranch         :: Text
                               , _bsrcBuildtoolsWindows     :: [Text]
                               , _bsrcBuildtoolsBootWindows :: [Text]
                               } deriving (Data, Typeable)

{- | Stage configuration file: packages to install in each stage

     see boot.yaml for more information
 -}
data BootStages = BootStages { _bstStage1a   :: Stage
                             , _bstStage1b   :: Stage
                             , _bstStage2    :: Stage
                             , _bstPretend   :: [Package] -- ^ packages we pretend to have in stage one, but actually hand off to GHC
                             , _bstCabal     :: Package   -- ^ installed between 1b and 2, only when doing a full boot
                             , _bstGhcjsPrim :: Package   -- ^ installed between 1a and 1b
                             , _bstGhcPrim   :: Package   -- ^ installed before stage 1a
                             } deriving (Data, Typeable)

type Stage   = [CondPackage]
type Package = Text          -- ^ just the package name, can be a directory name
                             --   (starting with ./ relative to the ghcjs-boot root),
                             --   a url or a plain package name

data PlatformCond = Windows | Unix  deriving (Eq, Ord, Enum, Data, Typeable)
data BootTypeCond = Full    | Quick deriving (Eq, Ord, Enum, Data, Typeable)

data CondPackage = CondPackage { _cpPlatform :: Maybe PlatformCond
                               , _cpBootType :: Maybe BootTypeCond
                               , _cpPackage  :: Package
                               } deriving (Data, Typeable)

data BootLocations = BootLocations { _blGhcjsTopDir :: FilePath       -- ^ install to here
                                   , _blGhcjsLibDir :: FilePath
                                   , _blGhcLibDir   :: FilePath       -- ^ copy GHC files from here
                                   , _blGlobalDB    :: FilePath       -- ^ global package database
                                   , _blUserDBDir   :: Maybe FilePath -- ^ user package database location
                                   , _blNativeToo   :: Bool           -- ^ build/install native code too
                                   } deriving (Data, Typeable)

data Program a = Program { _pgmName    :: Text           -- ^ program name for messages
                         , _pgmSearch  :: Text           -- ^ name searched for when configuring the program (from command line or config file)
                         , _pgmVersion :: Maybe Text     -- ^ version if known
                         , _pgmLoc     :: Maybe FilePath -- ^ absolute path to the program
                         , _pgmArgs    :: [Text]         -- ^ extra arguments to pass to the program
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
                                 , _bpGit        :: Program Optional
                                 , _bpAlex       :: Program Optional
                                 , _bpHappy      :: Program Optional
                                 , _bpTar        :: Program Optional
                                 , _bpCpp        :: Program Optional
                                 , _bpBash       :: Program Optional
                                 , _bpAutoreconf :: Program Optional
                                 , _bpMake       :: Program Optional
                                 } deriving (Data, Typeable)

data BootEnv = BootEnv { _beSettings  :: BootSettings
                       , _beSources   :: BootSources
                       , _beLocations :: BootLocations
                       , _bePrograms  :: BootPrograms
                       , _beStages    :: BootStages
                       }

data BootConfigFile = BootConfigFile BootStages BootSources BootPrograms
  deriving (Data, Typeable)

makeLenses ''Program
makeLenses ''CondPackage
makeLenses ''BootSettings
makeLenses ''BootSources
makeLenses ''BootLocations
makeLenses ''BootPrograms
makeLenses ''BootStages
makeLenses ''BootEnv

resolveConds :: Bool -> [CondPackage] -> [Package]
resolveConds quick stage =
  let excluded cp = cp ^. cpPlatform == Just (if isWindows then Unix else Windows) ||
                    cp ^. cpBootType == Just (if quick     then Full else Quick)
  in  map (view cpPackage) (filter (not . excluded) stage)

-- | all packages that can be built on this host
resolveCondsHost :: [CondPackage] -> [Package]
resolveCondsHost stage =
  let excluded cp = cp ^. cpPlatform == Just (if isWindows then Unix else Windows)
  in  map (view cpPackage) (filter (not . excluded) stage)

-- | all packages from all stages that can be built on this machine
allPackages :: B [Package]
allPackages = p <$> view beStages
  where
    p s = [s ^. bstGhcjsPrim, s ^. bstCabal, s ^. bstGhcPrim] ++
          resolveCondsHost ((s ^. bstStage1a) ++ (s ^. bstStage1b) ++ (s ^. bstStage2))

main :: IO ()
main = do
    -- temporary warning
    whenM ((==["--init"]) <$> getArgs) (putStrLn "ghcjs-boot has been updated. see README.\nUse `ghcjs-boot --dev' for a development build (if you installed GHCJS from a Git repo) or `ghcjs-boot' for a release build" >> exitFailure)
    settings <- adjustDefaultSettings <$> execParser optParser'
    when (settings ^. bsShowVersion) (printVersion >> exitSuccess)
    hSetBuffering stdout LineBuffering
    setLocaleEncoding utf8
    setForeignEncoding utf8
    env <- initBootEnv settings
    printBootEnvSummary False env
    r <- Sh.shelly $ runReaderT ((actions >> pure Nothing) `catchAny` (pure . Just)) env
    maybe exitSuccess Ex.throwIO r
  where
    actions :: B ()
    actions = verbosely . tracing False $ do
      e <- ask
      when (e ^. beSettings . bsClean) cleanTree
      removeCompleted
      mapM_ addCheckpoint ["ghcjs-boot checkpoints file", "init"]
      installBuildTools
      bool (e ^. beSettings . bsDev) installDevelopmentTree installReleaseTree
      initPackageDB
      cleanCache
      installRts
      installEtc
      installDocs
      installTests
      copyGhcjsPrim
      copyIncludes
      let base = e ^. beLocations . blGhcjsLibDir
      setenv "CFLAGS" $ "-I" <> toTextI (base </> "include")
      installFakes
      installStage1
      unless (e ^. beSettings . bsStage1Unbooted) $ do
        removeFakes
        unless (e ^. beSettings . bsQuick) installStage2
      when (e ^. beSettings . bsHaddock) buildDocIndex
      liftIO . printBootEnvSummary True =<< ask
      unless (e ^. beSettings . bsStage1Unbooted) addCompleted

cleanTree :: B ()
cleanTree = do
  topDir <- view (beLocations . blGhcjsTopDir)
  msg info ("cleaning installation tree " <> toTextI topDir)
  hasCheckpoint "init" >>= cond (rm_rf topDir)
    (failWith ("directory to clean might not be a GHCJS installation directory: " <> toTextI topDir <> ", not cleaning"))

instance Yaml.FromJSON BootSources where
  parseJSON (Yaml.Object v) = BootSources
    <$> v ..: "shims" <*> v ..: "boot" <*> v ..: "test"
    <*> v ..: "etc"                    <*> v ..: "doc"
    <*> v ..: "ghcjs-prim"             <*> v ..: "include"
    <*> v ..: "shims-dev"              <*> v  .: "shims-dev-branch"
    <*> v ..: "ghcjs-boot-dev"         <*> v  .: "ghcjs-boot-dev-branch"
    <*> v ..: "buildtools-windows"     <*> v ..: "buildtools-boot-windows"
    where
      o ..: p     = (nonempty =<< o .: p) <|> ((:[]) <$> o .: p)
      nonempty xs = if null xs then mempty else return xs
  parseJSON _ = mempty

instance Yaml.FromJSON BootPrograms where
  parseJSON (Yaml.Object v) = BootPrograms
    <$> v ..: "ghcjs" <*> v ..: "ghcjs-pkg" <*> v ..: "ghcjs-run"
    <*> v ..: "ghc"   <*> v ..: "ghc-pkg"
    <*> v ..: "cabal" <*> v ..: "node"      <*> v ..: "haddock-ghcjs"
    <*> v ..: "git"   <*> v ..: "alex"
    <*> v ..: "happy" <*> v ..: "tar"
    <*> v ..: "cpp"   <*> v ..: "bash"      <*> v ..: "autoreconf"
    <*> v ..: "make"
    where
      o ..: p = ((\t -> Program p t Nothing Nothing []) <$> o .: p) <|> (withArgs p =<< o .: p)
      withArgs :: Text -> Yaml.Value -> Yaml.Parser (Program a)
      withArgs p (Yaml.Object o) | [(k,v)] <- HM.toList o = Program p k Nothing Nothing <$> Yaml.parseJSON v
      withArgs _ _                                        = mempty
  parseJSON _ = mempty

instance Yaml.FromJSON BootStages where
  parseJSON (Yaml.Object v) = BootStages
    <$> v ..: "stage1a"             <*> v ..: "stage1b" <*> v ..: "stage2"
    <*> v .:: "stage1PretendToHave" <*> v  .: "cabal"   <*> v  .: "ghcjs-prim"
    <*> v  .: "ghc-prim"
    where
      o .:: p = ((:[])<$>o.:p) <|> o.:p
      o ..: p = pkgs Nothing Nothing =<< o .: p
      pkgs plc btc (Yaml.Object o) | [(k,v)] <- HM.toList o = matchCond plc btc k v
      pkgs plc btc (Yaml.String t)                          = pure [CondPackage plc btc t]
      pkgs plc btc (Yaml.Array v)                           = concat <$> mapM (pkgs plc btc) (V.toList v)
      pkgs _   _   _                                        = mempty
      matchCond plc btc k v
        | k == "IfWindows" && plc /= Just Unix    = pkgs (Just Windows) btc          v
        | k == "IfUnix"    && plc /= Just Windows = pkgs (Just Unix)    btc          v
        | k == "IfQuick"   && btc /= Just Full    = pkgs plc            (Just Quick) v
        | k == "IfFull"    && btc /= Just Quick   = pkgs plc            (Just Full)  v
        | otherwise                               = mempty
  parseJSON _ = mempty

instance Yaml.FromJSON BootConfigFile where
  parseJSON (Yaml.Object v) = BootConfigFile
    <$> v .: "packages" <*> v .: "sources" <*> v .: "programs"
  parseJSON _ = mempty

adjustDefaultSettings :: BootSettings -> BootSettings
adjustDefaultSettings s
  | isWindows && isNothing (s ^. bsGmpInclude) && isNothing (s ^. bsGmpLib) = s & bsGmpInTree .~ True
  | otherwise                                                               = s

{-
  We install some build tools automatically if we're on Windows
 -}
installBuildTools :: B ()
installBuildTools
  | not isWindows = return ()
  | otherwise = instBt -- >> setBuildEnv
  where
    instBt = checkpoint' "buildtools" "buildtools already installed" $ do
      subTop $ do
        checkpoint' "mingw" "MingW installation already copied" $ do
          msg info "MingW installation not found, copying from GHC"
          flip cp_r ".." <^> beLocations . blGhcLibDir . to (</> (".." </> "mingw"))
{-
      subTop $ do
        p <- absPath =<< pwd
        checkpoint' "buildtools" "Buildtools already installed" $ do
          checkpoint' "buildtools-boot" "Buildtools bootstrap archive already installed" $
            install' "Windows buildtools bootstrap archive" "buildtools-boot" <^> beSources . bsrcBuildtoolsBootWindows
          prependPathEnv [p </> "buildtools-boot" </> "bin"]
          install' "Windows buildtools" "buildtools" <^> beSources . bsrcBuildtoolsWindows
    setBuildEnv = do
      libDir <- view (beLocations . blGhcjsLibDir)
      cd libDir
      p <- absPath =<< pwd
      let bt = p </> "buildtools"
      mw <- canonicalize (p </> ".." </> "mingw")
      prependPathEnv [ mw </> "bin"
                     , bt </> "bin"
                     , bt </> "msys" </> "1.0" </> "bin"
                     , bt </> "git" </> "bin"
                     ]
      setenv "MINGW_HOME" (toTextI mw)
      setenv "PERL5LIB" (msysPath $ bt </> "share" </> "autoconf")
      mkdir_p (bt </> "etc")
      mkdir_p (bt </> "msys" </> "1.0" </> "mingw")
      writefile (bt </> "msys" </> "1.0" </> "etc" </> "fstab") $ T.unlines
        [ escapePath bt <> " /mingw"
        , escapePath (bt </> "msys" </> "1.0" </> "bin") <> " /bin"
        ]
-}
prependPathEnv :: [FilePath] -> B ()
prependPathEnv xs = do
  path1 <- get_env "Path"
  path2 <- get_env "PATH"
  let path = maybe "" (";"<>) (path1 <> path2)
      newPath = T.intercalate ";" (map toTextI xs) <> path
  setenv "Path" newPath
  setenv "PATH" newPath

-- convert C:\x\y to /c/x/y (only on Windows)
msysPath :: FilePath -> Text
msysPath p
  | isWindows = let p' = toTextI p
                    backToForward '\\' = '/'
                    backToForward x    = x
                    isRel = "." `T.isPrefixOf` p' -- fixme
                in bool isRel "" "/" <> T.map backToForward (T.filter (/=':') p')
  | otherwise = toTextI p

escapePath :: FilePath -> Text
escapePath p = let p' = toTextI p
                   escape ' '  = "\\ "
                   escape '\\' = "/"
                   escape c    = T.singleton c
               in  T.concatMap escape p'

optParser' :: ParserInfo BootSettings
optParser' = O.info (helper <*> optParser)
                    (fullDesc <>
                       header "GHCJS booter, build base libraries for the compiler" <>
                       progDesc description
                    )

description :: String
description = unlines
  [ "ghcjs-boot builds an initial set of libraries for GHCJS."
  ]

optParser :: Parser BootSettings
optParser = BootSettings
            <$> switch ( long "clean"      <> short 'c' <>
                  help "clean the installation directory first" )
            <*> switch ( long "version" <>
                  help "show the ghcjs-boot version" )
            <*> switch ( long "quick"     <> short 'q' <>
                  help "quick boot (no Cabal or ghcjs-base, but enough to compile basic tests)" )
            <*> switch ( long "dev"       <> short 'd' <>
                  help "fetch development sources (requires more build tools)" )
            <*> (optional . option auto) ( long "jobs"   <> short 'j' <> metavar "JOBS" <>
                  help "number of jobs to run in parallel" )
            <*> switch ( long "debug"   <> short 'd' <>
                  help "build debug libraries with extra checks" )
            <*> fmap not (switch ( long "no-prof" <>
                  help "don't generate profiling version of the libraries" ))
            <*> fmap not (switch ( long "no-haddock" <>
                  help "don't generate documentation" ))
            <*> (fmap Verbosity . option auto) ( long "verbosity"   <> short 'v' <> value 2 <>
                  help "verbose output" )
            <*> (optional . fmap T.pack . strOption) ( long "with-iconv-includes" <> metavar "DIR" <>
                  help "directory containing iconv.h" )
            <*> (optional . fmap T.pack . strOption) ( long "with-iconv-libraries" <> metavar "DIR" <>
                  help "directory containing iconv library" )
            <*> (optional . fmap T.pack . strOption) ( long "with-gmp-includes" <> metavar "DIR" <>
                  help "directory containing gmp.h" )
            <*> (optional . fmap T.pack . strOption) ( long "with-gmp-libraries" <> metavar "DIR" <>
                  help "directory containing gmp library" )
            <*> switch ( long "with-gmp-framework-preferred" <>
                  help "on OSX, prefer the GMP framework to the gmp lib" )
            <*> switch ( long "with-intree-gmp" <>
                  help "force using the in-tree GMP" )
            <*> (optional . fmap T.pack . strOption) ( long "with-cabal" <> metavar "PROGRAM" <>
                  help "cabal program to use" )
            <*> (optional . fmap T.pack . strOption) ( long "with-ghcjs-bin" <> metavar "DIR" <>
                  help "bin directory for GHCJS programs" )
            <*> (optional . fmap T.pack . strOption) ( long "with-ghcjs" <> metavar "PROGRAM" <>
                  help "ghcjs program to use" )
            <*> (optional . fmap T.pack . strOption ) ( long "with-ghcjs-pkg" <> metavar "PROGRAM" <>
                  help "ghcjs-pkg program to use" )
            <*> (optional . fmap T.pack . strOption ) ( long "with-ghcjs-run" <> metavar "PROGRAM" <>
                  help "ghcjs-run program to use" )
            <*> (optional . fmap T.pack . strOption) ( long "with-ghc" <> metavar "PROGRAM" <>
                  help "ghc program to use" )
            <*> (optional . fmap T.pack . strOption) ( long "with-node" <> metavar "PROGRAM" <>
                  help "node.js program to use" )
            <*> (optional . fmap T.pack . strOption) ( long "with-datadir" <> metavar "DIR" <>
                  help "data directory with libraries and configuration files" )
            <*> (optional . fmap T.pack . strOption) ( long "with-config" <> metavar "FILE" <>
                  help "boot configuration file (default: boot.yaml in datadir)" )
            <*> (optional . fmap T.pack . strOption ) ( long "shims-dev-repo" <> metavar "REPOSITORY" <>
                  help "override shims repository location" )
            <*> (optional . fmap T.pack . strOption ) ( long "shims-dev-branch" <> metavar "BRANCH" <>
                  help "override shims branch or commit to check out" )
            <*> (optional . fmap T.pack . strOption ) ( long "ghcjs-boot-dev-repo" <> metavar "REPOSITORY" <>
                  help "override ghcjs-boot repository location" )
            <*> (optional . fmap T.pack . strOption ) ( long "ghcjs-boot-dev-branch" <> metavar "BRANCH" <>
                  help "override ghcjs-boot branch or commit to check out" )
            <*> switch ( long "build-stage1-unbooted" <>
                  help "build stage1 packages but leave the compiler in unbooted state (for testing only)" )

initPackageDB :: B ()
initPackageDB = do
  msg info "creating package databases"
  initDB "--global" <^> beLocations . blGlobalDB
  traverseOf_ _Just initUser <^> beLocations . blUserDBDir
  where
    initUser dir = rm_f (dir </> "package.conf") >> initDB "--user" (dir </> "package.conf.d")
    initDB dbName db = do
      rm_rf db >> mkdir_p db
      ghcjs_pkg_ ["init", toTextI db] `catchAny_` return ()
      ghcjs_pkg_ ["recache", dbName]

cleanCache :: B ()
cleanCache =
  liftIO Info.getUserCacheDir >>= \case
    Just p -> rm_rf (fromString p) `catchAny_` return ()
    Nothing -> return ()

bootDescr :: Text
bootDescr = "boot libraries"

shimsDescr :: Text
shimsDescr = "shims, runtime system and support libraries"

installDevelopmentTree :: B ()
installDevelopmentTree = subTop $ do
  p <- pwd
  msgD info $ "preparing development boot tree"
  checkpoint' "ghcjs-boot-git" "ghcjs-boot repository already cloned and prepared" $ do
    testGit "ghcjs-boot" >>= \case
      Just False -> failWith "ghcjs-boot already exists and is not a git repository"
      Just True  -> do
        msg info "ghcjs-boot repository already exists but checkpoint not reached, cleaning first, then cloning"
        rm_rf "ghcjs-boot"
        initGhcjsBoot
      Nothing    -> do
        msgD info "cloning ghcjs-boot git repository"
        initGhcjsBoot
  checkpoint' "shims-git" "shims repository already cloned" $ do
    testGit "shims" >>= \case
      Just False -> failWith "shims already exists and is not a git repository"
      Just True  -> do
        msgD info "shims repository already exists but checkpoint not reached, cleaning first, then cloning"
        rm_rf "shims"
        cloneGit shimsDescr "shims" bsrcShimsDevBranch bsrcShimsDev
      Nothing    -> do
        msgD info "cloning shims git repository"
        cloneGit shimsDescr "shims" bsrcShimsDevBranch bsrcShimsDev
  where
    initGhcjsBoot = sub $ do
      cloneGit bootDescr "ghcjs-boot"  bsrcBootDevBranch bsrcBootDev
      cd "ghcjs-boot"
      git_ ["submodule", "update", "--init", "--recursive"]
      mapM_ patchPackage =<< allPackages
      preparePrimops
      buildGenPrim
      cleanGmp
    testGit d = cond (Just<$>test_d (d</>".git")) (pure Nothing) =<< test_d d
    cloneGit descr repoName branch srcs = do
      msgD info ("cloning git repository for " <> descr)
      cloneGitSrcs descr <^> beSources . srcs
      branch' <- view (beSources . branch)
      sub $ do
        cd repoName
        git_ ["checkout", branch']
    cloneGitSrcs d [] = failWith ("could not clone " <> d <> ", no available sources")
    cloneGitSrcs d (x:xs) = git_ ["clone", x] `catchAny_`
      (msgD warn "clone failed, trying next source" >> cloneGitSrcs d xs)

installReleaseTree :: B ()
installReleaseTree = subTop $ do
  msgD info "preparing release boot tree"
  checkpoint' "ghcjs-boot-release" "ghcjs-boot tree already installed" $ do
    whenM (test_d "ghcjs-boot") (msgD warn "existing ghcjs-boot tree found from incomplete installation")
    install' bootDescr "ghcjs-boot" <^> beSources . bsrcBoot
    preparePrimops
    buildGenPrim
  checkpoint' "shims-release" "shims tree already installed" $ do
    whenM (test_d "shims") (msgD warn "existing shims tree found from incomplete installation")
    install' shimsDescr "shims" <^> beSources . bsrcShims

-- | preprocess primops.txt.pp, one version for the JS platform
--   one for native
preparePrimops :: B ()
preparePrimops = subTop' ("ghcjs-boot" </> "data") . checkpoint' "primops" "primops already prepared" $ do
  msg info "preparing primops"
  mkdir_p "native"
  ghcLibDir <- view (beLocations . blGhcLibDir)
  cp (ghcLibDir </> "include" </> "MachDeps.h") "native"
  cp (ghcLibDir </> "include" </> "ghcautoconf.h") "native"
  cp (ghcLibDir </> "include" </> "ghcplatform.h") ("native" </> "ghc_boot_platform.h")
  silently $ do
    primopsJs <- cpp ["-P", "-Ijs", "primops.txt.pp"]
    writefile "primops-js.txt" primopsJs
    primopsNative <- cpp ["-P", "-Inative", "primops.txt.pp"]
    writefile "primops-native.txt" primopsNative

-- | build the genprimopcode tool, this requires alex and happy
buildGenPrim :: B ()
buildGenPrim = subTop' ("ghcjs-boot" </> "utils" </> "genprimopcode") $ do
  make "genprimopcode" [] $ do
    make "Lexer.hs"  ["Lexer.x"]  (alex_ ["Lexer.x"])
    make "Parser.hs" ["Parser.y"] (happy_ ["Parser.y"])
    ghc_   ["-o", "genprimopcode", "-O", "Main.hs", "+RTS", "-K128M"]

-- fixme this hardcodes the location of integer-gmp
integerGmp :: FilePath
integerGmp = "ghcjs-boot" </> "boot" </> "integer-gmp"

cleanGmp :: B ()
cleanGmp = subTop' integerGmp $ do
  rm_rf ("gmp" </> "intree")
  rm_f ("mkGmpDerivedConstants" </> exe "mkGmpDerivedConstants")
  rm_f "GmpDerivedConstants.h"

prepareGmp :: B ()
prepareGmp = subTop' integerGmp . checkpoint' "gmp" "in-tree gmp already prepared" $ do
  intreeInstalled <- test_f ("gmp" </> "intree" </> "include" </> "gmp.h")
  gmpInTree <- view (beSettings . bsGmpInTree)
  sub $ when (gmpInTree && not intreeInstalled) $ do
    cd "gmp"
    lsFilter "." isGmpSubDir rm_rf
    msg info "unpacking in-tree GMP"
    lsFilter "tarball" (return . isTarball) (installArchive False "in-tree libgmp" ".")
    d <- pwd
    ad <- absPath d
    lsFilter "." isGmpSubDir $ \dir -> do
      -- patch has already been applied
      cd dir
      adir <- absPath dir
      msgD info "building GMP"
      configure_ ["--prefix=" <> msysPath (ad </> "intree")]
      runMake_ []
      runMake_ ["install"]
  make "GmpGeneratedConstants.h" [] $ do
    gmpIncl <- view (beSettings . bsGmpInclude)
    p <- absPath =<< pwd
    buildGmpConstants (gmpIncl `mplus` bj gmpInTree (toTextI $ p </> "gmp" </> "intree" </> "include"))
    where
      lsFilter :: FilePath -> (FilePath -> B Bool) -> (FilePath -> B ()) -> B ()
      lsFilter dir p a = ls dir >>= mapM_ (\x -> p x >>= flip when (a x))
      isTarball file = any (`T.isSuffixOf` toTextI file) [".tar", ".tar.bz2"]
      isGmpSubDir dir = (("gmp-" `T.isPrefixOf`) . toTextI) <$> relativeTo "." dir

buildGmpConstants :: Maybe Text -> B ()
buildGmpConstants includeDir = subTop' integerGmp $ do
  msg info "generating GMP derived constants"
  cd "mkGmpDerivedConstants"
  ghc_ $ maybe [] (\d -> ["-I" <> d]) includeDir ++
    ["-fforce-recomp", "-no-hs-main", "-o", "mkGmpDerivedConstants", "mkGmpDerivedConstants.c"]
  p <- pwd
  constants <- run (Program "" "" Nothing (Just $ p </> "mkGmpDerivedConstants") []) []
  writefile "GmpDerivedConstants.h" constants

patchPackage :: Package -> B ()
patchPackage pkg
  | Just pkg' <- T.stripPrefix "./" (T.strip pkg) =
      let pkgName = last (T.splitOn "/" pkg')
          p       = "patches" </> fromText pkgName <.> "patch"
          applyPatch = do
            msg info ("applying patch: " <> toTextI p)
	    cd (fromText pkg')
            when isWindows (git_ ["config", "core.filemode", "false"])
	    -- workaround for Windows MSYS2 git not liking our absolute paths
	    git_ ["apply", T.replicate (1 + T.count "/" pkg') "../" <>
	                   "patches/" <> pkgName <> ".patch"]
      in  sub $ cond applyPatch (msg info $ "no patch for package " <> pkgName <> " found") =<< test_f p
  | otherwise = return ()

installRts :: B ()
installRts = subTop' "ghcjs-boot" $ do
  msg info "installing RTS"
  globalDB <- view (beLocations . blGlobalDB)
  ghcLib   <- view (beLocations . blGhcLibDir)
  ghcjsLib <- view (beLocations . blGhcjsLibDir)
  ghcjsTop <- view (beLocations . blGhcjsTopDir)
  let inc       = ghcjsLib </> "include"
      incNative = ghcjsLib </> "include_native"
#if __GLASGOW_HASKELL__ >= 709
      rtsLib    = ghcjsLib </> "rts"
#else
      rtsLib    = ghcjsLib </> "rts-1.0"
#endif
  rtsConf <- readfile (ghcLib </> "package.conf.d" </> "builtin_rts.conf")
  writefile (globalDB </> "builtin_rts.conf") (fixRtsConf (toTextI inc) (toTextI rtsLib) rtsConf)
  ghcjs_pkg_ ["recache", "--global", "--no-user-package-db"]
  forM_ [ghcjsLib, inc, incNative] mkdir_p
  sub $ cd (ghcLib </> "include") >> cp_r "." incNative
#if __GLASGOW_HASKELL__ >= 709
  sub $ cd (ghcLib </> "rts") >> cp_r "." rtsLib
#else
  sub $ cd (ghcLib </> "rts-1.0") >> cp_r "." rtsLib
#endif
  sub $ cd ("data" </> "include") >> installPlatformIncludes inc incNative
  cp (ghcLib </> "settings")          (ghcjsLib </> "settings")
  cp (ghcLib </> "platformConstants") (ghcjsLib </> "platformConstants")
  let unlitDest    = ghcjsLib </> exe "unlit"
      ghcjsRunDest = ghcjsLib </> exe "ghcjs-run"
  ghcjsRunSrc <- view (bePrograms . bpGhcjsRun . pgmLoc . to fromJust)
  cp (ghcLib </> exe "unlit") unlitDest
  cp ghcjsRunSrc ghcjsRunDest
  mapM_ (liftIO . Cabal.setFileExecutable . toStringI) [unlitDest, ghcjsRunDest]
  writefile (ghcjsLib </> "node") <^> bePrograms . bpNode . pgmLoc . to (maybe "-" toTextI)
  when (not isWindows) $ do
    let runSh = ghcjsLib </> "run" <.> "sh"
    writefile runSh "#!/bin/sh\nCOMMAND=$1\nshift\n\"$COMMAND\" \"$@\"\n"
    liftIO . Cabal.setFileExecutable . toStringI =<< absPath runSh
  -- required for integer-gmp
  whenM (view (beLocations . blNativeToo)) $ do
    prepareGmp
    cp ("boot" </> "integer-gmp" </> "mkGmpDerivedConstants" </> "GmpDerivedConstants.h") inc
  subTop $ do
    writefile "empty.c" ""
    ghc_ ["-c", "empty.c"]
  when isWindows $ cp (ghcLib </> exe "touchy") (ghcjsLib </> exe "touchy")
  msg info "RTS prepared"

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

copyGhcjsPrim :: B ()
copyGhcjsPrim = checkpoint' "ghcjs-prim" "ghcjs-prim" $
  install' "ghcjs-prim package sources" <$^> beLocations . blGhcjsLibDir . to (</> "ghcjs-prim") <<*^> beSources . bsrcGhcjsPrim

copyIncludes :: B ()
copyIncludes = checkpoint' "includes" "includes" $
  install' "ghcjs rts include files" <$^> beLocations . blGhcjsLibDir . to (</> "include")  <<*^> beSources . bsrcInclude

installEtc :: B ()
installEtc = checkpoint' "additional configuration files" "etc" $ do
  install' "additional configuration files" <$^> beLocations . blGhcjsLibDir <<*^> beSources . bsrcEtc
#ifdef WINDOWS
  -- compile the resources we need for the runner to prevent Windows from trying to detect
  -- programs that require elevated privileges
  ghcjsTop <- view (beLocations . blGhcjsTopDir)
  let windres = Program "windres" "windres" Nothing
                        (Just $ ghcjsTop </> ".." </> "mingw" </> "bin" </> "windres.exe") []
  subTop $ run_ windres ["runner.rc", "-o", "runner-resources.o"]
#endif

installDocs :: B ()
installDocs = checkpoint' "documentation" "doc" $
  install' "documentation" <$^> beLocations . blGhcjsLibDir . to (</>"doc") <<*^> beSources . bsrcDoc

installTests :: B ()
installTests = unlessM (hasCheckpoint "tests") $ do
    msg info "installing test suite"
    (install False "test suite" <$^> beLocations . blGhcjsLibDir . to (</>"test") <<*^> beSources . bsrcTest) >>=
      cond (addCheckpoint "tests") (msg warn "test suite could not be installed, continuing without")

buildDocIndex :: B ()
buildDocIndex = subTop' "doc" $ do
  haddockFiles <- findWhen (return . flip hasExtension "haddock") "."
  haddock_ $ ["--gen-contents", "--gen-index", "-o", "html", "--title=GHCJS Libraries"] ++
    map (\p -> "--read-interface=../" <> toTextI (directory p) <> "," <> toTextI p) haddockFiles

installStage2 :: B ()
installStage2 = subTop' "ghcjs-boot" $ do
  msg info "installing Cabal library"
  removeFakes
  cabalPkg <- view (beStages . bstCabal)
  preparePackage cabalPkg
  cabalInstall [cabalPkg]
  msg info "installing stage 2 packages"
  stage2 <- stagePackages bstStage2
  forM_ stage2 preparePackage
  cabalInstall stage2

installGhcjsPrim :: B ()
installGhcjsPrim = do
  msg info "installing ghcjs-prim"
  prim <- view (beStages . bstGhcjsPrim)
  preparePackage prim
  cabalStage1 [prim]

installStage1 :: B ()
installStage1 = subTop' "ghcjs-boot" $ do
  prim <- view (beStages . bstGhcPrim)
  installStage "0" [prim]
  fixGhcPrim
  installStage "1a" =<< stagePackages bstStage1a
  s <- ask
  when (s ^. beSettings . bsGmpInTree && s ^. beLocations . blNativeToo) installInTreeGmp
  installGhcjsPrim
  installStage "1b" =<< stagePackages bstStage1b
  resolveWiredInPackages
    where
      fixGhcPrim = do
        descr <- T.lines <$> ghcjs_pkg ["describe", "ghc-prim", "--no-user-package-db"]
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
   Left err   -> failWith ("error parsing wired-in packages file wiredinpkgs.yaml\n" <> T.pack err)
   Right pkgs -> do
     pkgs' <- forM pkgs $ \p ->
       (p,) . T.strip <$> ghcjs_pkg [ "--simple-output"
                                    , "field"
                                    , p
#if __GLASGOW_HASKELL__ >= 709
                                    , "key"
#else
                                    , "id"
#endif
                                    ]
     writefile ("wiredinkeys" <.> "yaml") $
       T.unlines ("# resolved wired-in packages" :
                  map (\(p,k) -> p <> ": " <> k) pkgs')

-- fixme: urk, this is probably not how it's supposed to be done
installInTreeGmp :: B ()
installInTreeGmp = subTop' integerGmp $ do
  p <- absPath =<< pwd
  let gmpLib = p </> "gmp" </> "intree" </> "lib" </> "libgmp.a"
  libPath <- ghcjs_pkg ["field", "integer-gmp", "library-dirs", "--simple-output", "--no-user-package-db"]
  libPath' <- canonic (fromText $ T.strip libPath)
  msg info $ "installing in-tree gmp: " <> toTextI gmpLib <> " -> " <> toTextI libPath'
  cp gmpLib libPath'
  descr <- T.lines <$> ghcjs_pkg ["describe", "integer-gmp", "--no-user-package-db"]
  let updateLine line | "extra-libraries:" `T.isPrefixOf` line = line <> " gmp"
                      | otherwise                              = line
  setStdin (T.unlines $ map updateLine descr)
  ghcjs_pkg_ ["update", "-", "--global", "--no-user-package-db"]

preparePackage :: Package -> B ()
preparePackage pkg
  | "./" `T.isPrefixOf` pkg || "../" `T.isPrefixOf` pkg = sub $ do
    msg trace ("preparing package " <> pkg)
    cd (fromText pkg)
    whenM (test_f "configure.ac") $
      make "configure" ["configure.ac"]
        (msg info ("generating configure script for " <> pkg) >> autoreconf_)
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
          Nothing -> failWith ("cannot find package id of " <> pkg <> "-" <> version)
          Just pkgId -> do
            globalDB <- view (beLocations . blGlobalDB)
            libDir   <- view (beLocations . blGhcjsLibDir)
            let conf = fakeConf libDir libDir pkg version pkgId
            writefile (globalDB </> fromText pkgId <.> "conf") conf
  ghcjs_pkg_ ["recache", "--global", "--no-user-package-db"]

findPkgId:: [Text] -> Text -> Text -> Maybe Text
findPkgId dump pkg version =
  listToMaybe (filter (pkgVer `T.isPrefixOf`) ids)
    where
      pkgVer = pkg <> "-" <> version <> "-"
      ids = map (T.dropWhile isSpace . T.drop 3) $ filter ("id:" `T.isPrefixOf`) dump

fakeConf :: FilePath -> FilePath -> Text -> Text -> Text -> Text
fakeConf incl lib name version pkgId = T.unlines
            [ "name:           " <> name
            , "version:        " <> version
            , "id:             " <> pkgId
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
  pkgs <- T.words <$> ghcjs_pkg ["list", "--simple-output", "--no-user-package-db"]
  forM_ pkgs $ \p -> when (any (`T.isPrefixOf` p) fakes)
    (msg info ("unregistering " <> p) >> ghcjs_pkg_ ["unregister", p, "--no-user-package-db"])

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

-- | unpack a tar file (does not support compression)
--   only supports files, does not try to emulate symlinks
unpackTar :: Bool     -- ^ strip the first directory component?
          -> FilePath -- ^ destination to unpack to
          -> FilePath -- ^ the tar file
          -> B ()
unpackTar stripFirst dest tarFile = do
  mkdir_p dest
  entries <- Tar.read . BL.fromStrict <$> readBinary tarFile
  void $ Tar.foldEntries (\e -> (>>=checkExtract e)) (return Nothing) (\e -> failWith $ "error unpacking tar: " <> showT e) entries
    where
      dropComps = if stripFirst then 1 else 0
      failSec e msg = failWith $ "tar security check, " <> msg <> ": " <> T.pack (Tar.entryPath e)
      checkExtract e Nothing
        | (p:_) <- System.FilePath.splitDirectories (Tar.entryPath e)
                                        = checkExtract e (Just p)
        | otherwise                     = failSec e "no path"
      checkExtract e je@(Just expected)
        | System.FilePath.isAbsolute ep = failSec e "absolute path"
        | any (=="..") epd              = failSec e "'..' in path"
        | listToMaybe epd /= je && isSupportedEntry (Tar.entryContent e)
                                        = failSec e ("tar bomb, expected path component: " <> T.pack expected)
        | otherwise                     = do
            view (beSettings . bsVerbosity) >>= \v ->
              -- this gets chatty, reduce verbosity for file writes / directory creates here unless we're at trace level
              (if v < trace then quieter warn else id)
                (extractEntry e $ dest </> fromString (System.FilePath.joinPath (drop (if stripFirst then 1 else 0) epd)))
            return je
        where ep  = Tar.entryPath e
              epd = System.FilePath.splitDirectories ep
      isSupportedEntry (Tar.NormalFile{}) = True
      isSupportedEntry (Tar.Directory{})  = True
      isSupportedEntry _                  = False
      extractEntry e tgt
        | Tar.NormalFile bs size <- Tar.entryContent e = do
            mkdir_p (directory tgt)
            writeBinary tgt bs
            setPermissions (Tar.entryPermissions e) tgt
        | Tar.Directory <- Tar.entryContent e = do
            mkdir_p tgt
            setPermissions (Tar.entryPermissions e) tgt
        | otherwise =
            msg warn ("ignoring unexpected entry type in tar. only normal files and directories (no links) are supported:\n    " <> toTextI tgt)
      setPermissions mode tgt = do
        absTgt <- absPath tgt
        msgD trace ("setting permissions of " <> toTextI tgt <> " to " <> showT mode)
        let tgt  = toStringI absTgt
            tgt' = bool (last tgt `elem` ['/','\\']) (init tgt) tgt
        liftIO (setFileMode tgt' mode)


ghc_         = runE_ bpGhc
ghc_pkg      = runE  bpGhcPkg
ghcjs_pkg    = runE  bpGhcjsPkg
ghcjs_pkg_   = runE_ bpGhcjsPkg
alex_        = runE_ bpAlex
happy_       = runE_ bpHappy
haddock_     = runE_ bpHaddock
tar_         = runE_ bpTar
git_         = runE_ bpGit
cpp          = runE  bpCpp
cabal        = runE  bpCabal
cabal_       = runE_ bpCabal

runE  g a = view (bePrograms . g) >>= flip run  a
runE_ g a = view (bePrograms . g) >>= flip run_ a

cabalStage1 :: [Text] -> B ()
-- | stage 1 cabal install: boot mode, hand off to GHC if GHCJS cannot yet compile it
cabalStage1 pkgs = sub $ do
  ghc <- requirePgmLoc =<< view (bePrograms . bpGhc)
  s   <- view beSettings
  p   <- pwd
  setenv "GHCJS_BOOTING" "1"
  setenv "GHCJS_BOOTING_STAGE1" "1"
  setenv "GHCJS_WITH_GHC" (toTextI ghc)
  let configureOpts = catMaybes $ [("--with-iconv-includes="  <>)<$>s^.bsIconvInclude
                                  ,("--with-iconv-libraries=" <>)<$>s^.bsIconvLib
                                  ,("--with-gmp-includes="    <>)<$>(s^.bsGmpInclude<>inTreePath p "include")
                                  ,("--with-gmp-libraries=  " <>)<$>s^.bsGmpLib
                                  ] ++ gmpOpts
      -- fixme this hardcodes the location of integer-gmp
      inTreePath p sub =
        bj (s^.bsGmpInTree) (toTextI (p </> "boot" </> "integer-gmp" </> "gmp" </> "intree"  </> sub))
      gmpOpts = [bj (s^.bsGmpFramework) "--with-gmp-framework-preferred"
                ,bj (s^.bsGmpInTree)    "--with-intree-gmp"
                ]
  globalFlags <- cabalGlobalFlags
  flags <- cabalInstallFlags (length pkgs == 1)
  let args = globalFlags ++ ("install" : pkgs) ++
             [ "--solver=topdown" -- the modular solver refuses to install stage1 packages
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
  setenv "GHCJS_BOOTING" "1"
  let args = globalFlags ++ "install" : pkgs ++ flags
  checkInstallPlan pkgs args
  cabal_ args

-- check that Cabal is only going to install the packages we specified
-- uses somewhat fragile parsing of --dry-run output, find a better way
checkInstallPlan :: [Package] -> [Text] -> B ()
checkInstallPlan pkgs opts = do
  plan <- cabal (opts ++ ["-v2", "--dry-run"])
  when (hasReinstalls plan || hasUnexpectedInstalls plan || hasNewVersion plan) (err plan)
  where
    hasReinstalls = T.isInfixOf "(reinstall)"   -- reject reinstalls
    hasNewVersion = T.isInfixOf "(new version)" -- only allow one version of each package during boot
    hasUnexpectedInstalls plan =
      let ls = filter ("(new package)" `T.isInfixOf`) (T.lines plan)
      in  length ls /= length pkgs || not (all isExpected ls)
    isExpected l
      | (w:_) <- T.words l, ps@(_:_) <- T.splitOn "-" w =
          any (T.intercalate "-" (init ps) `T.isInfixOf`) pkgs
      | otherwise = False
    err plan = failWith $ "unacceptable install plan, expecting exactly the following list of packages to be installed,\n" <>
                          "without reinstalls and only one version of each package in the database:\n\n" <>
                          T.unlines (map ("  - " <>) pkgs) <> "\nbut got:\n\n" <> plan

cabalGlobalFlags :: B [Text]
cabalGlobalFlags = do
  instDir  <- view (beLocations . blGhcjsTopDir)
  return [ "--config-file", toTextI (instDir </> "cabalBootConfig")
         , "--ignore-sandbox"
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
-- workaround for hoogle support being broken in haddock for GHC 7.10RC1
#if !(__GLASGOW_HASKELL__ >= 709)
           , "--haddock-hoogle"
#endif
           , "--haddock-hyperlink-source"
-- don't slow down Windows builds too much, on other platforms we get this more
-- or less for free, thanks to dynamic-too
#ifndef WINDOWS
           , "--enable-shared"
#endif
           , bool prof "--enable-library-profiling" "--disable-library-profiling"
           ] ++
           bool isWindows [] ["--root-cmd", toTextI (instDir </> "run" <.> "sh")] ++
           -- workaround for Cabal bug?
           bool isWindows ["--disable-executable-stripping", "--disable-library-stripping"] [] ++
           catMaybes [ (((bool parmakeGhcjs "--ghcjs-options=-j" "-j")<>) . showT) <$> j
                     , bj debug "--ghcjs-options=-debug"
                     , bj (v > info) "-v2"
                     ]

configure_  = run_ (Program "configure" "./configure" Nothing (Just "./configure") [])
#ifdef WINDOWS
autoreconf_ = runE_ bpBash ["autoreconf"]
#else
autoreconf_ = runE_ bpAutoreconf []
#endif
runMake_    = runE_ bpMake

ignoreExcep a = a `catchAny` (\e -> msg info $ "ignored exception: " <> showT e)

stagePackages :: Getter BootStages Stage -> B [Package]
stagePackages l = do
  quick    <- view (beSettings . bsQuick)
  condPkgs <- view (beStages . l)
  return (resolveConds quick condPkgs)

whenM :: Monad m => m Bool -> m () -> m ()
whenM c m = c >>= flip when m

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM c m = c >>= flip unless m

make :: FilePath   -- ^ target, build this file if not exists
     -> [FilePath] -- ^ also build if any of these is newer than the target (ignored if they don't exist)
     -> B ()       -- ^ action to run for building
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

install' :: Text -> FilePath -> [Text] -> B ()
install' descr dest srcs = void (install True descr dest srcs)

-- | install some files, from multiple sources with fallback
install :: Bool     -- ^ install is required, exit with a panic if it didn't succeed
        -> Text     -- ^ description, for progress output
        -> FilePath -- ^ destination
        -> [Text]   -- ^ sources, can be tar files, directories, tar.gz / tar.xz works with external tools
        -> B Bool   -- ^ whether installation was succesful
install req descr dest []
  | req       = failWith ("cannot install " <> descr <> " to " <> toTextI dest <> " , no more sources")
  | otherwise = return False
install req descr dest (s:ss)
  | "http://" `T.isPrefixOf` s = withTmpDir $
      \t -> let file = t </> fromText (last $ T.split (=='/') s)
            in  fetch descr file s >>= cond
                  (install req descr dest (toTextI file:ss))
                  (msg warn ("could not fetch " <> s <> ", trying next source") >> install req descr dest ss)
  | otherwise = do
        let s' = fromText s
        d <- test_d s'
        if d then do
               msg info ("installing " <> descr <> ", copying directory: " <> s <> " -> " <> toTextI dest)
               mkdir_p dest >> ls s' >>= mapM_ (\file -> cp_r (s' </> filename file) (dest </> filename file))
               return True
             else do
               f <- test_f s'
               if f then do
                      size <- filesize s'
                      if size == 0
                         then do
                           isDev <- view (beSettings . bsDev)
                           if isDev
                               then msg info ("source " <> s <> " for " <> descr <> " is empty, trying next")
                               else msg warn $ T.unlines
                                      [ "Archive file " <> s <> " for " <> descr <> " is empty."
                                      , "You might be missing the required cache archives for doing a release build."
                                      , "Use `ghcjs-boot --dev' if you installed GHCJS from a Git repository."
                                      ]
                           install req descr dest ss
                         else installArchive True descr dest s' >> return True
                    else do
                         msg trace ("source " <> s <> " for " <> descr <> " does not exist, trying next")
                         install req descr dest ss

-- | install files from an archive
installArchive :: Bool
               -> Text
               -> FilePath
               -> FilePath
               -> B ()
installArchive stripFirst descr dest src
  | suff ".tar"     = do
      msg info ("installing " <> descr <> " unpacking tar (internal) " <> s <> " -> " <> d)
      unpackTar stripFirst dest src
  | suff ".tar.gz"  = m "tar.gz"  >> untar "-xzf"
  | suff ".tar.bz2" = m "tar.bz2" >> untar "-xjf"
  | suff ".tar.xz"  = m "tar.xz"  >> untar "-xJf"
  | otherwise       = failWith ("unknown archive type installing " <> descr <> ": " <> s)
  where
    m e    = msg info ("installing " <> descr <> " unpacking " <> e <> " " <> s <> " -> " <> d)
    suff e = e `T.isSuffixOf` s
    d      = toTextI dest
    s      = toTextI src
    str    = bool stripFirst ["--strip-components=1"] []
    untar o = sub (absPath src >>= \as -> mkdir_p dest >> cd dest >> tar_ ([o, msysPath as] ++ str))

-- | download a file over HTTP
fetch :: Text     -- ^ description
      -> FilePath -- ^ target
      -> Text     -- ^ url to download
      -> B Bool   -- ^ True if the file was downloaded succesfully
fetch descr dest url
  | Just u <- parseURI (T.unpack url) = do
      msg info ("installing " <> descr <> ", downloading " <> url <> " -> " <> toTextI dest)
      liftIO (download u) >>= \case
        Nothing -> return False
        Just r | (2,_,_) <- rspCode r -> do
                 msg info ("finished downloading, status " <> (T.pack . show . rspCode $ r) <> " writing file")
                 writeBinary dest (BL.fromStrict $ rspBody r)
                 return True
             | otherwise -> do
                 msg info ("file not downloaded, status " <> (T.pack . show . rspCode $ r))
                 return False
  | otherwise = return False
  where
    download :: URI -> IO (Maybe (Response B.ByteString))
    download u =
      (Just . snd <$> (Br.browse $ Br.setAllowRedirects True >> Br.request (mkRequest GET u)))
        `Ex.catch` \(Ex.SomeException _) -> return Nothing

-- | initialize our boot environment by reading the configuration files, finding all programs
initBootEnv :: BootSettings -> IO BootEnv
initBootEnv bs = do
  dataDir <- bootDataDir bs
  env     <- (traverse . both %~ T.pack) <$> getEnvironment
  -- substitute some values in our config files
  let subst = [ ("datadir", toTextI dataDir)
              , ("version", T.pack Info.getCompilerVersion)
              ]
      substituteConfig c = c & template %~ substText
                             & template . iso toTextI fromText %~ substText
      substText = Utils.substPatterns subst env
  BootConfigFile stgs srcs pgms1 <- substituteConfig <$> readBootConfigFile bs
  let srcs' = configureBootSources bs srcs
  pgms2 <- configureBootPrograms bs srcs' pgms1
  locs  <- configureBootLocations bs pgms2
  return (BootEnv bs srcs' locs pgms2 stgs)

-- | configure the sources
configureBootSources :: BootSettings -> BootSources -> BootSources
configureBootSources bs srcs =
  srcs & bsrcShimsDev       %~ override (const . (:[])) bsShimsDevRepo
       & bsrcShimsDevBranch %~ override const bsShimsDevBranch
       & bsrcBootDev        %~ override (const . (:[])) bsBootDevRepo
       & bsrcBootDevBranch  %~ override const bsBootDevBranch
  where override f l = maybe id f (bs^.l)

-- | configure the locations
configureBootLocations :: BootSettings
                       -> BootPrograms
                       -> IO BootLocations
configureBootLocations bs pgms = do
  ghcLibDir    <- fromText . T.strip <$> run' bs (pgms ^. bpGhc)   ["--print-libdir"]
  ghcjsLibDir  <- fromText . T.strip <$> run' bs (pgms ^. bpGhcjs) ["--ghcjs-booting-print", "--print-libdir"]
  ghcjsTopDir  <- fromText . T.strip <$> run' bs (pgms ^. bpGhcjs) ["--ghcjs-booting-print", "--print-topdir"]
  globalDB     <- fromText . T.strip <$> run' bs (pgms ^. bpGhcjs) ["--ghcjs-booting-print", "--print-global-db"]
  userDBT      <- T.strip <$> run' bs (pgms ^. bpGhcjs) ["--ghcjs-booting-print", "--print-user-db-dir"]
  nativeToo    <- (=="True") . T.strip <$> run' bs (pgms ^. bpGhcjs) ["--ghcjs-booting-print", "--print-native-too"]
  when (T.null (toTextI ghcjsLibDir)) $
    failWith ("Could not determine GHCJS library installation path.\n" <>
              "Make sure that the ghcjs wrapper script (or options file on Windows) " <>
              "has been set up correctly.")
  return $ BootLocations ghcjsTopDir ghcjsLibDir ghcLibDir globalDB
                           (bool (userDBT == "<none>") Nothing (Just $ fromText userDBT))
                           nativeToo

-- | build the program configuration and do some sanity checks
configureBootPrograms :: BootSettings    -- ^ command line settings
                      -> BootSources
                      -> BootPrograms    -- ^ default programs from config file
                      -> IO BootPrograms -- ^ configured programs
configureBootPrograms bs srcs pgms0 = do
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
                    & bpCabal    %~ r bsWithCabal
                    & bpNode     %~ r bsWithNode
  -- resolve all programs
  pgms3 <- mapMOf tpo (resolveProgram bs) =<< mapMOf tpr (resolveProgram bs) pgms2
  traverseOf_ tpr (reportProgramLocation bs) pgms3
  traverseOf_ tpo (reportProgramLocation bs) pgms3
  pgms4 <- checkProgramVersions bs pgms3
  checkCabalSupport bs pgms4
  return pgms4

-- | resolves program
resolveProgram :: MaybeRequired (Program a) => BootSettings -> Program a -> IO (Program a)
resolveProgram bs pgm = do
  let search' = pgm ^. pgmSearch . to fromText
  absSearch <- (</> search') <$> getWorkingDirectory
  let searchPaths = catMaybes [ Just search'
                              , bj (relative search' && length (splitDirectories search') > 1) absSearch
                              ]
  fmap catMaybes (mapM (findExecutable . encodeString) searchPaths) >>= \case
      (p':_)                  -> (\cp -> pgm & pgmLoc .~ Just cp) <$> {- canonicalizePath -} return (fromString p')
      _      | isRequired pgm -> failWith ("program " <> pgm ^. pgmName <>
                                           " is required but could not be found at " <> pgm ^. pgmSearch)
             | otherwise      -> return (pgm & pgmLoc .~ Nothing)

-- | report location of a configured program
reportProgramLocation :: BootSettings -> Program a -> IO ()
reportProgramLocation bs p
  | Just l <- p ^. pgmLoc = msg' bs info ("program " <> p ^. pgmName <> " found at " <> toTextI l)
  | otherwise             = msg' bs info ("program " <> p ^. pgmName <> " NOT found, searched for " <> p ^. pgmSearch)

-- | check that the GHC, ghcjs and ghcjs-pkg we're using are the correct version
checkProgramVersions :: BootSettings -> BootPrograms -> IO BootPrograms
checkProgramVersions bs pgms = do
  pgms' <- foldrM verifyVersion pgms
    [ (bpGhcjs,    "--numeric-version",       Just Info.getCompilerVersion,    True)
    , (bpGhcjs,    "--numeric-ghc-version",   Just Info.getGhcCompilerVersion, False)
    , (bpGhc,      "--numeric-version",       Just Info.getGhcCompilerVersion, True)
    , (bpGhcjsPkg, "--numeric-ghcjs-version", Nothing,                         True)
    , (bpGhcjsPkg, "--numeric-ghc-version",   Just Info.getGhcCompilerVersion, False)
    , (bpCabal,    "--numeric-version",       Nothing,                         True)
    , (bpNode,     "--version",               Nothing,                         True)
    ]
  verifyNotProfiled
  verifyNodeVersion pgms'
  where
    verifyNotProfiled :: IO ()
    verifyNotProfiled = return ()
    --  res <- T.strip <$> run' bs (pgms ^. bpGhcjs) ["--ghcjs-booting-print", "--print-rts-profiled"]
    --  when (res /= "False") $ failWith ("GHCJS program " <> pgms ^. bpGhcjs . pgmLocText <>
    --                                    " has been installed with executable profiling.\n" <>
    --                                    "You need a non-profiled executable to boot")
    verifyVersion :: (Lens' BootPrograms (Program a), Text, Maybe String, Bool) -> BootPrograms -> IO BootPrograms
    verifyVersion (l, arg :: Text, expected :: Maybe String, update :: Bool) ps = do
      res <- T.strip <$> run' bs (ps ^. l) [arg]
      case expected of
        Nothing -> return ()
        Just exp -> when (res /= T.pack exp) $
          failWith ("version mismatch for program " <> ps ^. l . pgmName <> " at " <> ps ^. l . pgmLocText
                      <> ", expected " <> T.pack exp <> " but got " <> res)
      return $ (if update then (l . pgmVersion .~ Just res) else id) ps
    verifyNodeVersion pgms = do
      let verTxt = fromMaybe "-" (pgms ^. bpNode . pgmVersion)
          v      = mapM (readMaybe . T.unpack . T.dropWhile (== 'v')) . T.splitOn "." . T.takeWhile (/='-') $ verTxt :: Maybe [Integer]
      case v of
        Just (x:y:z:_)
          | x > 0 || y > 10 || (y == 10 && z >= 28) -> return pgms
          | otherwise -> failWith ("minimum required version for node.js is 0.10.28, found: " <> verTxt)
        _             -> failWith ("unrecognized version for node.js: " <> verTxt)

-- | check that cabal-install supports GHCJS and that our boot-GHC has a Cabal library that supports GHCJS
checkCabalSupport :: BootSettings -> BootPrograms -> IO ()
checkCabalSupport bs pgms = do
  cbl <- run' bs (pgms ^. bpCabal) ["install", "--help"]
  when (not $ "--ghcjs" `T.isInfixOf` cbl) $
    failWith ("cabal-install program " <> pgms ^. bpCabal . pgmLocText <> " does not support GHCJS")
  void (run' bs (pgms ^. bpGhc) ["-e", "either error id (Text.Read.readEither \"GHCJS\" :: Either String Distribution.Simple.CompilerFlavor)"]) `Ex.catch`
    \(Ex.SomeException _) -> failWith
       ("GHC program " <> pgms ^. bpGhc . pgmLocText <> " does not have a Cabal library that supports GHCJS\n" <>
        "(note that the Cabal library is not the same as the cabal-install program, you need a compatible version for both)")

-- | read the boot configuration yaml file
readBootConfigFile :: BootSettings -> IO BootConfigFile
readBootConfigFile bs = do
  bf <- bootConfigFile bs
  msgD' bs trace ("reading file " <> toTextI bf)
  b <- B.readFile (toStringI bf)
  case Yaml.decodeEither b of
    Left err  -> failWith ("error parsing boot configuration file " <> toTextI bf <> "\n" <> T.pack err)
    Right bss -> return bss

printBootEnvSummary :: Bool -> BootEnv -> IO ()
printBootEnvSummary after be = do
  section "Boot libraries installation for GHCJS" $ do
    bootLoc  <- getExecutablePath
    bootMod  <- getModified (fromString bootLoc)
    bootConf <- bootConfigFile (be ^. beSettings)
    ghcjsMod <- maybe (return "<unknown>") (fmap show . getModified) (be ^. bePrograms . bpGhcjs . pgmLoc)
    curDir   <- getWorkingDirectory
    p $ bool after
          ["ghcjs-boot has installed the libraries and runtime system for GHCJS"]
          ["ghcjs-boot will install the libraries and runtime system for GHCJS"]
    h "boot program"
    t "rl" [["ghcjs-boot program version", Info.getCompilerVersion]
           ,["file location", bootLoc]
           ,["last modified", show bootMod],[]
           ,["using configuration file", toStringI bootConf]
           ,["current directory", toStringI curDir]
           ]
    h "boot configuration"
    t "rl" [["installation directory", path $ beLocations . blGhcjsTopDir]
           ,["global package DB", path $ beLocations . blGlobalDB]
           ,["user package DB location", path $ beLocations . blUserDBDir . to (fromMaybe "<none>")],[]
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
           ,["quick boot", y isQuick]
           ,["clean tree first", be ^. beSettings . bsClean . to y]
           ,["development boot", y isDev]
           ,["native too", be ^. beLocations . blNativeToo . to y]
           ]
    h "packages"
    p ["stage 1a"] >> l (stg bstStage1a)
    p ["ghcjs-prim:  " ++ be ^. beStages . bstGhcjsPrim . to str]
    p ["stage 1b"] >> l (stg bstStage1b)
    when (not isQuick) $ do
        p ["Cabal:  " ++ be ^. beStages . bstCabal . to str]
        p ["stage 2"] >> l (stg bstStage2)
  section "Configured programs" $ do
    t "hlll" $ ["program", "version", "location"] :
      be ^.. bePrograms . (template :: Traversal' BootPrograms (Program Required)) . to pgm ++
      be ^.. bePrograms . (template :: Traversal' BootPrograms (Program Optional)) . to pgm
  section "Installation sources" $ do
    t "rl" $ concatMap (\(t,l) -> [t,""] : be ^.. beSources . l . traverse . to (\x->["",str x]) ++ [["",""]])
      [("shims (runtime system)", bsrcShims), ("boot libraries", bsrcBoot), ("test suite", bsrcTest), ("configuration files", bsrcEtc), ("documentation", bsrcDoc)] ++
      [["bootstrap GHC library path",""],["", path $ beLocations . blGhcLibDir]]
    when isWindows $ do
      h "Windows development tools"
      t "rl" $ ["development tools",""] : be ^.. beSources . bsrcBuildtoolsWindows     . traverse . to (\x->["",str x]) ++
               ["bootstrap package",""] : be ^.. beSources . bsrcBuildtoolsBootWindows . traverse . to (\x->["",str x])
    when (isDev) $ do
      h "development source repositories"
      p ["shims (" ++ be ^. beSources . bsrcShimsDevBranch . to str ++ ")"]
      l (be ^.. beSources . bsrcShimsDev . traverse . to str)
      p ["ghcjs-boot (" ++ be ^. beSources . bsrcBootDevBranch . to str ++ ")"]
      l (be ^.. beSources . bsrcBootDev . traverse . to str)
  where
    stg s       = be ^.. beStages . s . to (resolveConds isQuick) . traverse . to str
    isDev       = be ^. beSettings . bsDev
    isQuick     = be ^. beSettings . bsQuick
    h xs        = b >> mapM_ (putStrLn . indent 2) [xs, replicate (length xs) '-'] >> b
    p xs        = mapM_ (putStrLn . indent 3) xs >> b
    l xs        = mapM_ (putStrLn . indent 3 . ("- "++)) xs >> b
    t :: String -> [[String]] -> IO ()
    t aln xxs   = let colWidths = map (foldl' (\m xs -> max m (length xs)) 0) (transpose xxs)
                      (colAlign,hdr) = case aln of
                        ('h':a) -> (a, True)
                        a       -> (a, False)
                      colSep    = replicate 3 ' '
                      cell w a xs = let pad = sp (w - length xs) in if a == 'r' then pad ++ xs else xs ++ pad
                      cols xs   = sp 3 ++ intercalate (sp 3) xs
                      row xs    = cols (zipWith3 cell colWidths colAlign xs)
                  in case (xxs, hdr) of
                      (x:ys, True) -> putStrLn (row x) >> putStrLn (cols $ map sp colWidths) >> mapM_ (putStrLn . row) ys >> b
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
    pgm x       = [x ^. pgmName . to str, maybe "-" T.unpack (x ^. pgmVersion) , x ^. pgmLocString]

-- | boot.yaml
bootConfigFile :: BootSettings -> IO FilePath
bootConfigFile bs
  | Just bsf <- bs ^. bsWithConfig = return (fromText bsf)
  | otherwise                      = (</> ("lib" </> "etc" </> "boot" <.> "yaml")) <$> bootDataDir bs

bootDataDir :: BootSettings -> IO FilePath
bootDataDir bs
  | Just dd <- bs ^. bsWithDataDir = return (fromText dd)
  | otherwise                      = fromString <$> Info.ghcjsBootDefaultDataDir

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
cp f t         = msgD info ("cp "      <> toTextI f <> " -> " <> toTextI t) >> lift (Sh.cp f t)
cp_r f t       = msgD info ("cp_r "    <> toTextI f <> " -> " <> toTextI t) >> lift (Sh.cp_r f t)
rm_f p         = msgD info ("rm_f "    <> toTextI p) >> lift (Sh.rm_f p)
rm_rf p        = msgD info ("rm_rf "   <> toTextI p) >> lift (Sh.rm_rf p)
cd p           = msgD trace ("cd "     <> toTextI p) >> lift (Sh.cd p)
sub            = liftE  Sh.sub
test_d         = lift . Sh.test_d
test_f         = lift . Sh.test_f
test_s         = lift . Sh.test_s
run p xs       = msgD info (traceRun p xs) >> requirePgmLoc p >>= \loc -> lift (Sh.run  loc (p ^. pgmArgs ++ xs))
run_ p xs      = msgD info (traceRun p xs) >> requirePgmLoc p >>= \loc -> lift (Sh.run_ loc (p ^. pgmArgs ++ xs))
readBinary p   = msgD trace ("reading " <> toTextI p) >> lift (Sh.readBinary p)
canonic        = lift . Sh.canonic
absPath        = lift . Sh.absPath
pwd            = lift   Sh.pwd
silently       = liftE  Sh.silently
verbosely      = liftE  Sh.verbosely
tracing b      = liftE  (Sh.tracing b)
findWhen f p   = ask >>= \e -> lift (Sh.findWhen (runB e . f) p)
errorExit      = lift . Sh.errorExit
writefile p t  = msgD info  ("writing " <> toTextI p) >> lift (Sh.writefile p t)
appendfile p t = msgD info ("appending " <> toTextI p) >> lift (Sh.appendfile p t)
readfile p     = msgD trace ("reading " <> toTextI p) >> lift (Sh.readfile p)
withTmpDir     = liftE2 Sh.withTmpDir
catchAny a h   = ask >>= \e -> lift (Sh.catchany_sh (runReaderT a e) (\ex -> runReaderT (h ex) e))
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
traceRun p xs = "[" <> p ^. pgmName <> "]: " <> p ^. pgmLocText <> " " <> T.intercalate " " (map (showT . T.unpack) xs)

-- | add a checkpoint to the file
addCheckpoint :: Text -> B ()
addCheckpoint name = unlessM (hasCheckpoint name) $ do
  mkdir_p =<< view (beLocations . blGhcjsTopDir)
  flip appendfile (name <> "\n") =<< checkpointFile

-- | check whether we have passed a checkpoint. this reads the
--   whole checkpoints file so use sparingly
hasCheckpoint :: Text -> B Bool
hasCheckpoint name =
  (((name `elem`) . map T.strip . T.lines) <$> (readfile =<< checkpointFile)) `catchAny`
     \e -> msg warn ("no checkpoint " <> name <> " because of " <> showT e) >> return False

-- | perform the action if the checkpoint does not exist,
--   add the checkpoint when the action completes without exceptions
checkpoint :: Text -> B () -> B ()
checkpoint name m = unlessM (hasCheckpoint name) (m <* addCheckpoint name)

checkpoint' name txt m = hasCheckpoint name >>= cond (msg info txt) (m <* addCheckpoint name)

checkpointFile :: B FilePath
checkpointFile =
  absPath . (</> ("ghcjs_boot" <.> "charlie")) =<< view (beLocations . blGhcjsTopDir)

addCompleted :: B ()
addCompleted = do
  t <- cond "quick" "full" <$^> beSettings . bsQuick
  f <- completedFile
  writefile f t

removeCompleted :: B ()
removeCompleted = rm_f =<< completedFile

completedFile :: B FilePath
completedFile =
  absPath . (</> ("ghcjs_boot" <.> "completed")) =<< view (beLocations . blGhcjsTopDir)

requirePgmLoc :: Program a -> B FilePath
requirePgmLoc p
  | Just loc <- p ^. pgmLoc = return loc
  | otherwise = do
        -- search in original path, where we configured the programs. the shelly path might be local
        path <- fromMaybe "" <$> liftIO (Utils.getEnvMay "PATH")
        failWith $ "program " <> p ^. pgmName <> " is required but was not found\n" <>
                   "  name searched for (from boot.yaml or command line): " <> p ^. pgmSearch <> "\n" <>
                   "  searched in PATH:\n" <> T.pack path

run' :: BootSettings -> Program a -> [Text] -> IO Text
run' bs p xs = do
  msgD' bs info (traceRun p xs)
  (e, out, _err) <- readProcessWithExitCode (p ^. pgmLocString) (map T.unpack xs) ""
  when (e /= ExitSuccess) (failWith $ "program " <> p ^. pgmLocText <> " returned a nonzero exit code")
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
(<^>) :: MonadReader s m => (a -> m b) -> Getting a s a -> m b
(<^>) m l = m =<< view l

infixl 3 <*^>
(<*^>) :: (Applicative m, MonadReader s m) => (m (a -> b)) -> Getting a s a -> m b
(<*^>) f l = f <*> view l

infixl 3 <<*^>
(<<*^>) :: (Applicative m, MonadReader s m) => (m (a -> m b)) -> Getting a s a -> m b
(<<*^>) f l = join (f <*> view l)

infixl 4 <$^>
(<$^>) :: (Functor m, MonadReader s m) => (a -> b) -> Getting a s a -> m b
(<$^>) f l = f <$> view l
