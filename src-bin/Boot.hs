{-# LANGUAGE CPP, ExtendedDefaultRules, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import Compiler.GhcjsProgram (printVersion)
import Compiler.Info
import Prelude hiding (FilePath)
import Shelly
import qualified GHC.Paths as Paths
import Data.Text (Text)
import qualified Data.Text as T
import Data.Monoid
import Data.Maybe
import Data.Char
import Control.Applicative
import Control.Monad
import Options.Applicative
import qualified Control.Exception as Ex
import System.Exit (exitSuccess, exitFailure)

import qualified Data.ByteString.Lazy as BL
import Network.HTTP (simpleHTTP, getRequest)
import Network.HTTP.Base (Response(..), Request(..), RequestMethod(..), mkRequest)
import Network.Stream (Result, ConnError(..))
import Network.URI (parseURI)
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.BZip as BZip
import qualified Codec.Archive.Tar as Tar

default (Text)

main = do
    settings <- adjustDefaultSettings <$> execParser optParser'
    when (showVersion settings) (printVersion >> exitSuccess)
    r <- (shellyNoDir $ actions settings `catchany_sh` (return . Just))
    maybe exitSuccess Ex.throwIO r
  where
    actions :: BootSettings -> Sh (Maybe Ex.SomeException)
    actions s = verbosely . tracing False $ do
      checkBuildTools s
      e <- test_f ("data" </> "primops.txt.pp")
      if initTree s
        then initSourceTree e s
        else when (not e) $ do
          cdBase
          e' <- test_d "ghcjs-boot"
          if e' then cd "ghcjs-boot"
                else do
                  p <- pwd
                  errorExit ("cannot find `" <> toTextIgnore (p </> "ghcjs-boot") <> "'\n" <>
                             "perhaps you need to run `ghcjs-boot --init' or run from\n" <>
                             "a directory with an initialized `ghcjs-boot' repository")
      initPackageDB
      dir <- pwd
      echo ("booting from: " <> toTextIgnore dir)
      pjs <- test_f ("data" </> "primops-js.txt")
      when (not pjs) (errorExit $ "Cannot find `data/primops-js.txt': the source tree appears to have\n" <>
                                  "not been initialized. Run `ghcjs-boot --init' first." )
      installRts s
      installFakes
      installBootPackages s
      installGhcjsPrim s
      checkShims
      if quick s then do
                   installGhcjsPackages s
                   removeFakes
                 else do
                   installExtraPackages s -- this removes the fake Cabal
                   installGhcjsPackages s
      return Nothing

data BootSettings = BootSettings { initTree  :: Bool      -- ^ initialize the source tree
                                 , showVersion :: Bool    -- ^ show the version and exit
                                 , preBuilt  :: Bool      -- ^ download prebuilt archive if available
                                 , quick     :: Bool      -- ^ skip Cabal and ghcjs-base
                                 , jobs      :: Int       -- ^ number of parallel jobs
                                 , debug     :: Bool      -- ^ build debug version of the libraries
                                 , verbose   :: Bool
                                 , iconvInclude :: Maybe Text -- ^ directory containing iconv.h
                                 , iconvLib     :: Maybe Text -- ^ directory containing iconv library
                                 , gmpInclude   :: Maybe Text -- ^ directory containing gmp.h
                                 , gmpLib       :: Maybe Text -- ^ directory containing gmp library
                                 , gmpFramework :: Bool       -- ^ with-gmp-framework-preferred
                                 , gmpInTree    :: Bool       -- ^ force using the in-tree GMP
                                 } deriving (Ord, Eq)

adjustDefaultSettings :: BootSettings -> BootSettings
#ifdef WINDOWS
adjustDefaultSettings s
  | isNothing (gmpInclude s) && isNothing (gmpLib s) = s { gmpInTree = True }
  | otherwise                                        = s
#else
adjustDefaultSettings = id
#endif

#ifdef WINDOWS
-- small 'boot' archive with tools (wget, xz and tar)
-- for getting the big archive more quickly
buildToolsBootURI :: String
buildToolsBootURI = "http://hdiff.luite.com/ghcjs/ghcjs-buildtools-boot-windows-" ++ getCompilerVersion ++ ".tar.gz"

buildToolsURI :: String
buildToolsURI = "http://hdiff.luite.com/ghcjs/ghcjs-buildtools-windows-" ++ getCompilerVersion ++ ".tar.xz"

buildToolsBootFile :: FilePath
buildToolsBootFile = fromString ("ghcjs-buildtools-boot-windows-" ++ getCompilerVersion) <.> "tar" <.> "gz"

buildToolsFile :: FilePath
buildToolsFile = fromString ("ghcjs-buildtools-windows-" ++getCompilerVersion) <.> "tar" <.> "xz"

checkBuildTools :: BootSettings -> Sh ()
checkBuildTools s = do
  mw <- sub $ do
    cdBase
    base <- absPath =<< pwd
    mw <- canonicalize (base </> ".." </> "mingw")
    mingwInstalled <- test_d mw
    when (not mingwInstalled) $ do
      echo "copying MingW installation"
      cp_r (Paths.libdir </> ".." </> "mingw") (base </> "..")
    return mw
  bt <- sub $ do
    cdBase
    p <- absPath =<< pwd
    let bt = p </> "buildtools"
    toolsExist <- test_d "buildtools"
    when (not toolsExist) $ do
      if not (initTree s)
        then do
          echo "buildtools required for ghcjs-boot not installed"
          echo "`ghcjs-boot --init` will automatically install them"
          liftIO exitFailure
        else do
          toolsArchiveExists <- test_f buildToolsFile
          toolsBootDirExists <- test_d "buildtools-boot"
          when (not toolsArchiveExists || not toolsBootDirExists) $ do
            unless toolsBootDirExists $ do
              toolsBootArchiveExists <- test_f buildToolsBootFile
              unless toolsBootArchiveExists $ do
                echo "fetching extra buildtools"
                res <- liftIO $ simpleHTTP (getBsRequest buildToolsBootURI)
                case res of
                  Left err -> do
                    echo $ "could not download " <> T.pack buildToolsBootURI
                    echo $ "error: " <> T.pack (show err)
                    echo $ "you can manually get the file and place it in: " <> toTextIgnore p
                    liftIO exitFailure
                  Right response -> writeBinary buildToolsBootFile (rspBody response)
              unpackTarGz "." buildToolsBootFile
              prependPath [p </> "buildtools-boot" </> "bin"]
            wget [T.pack buildToolsURI]
          prependPath [p </> "buildtools-boot" </> "bin"]
          echo "extracting buildtools"
          tar ["-xJvf", toTextIgnore buildToolsFile]
    return bt
  prependPath [ mw </> "bin"
              , bt </> "bin"
              , bt </> "msys" </> "1.0" </> "bin"
              , bt </> "git" </> "bin"
              ]
  setenv "MINGW_HOME" (toTextIgnore mw)
  setenv "PERL5LIB" (msysPath $ bt </> "share" </> "autoconf")
  mkdir_p (bt </> "etc")
  writefile (bt </> "msys" </> "1.0" </> "etc" </> "fstab") $ T.unlines
    [ escapePath bt <> " /mingw"
    , escapePath (bt </> "msys" </> "1.0" </> "bin") <> " /bin"
    ]

prependPath :: [FilePath] -> Sh ()
prependPath xs = do
  path1 <- get_env "Path"
  path2 <- get_env "PATH"
  let path = maybe "" (";"<>) (path1 <> path2)
      newPath = T.intercalate ";" (map toTextIgnore xs) <> path
  setenv "Path" newPath
  setenv "PATH" newPath

-- convert C:\x\y to /c/x/y
msysPath :: FilePath -> Text
msysPath p = let p' = toTextIgnore p
                 backToForward '\\' = '/'
                 backToForward x    = x
             in "/" <> T.map backToForward (T.filter (/=':') p')

escapePath :: FilePath -> Text
escapePath p = let p' = toTextIgnore p
                   escape ' ' = "\\ "
                   escape c   = T.singleton c
               in  T.concatMap escape p'
#else
checkBuildTools :: BootSettings -> Sh ()
checkBuildTools _ = return ()

msysPath :: FilePath -> Text
msysPath = toTextIgnore
#endif

optParser' :: ParserInfo BootSettings
optParser' = info (helper <*> optParser) ( fullDesc <>
                  header "GHCJS booter, build base libraries for the compiler" <>
                  progDesc description
                  )

description :: String
description = unlines
  [ "ghcjs-boot builds the base libraries for GHCJS."
  , "If `ghcjs-boot' is run in a directory containing the `ghcjs-boot' [1] repository,"
  , "the sources from the current directory are used. Otherwise, `ghcjs-boot'"
  , "makes a global copy in `~/.ghcjs'"
  , ""
  , "[1] https://github.com/ghcjs/ghcjs-boot"
  ]

optParser :: Parser BootSettings
optParser = BootSettings
            <$> switch ( long "init"      <> short 'i' <>
                  help "initialize the boot tree for first time build (or update an existing one)" )
            <*> switch ( long "version" <>
                  help "show the ghcjs-boot version")
            <*> switch ( long "preBuilt"  <> short 'p' <>
                  help "download prebuilt libraries for compiler (might not be available)" )
            <*> switch ( long "quick"     <> short 'q' <>
                  help "quick boot (no Cabal or ghcjs-base, but enough to compile basic tests)" )
            <*> option ( long "jobs"   <> short 'j' <> metavar "JOBS" <> value 1 <>
                  help "number of jobs to run in parallel" )
            <*> switch ( long "debug"   <> short 'd' <>
                  help "build debug libraries with extra checks" )
            <*> switch ( long "verbose"   <> short 'v' <>
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

initPackageDB :: Sh ()
initPackageDB = do
  echo "creating package databases"
  base <- liftIO getGlobalPackageBase
  inst <- liftIO getGlobalPackageInst
  rm_rf . fromString =<< liftIO getGlobalPackageDB
  rm_rf . fromString =<< liftIO getUserPackageDB
  mkdir_p (fromString base)
  mkdir_p (fromString inst)
  ghcjs_pkg' ["init", T.pack base <> "/package.conf.d"] `catchany_sh` const (return ())
  ghcjs_pkg' ["init", T.pack inst <> "/package.conf.d"] `catchany_sh` const (return ())

-- | these need to be installed in the boot setting:
--   fake Cabal package registered, GHCJS_BOOTING environment variable set
bootPackages :: [String]
bootPackages = [ "ghc-prim"
               , "integer-gmp"
               , "base"
               , "deepseq"
               , "containers"
               , "directory"
               , "template-haskell"
               , "array"
               , "pretty"
               , "time"
               , "process"
#ifdef WINDOWS
               , "Win32"
#else
               , "unix"
#endif
               ]

-- | We need to pretend that these are installed while building bootPackages
--   GHCJS itself will call GHC when any of these are used
--   these are removed again after booting
fakePackages :: [String]
fakePackages = [ "Cabal"
               ]

-- | the GHCJS base libraries to install, except ghcjs-prim which is
--   installed earlier
ghcjsPackages :: [String]
ghcjsPackages = [ "ghcjs-base"
                ]

-- | reduced set of packages for quick boot
ghcjsQuickPackages = [ ]

initSourceTree :: Bool -> BootSettings -> Sh ()
initSourceTree localTree settings = do
  if localTree
    then do
      p <- pwd
      echo $ "preparing local boot tree ( " <> toTextIgnore p <> " )"
      echo "boot tree in current dir: not git pulling automatically"
      isGit <- test_d ".git"
      when isGit $ do
        git ["submodule", "update", "--init", "--recursive"]
        git ["submodule", "foreach", "git", "reset", "--hard"]
    else do
      cdBase
      p <- pwd
      echo $ "preparing global boot tree ( " <> toTextIgnore (p </> "ghcjs-boot") <> " )"
      e <- test_d "ghcjs-boot"
      if e then do
           cd "ghcjs-boot"
           isGit <- test_d ".git"
           if isGit
             then do
               echo "updating existing tree"
               git ["submodule", "foreach", "git", "reset", "--hard"]
               git ["pull", "--recurse-submodules"]
             else echo "ghcjs-boot is not a git repository, not updating"
         else do
           echo "cloning new tree"
           git ["clone", "http://github.com/ghcjs/ghcjs-boot"]
           cd "ghcjs-boot"
           git ["submodule", "update", "--init", "--recursive"]
  forM_ bootPackages (patchPackage "boot")
  extraPackages <- ls "extra"
  forM_ extraPackages $ \p -> do
                           isDir <- test_d p
                           rp <- relativeTo "extra" p
                           when isDir (patchPackage "extra" (toString rp))
  preparePrimops
  buildGenPrim
  cleanGmp

-- | preprocess primops.txt.pp, one version for the JS platform
--   one for native
preparePrimops :: Sh ()
preparePrimops = sub $ do
  cd "data"
  mkdir_p "native"
  cp (Paths.libdir </> "include" </> "MachDeps.h") "native"
  cp (Paths.libdir </> "include" </> "ghcautoconf.h") "native"
  cp (Paths.libdir </> "include" </> "ghcplatform.h") ("native" </> "ghc_boot_platform.h")
  primopsJs <- cpp ["-P", "-Ijs", "primops.txt.pp"]
  writefile "primops-js.txt" primopsJs
  primopsNative <- cpp ["-P", "-Inative", "primops.txt.pp"]
  writefile "primops-native.txt" primopsNative

-- | build the genprimopcode tool, this requires alex and happy
buildGenPrim :: Sh ()
buildGenPrim = sub $ do
  cd ("utils" </> "genprimopcode")
  e <- test_f "genprimopcode"
  when (not e) $ do
    alex  ["Lexer.x"]
    happy ["Parser.y"]
    ghc   ["-o", "genprimopcode", "-O", "Main.hs", "+RTS", "-K128M"]

cleanGmp :: Sh ()
cleanGmp = sub $ do
  cd ("boot" </> "integer-gmp")
  rm_rf ("gmp" </> "intree")
  rm_f ("mkGmpDerivedConstants" </> exe "mkGmpDerivedConstants")
  rm_f "GmpDerivedConstants.h"

prepareGmp :: BootSettings -> Sh ()
prepareGmp settings = sub $ do
  cd ("boot" </> "integer-gmp")
  intreeInstalled <- test_f ("gmp" </> "intree" </> "include" </> "gmp.h")
  when (gmpInTree settings && not intreeInstalled) $ do
      sub $ do
        cd "gmp"
        lsFilter "." isGmpSubDir rm_rf
        echo "unpacking in-tree GMP"
        lsFilter "tarball" (return . isTarball) (unpackTarBz2 ".")
        d <- pwd
        ad <- absPath d
        lsFilter "." isGmpSubDir $ \dir -> do
          -- patch has already been applied
          cd dir
          adir <- absPath dir
          echo ("building GMP in dir: " <> toTextIgnore adir)
          configure ["--prefix=" <> msysPath (ad </> "intree")]
          make []
          make ["install"]
  constantsGenerated <- test_f "GmpGeneratedConstants.h"
  when (not constantsGenerated) $ do
    if gmpInTree settings
	  then do
        ad <- absPath =<< pwd
        buildGmpConstants (gmpInclude settings `mplus`
	      Just (toTextIgnore (ad </> "gmp" </> "intree" </> "include")))
      else
        buildGmpConstants (gmpInclude settings)
    where
      lsFilter :: FilePath -> (FilePath -> Sh Bool) -> (FilePath -> Sh ()) -> Sh ()
      lsFilter dir p a = ls dir >>= mapM_ (\x -> p x >>= flip when (a x))
      isTarball file = ".tar.bz2" `T.isSuffixOf` toTextIgnore file
      isGmpSubDir dir = (("gmp-" `T.isPrefixOf`) . toTextIgnore) <$> relativeTo "." dir

buildGmpConstants :: Maybe Text -> Sh ()
buildGmpConstants includeDir = sub $ do
  echo "generating GMP derived constants"
  cd "mkGmpDerivedConstants"
  ghc $ maybe [] (\d -> ["-I" <> d]) includeDir ++
    ["-fforce-recomp", "-no-hs-main", "-o", "mkGmpDerivedConstants", "mkGmpDerivedConstants.c"]
  p <- pwd
  constants <- run (p </> "mkGmpDerivedConstants") []
  writefile "GmpDerivedConstants.h" constants

patchPackage :: FilePath -> String -> Sh ()
patchPackage base pkg = sub $ do
  let p = "patches" </> fromString pkg <.> "patch"
  e <- test_f p
  when e $ do
    echo $ "applying patch: " <> toTextIgnore p
    readfile p >>= setStdin
    cd (base </> fromString pkg)
    ignoreExcep $ patch ["-p1", "-N"]

installRts :: BootSettings -> Sh ()
installRts settings = do
  echo "installing RTS"
  dest <- liftIO getGlobalPackageDB
  base <- liftIO getGlobalPackageBase
  let lib    = base </> "lib"
      inc    = lib  </> "include"
      rtsLib = lib  </> "rts-1.0"
  rtsConf <- readfile (Paths.libdir </> "package.conf.d" </> "builtin_rts.conf")
  writefile (dest </> "builtin_rts.conf") $
                 fixRtsConf (toTextIgnore inc) (toTextIgnore rtsLib) rtsConf
  ghcjs_pkg' ["recache", "--global"]
  mkdir_p lib
  mkdir_p inc
  sub $ cd (Paths.libdir </> "include") >> cp_r "." inc
  sub $ cd (Paths.libdir </> "rts-1.0") >> cp_r "." rtsLib
  sub $ cd ("data" </> "include")       >> cp_r "." inc
  cp (Paths.libdir </> "settings")          (lib </> "settings")
  cp (Paths.libdir </> "platformConstants") (lib </> "platformConstants")
  cp (Paths.libdir </> "settings")          (base </> "settings")
  cp (Paths.libdir </> "platformConstants") (base </> "platformConstants")
  cp (Paths.libdir </> exe "unlit")         (base </> exe "unlit")
  -- required for integer-gmp
  prepareGmp settings
  cp ("boot" </> "integer-gmp" </> "mkGmpDerivedConstants" </> "GmpDerivedConstants.h") inc
#ifdef WINDOWS
  cp (Paths.libdir </> exe "touchy") (base </> exe "touchy")
#endif
  echo "RTS prepared"

exe :: FilePath -> FilePath
#ifdef WINDOWS
exe p = p <.> "exe"
#else
exe p = p
#endif

cabalFlags :: Bool -- ^ pass -j argument to GHC instead of cabal?
           -> BootSettings
           -> [Text]
cabalFlags parGhc s = v ++ j ++ d ++ str
  where
#ifdef WINDOWS
    -- workaround for Cabal bug?
    str = ["--disable-executable-stripping", "--disable-library-stripping"]
#else
    str = []
#endif
    v = if verbose s then ["-v"] else []
    j = if jobs s /= 1 then [if parGhc then jGhc else jCabal] else []
    d = if debug s then ["--ghcjs-options=--debug"] else []
    jGhc   = T.pack $ "--ghcjs-options=-j" ++ show (jobs s)
    jCabal = T.pack $ "-j" ++ show (jobs s)

installBootPackages :: BootSettings -> Sh ()
installBootPackages s = sub $ do
  echo "installing boot packages"
  cd "boot"
  p <- pwd
  forM_ bootPackages preparePackage
  when (not $ null bootPackages) $ do
    (cabalBoot $ ["install", "--ghcjs", "--solver=topdown"] ++ configureOpts p ++ cabalFlags True s ++ map (T.pack.("./"++)) bootPackages)
    when (gmpInTree s) installInTreeGmp
    where
      configureOpts p = map ("--configure-option=" <>) $ catMaybes
            [ fmap ("--with-iconv-includes="  <>) (iconvInclude s)
            , fmap ("--with-iconv-libraries=" <>) (iconvLib s)
            , fmap ("--with-gmp-includes="   <>) (gmpInclude s <> inTreePath p "include")
            , fmap ("--with-gmp-libraries="  <>) (gmpLib s {- <> inTreePath p "lib" -})
            , if gmpFramework s then Just "--with-gmp-framework-preferred" else Nothing
            , if gmpInTree s then Just "--with-intree-gmp" else Nothing
            ]
      inTreePath p sub | gmpInTree s = Just (toTextIgnore (p' </> sub))
                       | otherwise   = Nothing
            where
              p' = p </> "integer-gmp" </> "gmp" </> "intree"

      -- urk, this is probably not how it's supposed to be done
      installInTreeGmp = do
        p <- absPath =<< pwd
        let gmpLib = p </> "integer-gmp" </> "gmp" </> "intree" </> "lib" </> "libgmp.a"
        libPath <- ghcjs_pkg ["field", "integer-gmp", "library-dirs", "--simple-output"]
        libPath' <- canonic (fromText $ T.strip libPath)
        cp gmpLib libPath'
        descr <- T.lines <$> ghcjs_pkg ["describe", "integer-gmp"]
        let updateLine line | "extra-libraries:" `T.isPrefixOf` line = line <> " gmp"
                            | otherwise                              = line
        setStdin (T.unlines $ map updateLine descr)
        ghcjs_pkg' ["update", "-", "--user"]

-- | extra packages are not hardcoded but live in two files:
--     extra/extra0  -- installed before Cabal (list Cabal dependencies here)
--     extra/extra1  -- installed after Cabal
-- remember that you can install cabal packages from a url, but only
-- local packages (starting with ./) are 'prepared'
installExtraPackages :: BootSettings -> Sh ()
installExtraPackages s = sub $ do
  echo "installing Cabal and extra packages"
  cd "extra"
  installExtra cabalBoot =<< readExtra "extra0"
  sub (cd ("cabal" </> "Cabal") >> cabal ["clean"])
  cabal $ ["install", "--ghcjs", "./cabal/Cabal", "--only-dependencies"]
  removeFakes
  cabal $ ["install", "--ghcjs", "./cabal/Cabal"] ++ cabalFlags False s
  installExtra cabal =<< readExtra "extra1"
    where
      ignored x = T.null x || "#" `T.isPrefixOf` x
      readExtra file =
        filter (not . ignored) . map T.strip . T.lines <$> readfile file
      installExtra c pkgs = do
        forM_ pkgs $ \pkg -> when ("./" `T.isPrefixOf` pkg) $
          preparePackage $ T.unpack (T.drop 2 pkg)
        when (not $ null pkgs)
          (c $ ["install", "--ghcjs"] ++ cabalFlags False s ++ pkgs)

installGhcjsPrim :: BootSettings -> Sh ()
installGhcjsPrim s = sub $ do
  echo "installing ghcjs-prim"
  let pkgs = ["ghcjs-prim"]
  cd "ghcjs"
  forM_ pkgs preparePackage
  when (not $ null pkgs)
    (cabalBoot $ ["install", "--ghcjs"] ++ cabalFlags False s ++ map (T.pack.("./"++)) pkgs)

installGhcjsPackages :: BootSettings -> Sh ()
installGhcjsPackages s = sub $ do
  echo "installing GHCJS-specific base libraries"
  let pkgs = if quick s then ghcjsQuickPackages else ghcjsPackages
      c    = if quick s then cabalBoot else cabal
  cd "ghcjs"
  forM_ pkgs preparePackage
  when (not $ null pkgs)
    (c $ ["install", "--ghcjs"] ++ cabalFlags False s ++ map (T.pack.("./"++)) pkgs)

preparePackage :: String -> Sh ()
preparePackage pkg = sub $ do
  cd (fromString pkg)
  conf   <- test_f "configure"
  confac <- test_f "configure.ac"
  when (confac && not conf) $ do
    echo ("generating configure script for " <> T.pack pkg)
    autoreconf []
  rm_rf "dist"
--  cabal ["clean"]

fixRtsConf :: Text -> Text -> Text -> Text
fixRtsConf incl lib conf = T.unlines . map fixLine . T.lines $ conf
    where
      fixLine l
          | "library-dirs:" `T.isPrefixOf` l = "library-dirs: " <> lib
          | "include-dirs:" `T.isPrefixOf` l = "include-dirs: " <> incl
          | otherwise                        = l

-- | register fake, empty packages to be able to build packages
--   that depend on Cabal
installFakes :: Sh ()
installFakes = silently $ do
  base <- T.pack <$> liftIO getGlobalPackageBase
  db   <- T.pack <$> liftIO getGlobalPackageDB
  installed <- T.words <$> ghc_pkg ["list", "--simple-output"]
  dumped <- T.lines <$> ghc_pkg ["dump"]
  let fakes = map T.pack fakePackages
  forM_ fakes $ \pkg ->
    case reverse (filter ((pkg<>"-") `T.isPrefixOf`) installed) of
      [] -> error (T.unpack $ "required package " <> pkg <> " not found in host GHC")
      (x:_) -> do
        let version = T.drop 1 (T.dropWhile (/='-') x)
        case findPkgId dumped pkg version of
          Nothing -> error (T.unpack $ "cannot find package id of " <> pkg <> "-" <> version)
          Just pkgId -> do
            let conf = fakeConf base base pkg version pkgId
            writefile (db </> (pkgId <.> "conf")) conf
  ghcjs_pkg' ["recache", "--global"]

findPkgId:: [Text] -> Text -> Text -> Maybe Text
findPkgId dump pkg version =
  listToMaybe (filter (pkgVer `T.isPrefixOf`) ids)
    where
      pkgVer = pkg <> "-" <> version <> "-"
      ids = map (T.dropWhile isSpace . T.drop 3) $ filter ("id:" `T.isPrefixOf`) dump

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

-- | remove the fakes after we're done with them
removeFakes :: Sh ()
removeFakes = do
  let fakes = map (T.pack . (++"-")) fakePackages
  pkgs <- T.words <$> ghcjs_pkg ["list", "--simple-output"]
  forM_ pkgs $ \p -> when (any (`T.isPrefixOf` p) fakes)
    (echo ("unregistering " <> p) >> ghcjs_pkg' ["unregister", p])

checkShims :: Sh ()
checkShims = sub $ do
  cdBase
  e <- test_f ("shims" </> "base" <.> "yaml")
  if e then sub $ do
              echo "shims repository already exists, be sure to keep it updated"
       else sub $ do
              echo "The shims repository is missing, fetching"
              git ["clone", "https://github.com/ghcjs/shims.git"]

getBsRequest :: String -> Request BL.ByteString
getBsRequest urlString =
  case parseURI urlString of
    Nothing -> error ("getBsRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest GET u

writeBinary :: FilePath -> BL.ByteString -> Sh ()
writeBinary file bs = do
  file' <- absPath file
  liftIO $ BL.writeFile (toString file') bs

unpackTarGz :: FilePath -> FilePath -> Sh ()
unpackTarGz dest tarFile =
  unpackCompressedTar dest tarFile GZip.decompress

unpackTarBz2 :: FilePath -> FilePath -> Sh ()
unpackTarBz2 dest tarFile =
  unpackCompressedTar dest tarFile BZip.decompress

unpackCompressedTar :: FilePath -> FilePath -> (BL.ByteString -> BL.ByteString) -> Sh ()
unpackCompressedTar dest tarFile decompress = do
  dest' <- absPath dest
  bs    <- BL.fromStrict <$> readBinary tarFile
  liftIO (Tar.unpack (toString dest') . Tar.read . decompress $ bs)

git          = run_ "git"
alex         = run_ "alex"
happy        = run_ "happy"
ghc          = run_ "ghc"
ghc_pkg      = run  "ghc-pkg"
ghcjs_pkg    = run  "ghcjs-pkg"
ghcjs_pkg'   = run_  "ghcjs-pkg"
cabalBoot xs = sub (setenv "GHCJS_BOOTING" "1" >> run_ "cabal" xs)
cabal        = run_ "cabal"
patch        = run_ "patch"
cpp       xs = run  "cpp" xs

#ifdef WINDOWS
bash cmd xs = run_ "bash" ["-c", T.unwords (map escapeArg (cmd:xs))]
  where
    -- might not escape everything, be careful
    escapeArg = T.concatMap escapeChar
    escapeChar ' '  = "\\ "
    escapeChar '\\' = "\\\\"
    escapeChar x    = T.singleton x

configure  = bash "./configure"
autoreconf = bash "autoreconf"
make       = bash "make"
wget       = run_ "wget"
tar        = run_ "tar"
#else
configure  = run_ "./configure"
autoreconf = run_ "autoreconf"
make       = run_ "make"
#endif

ignoreExcep a = a `catchany_sh` (\e -> echo $ "ignored exception: " <> T.pack (show e))

cdBase :: Sh ()
cdBase = do
  base <- fromString <$> liftIO getGlobalPackageBase
  mkdir_p base
  cd base

cdBoot :: Sh ()
cdBoot = cdBase >> cd "ghcjs-boot"

fromString :: String -> FilePath
fromString = fromText . T.pack

toString :: FilePath -> String
toString = T.unpack . toTextIgnore
