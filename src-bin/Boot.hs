{-# LANGUAGE ExtendedDefaultRules, OverloadedStrings #-}
module Main where

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
import System.Exit (exitSuccess)

default (Text)

main = do
    settings <- execParser optParser'
    r <- (shellyNoDir $ actions settings `catchany_sh` (return . Just))
    maybe exitSuccess Ex.throwIO r
  where
    actions :: BootSettings -> Sh (Maybe Ex.SomeException)
    actions s = verbosely . tracing False $ do
      e <- test_f ("data" </> "primops.txt.pp")
      if initTree s
        then initSourceTree e
        else when (not e) $ do
          cdBase
          e' <- test_d ("ghcjs-boot")
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
      installRts
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
                                 , preBuilt  :: Bool      -- ^ download prebuilt archive if available
                                 , quick     :: Bool      -- ^ skip Cabal and ghcjs-base
                                 , jobs      :: Int       -- ^ number of parallel jobs
                                 , debug     :: Bool      -- ^ build debug version of the libraries
                                 , verbose   :: Bool
                                 } deriving (Ord, Eq)

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

initPackageDB :: Sh ()
initPackageDB = do
  echo "creating package databases"
  base <- liftIO getGlobalPackageBase
  inst <- liftIO getGlobalPackageInst
  rm_rf . fromString =<< liftIO getGlobalPackageDB
  rm_rf . fromString =<< liftIO getUserPackageDB
  mkdir_p (fromString base)
  mkdir_p (fromString inst)
  ghcjs_pkg ["init", T.pack base <> "/package.conf.d"] `catchany_sh` const (return ())
  ghcjs_pkg ["init", T.pack inst <> "/package.conf.d"] `catchany_sh` const (return ())

-- | these need to be installed in the boot setting:
--   fake Cabal package registered, GHCJS_BOOTING environment variable set
bootPackages :: [String]
bootPackages = [ "ghc-prim"
               , "integer-gmp"
               , "base"
               , "deepseq"
               , "containers"
               , "template-haskell"
               , "array"
               , "pretty"
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

initSourceTree :: Bool -> Sh ()
initSourceTree currentDir = do
  if currentDir
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
  forM_ bootPackages  (patchPackage "boot")
  preparePrimops
  buildGenPrim
  buildGmpConstants

-- | preprocess primops.txt.pp, one version for the JS platform
--   one for native
preparePrimops :: Sh ()
preparePrimops = sub $ do
  cd "data"
  mkdir_p "native"
  cp (Paths.libdir </> "include" </> "MachDeps.h") "native"
  cp (Paths.libdir </> "include" </> "ghcautoconf.h") "native"
  cp (Paths.libdir </> "include" </> "ghcplatform.h") ("native" </> "ghc_boot_platform.h")
  cpp ["-P", "-Ijs", "primops.txt.pp", "-o", "primops-js.txt"]
  cpp ["-P", "-Inative", "primops.txt.pp", "-o", "primops-native.txt"]


-- | build the genprimopcode tool, this requires alex and happy
buildGenPrim :: Sh ()
buildGenPrim = sub $ do
  cd ("utils" </> "genprimopcode")
  e <- test_f "genprimopcode"
  when (not e) $ do
    alex  ["Lexer.x"]
    happy ["Parser.y"]
    ghc   ["-o", "genprimopcode", "-O", "Main.hs"]

buildGmpConstants :: Sh ()
buildGmpConstants = sub $ do
  cd ("boot" </> "integer-gmp" </> "mkGmpDerivedConstants")
  ghc ["-no-hs-main", "-o", "mkGmpDerivedConstants", "mkGmpDerivedConstants.c"]
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

installRts :: Sh ()
installRts = do
  echo "installing RTS"
  dest <- liftIO getGlobalPackageDB
  base <- liftIO getGlobalPackageBase
  let lib    = base </> "lib"
      inc    = lib  </> "include"
      rtsLib = lib  </> "rts-1.0"
  rtsConf <- readfile (Paths.libdir </> "package.conf.d" </> "builtin_rts.conf")
  writefile (dest </> "builtin_rts.conf") $
                 fixRtsConf (toTextIgnore inc) (toTextIgnore rtsLib) rtsConf
  ghcjs_pkg ["recache", "--global"]
  mkdir_p lib
  mkdir_p inc
  sub $ cd (Paths.libdir </> "include") >> cp_r "." inc
  sub $ cd (Paths.libdir </> "rts-1.0") >> cp_r "." rtsLib
  cp (Paths.libdir </> "settings")          (lib </> "settings")
  cp (Paths.libdir </> "platformConstants") (lib </> "platformConstants")
  cp (Paths.libdir </> "settings")          (base </> "settings")
  cp (Paths.libdir </> "platformConstants") (base </> "platformConstants")
  cp (Paths.libdir </> "unlit")             (base </> "unlit")
  -- required for integer-gmp
  cp ("boot" </> "integer-gmp" </> "mkGmpDerivedConstants" </> "GmpDerivedConstants.h") inc

cabalFlags :: Bool -- ^ pass -j argument to GHC instead of cabal?
           -> BootSettings 
           -> [Text]
cabalFlags parGhc s = v ++ j ++ d
  where
    v = if verbose s then ["-v"] else []
    j = if jobs s /= 1 then [if parGhc then jGhc else jCabal] else []
    d = if debug s then ["--ghcjs-options=--debug"] else []
    jGhc   = T.pack $ "--ghcjs-options=-j" ++ show (jobs s)
    jCabal = T.pack $ "-j" ++ show (jobs s)

installBootPackages :: BootSettings -> Sh ()
installBootPackages s = sub $ do
  echo "installing boot packages"
  cd "boot"
  forM_ bootPackages preparePackage
  when (not $ null bootPackages)
    (cabalBoot $ ["install", "--ghcjs", "--solver=topdown"] ++ cabalFlags True s ++ map (T.pack.("./"++)) bootPackages)

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
        forM_ pkgs $ \pkg -> when ("./" `T.isPrefixOf` pkg) $ do
          let pkg' = T.unpack (T.drop 2 pkg)
          when (initTree s) $ sub (cd ".." >> patchPackage "extra" pkg')
          preparePackage pkg'
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
  when (confac && not conf) (run_ "autoreconf" [])
  rm_rf "dist"
  cabal ["clean"]

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
  installed <- T.words <$> run "ghc-pkg" ["list", "--simple-output"]
  dumped <- T.lines <$> run "ghc-pkg" ["dump"]
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
  ghcjs_pkg ["recache", "--global"]

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
  pkgs <- T.words <$> run "ghcjs-pkg" ["list", "--simple-output"]
  forM_ pkgs $ \p -> when (any (`T.isPrefixOf` p) fakes)
    (echo ("unregistering " <> p) >> ghcjs_pkg ["unregister", p])

checkShims :: Sh ()
checkShims = sub $ do
  cdBase
  e <- test_f ("shims" </> "base" <.> "yaml")
  if e then sub $ do
              echo "shims repository already exists, be sure to keep it updated"
       else sub $ do
              echo "The shims repository is missing, fetching"
              git ["clone", "https://github.com/ghcjs/shims.git"]

cpp          = run_ "cpp"
gcc          = run_ "gcc"
git          = run_ "git"
alex         = run_ "alex"
happy        = run_ "happy"
ghc          = run_ "ghc"
path         = run_ "patch"
ghcjs_pkg    = run_ "ghcjs-pkg"
cabalBoot xs = sub (setenv "GHCJS_BOOTING" "1" >> run_ "cabal" xs)
cabal        = run_ "cabal"
patch        = run_ "patch"

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
