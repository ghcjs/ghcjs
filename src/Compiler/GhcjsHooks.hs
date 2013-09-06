{-# LANGUAGE OverloadedStrings #-}
module Compiler.GhcjsHooks where

import           Config               (cDYNAMIC_GHC_PROGRAMS, cProjectVersion)
import           CoreToStg (coreToStg)
import           CorePrep (corePrepPgm)
import           Distribution.Package (PackageName (..))
import           DriverPipeline
import           DriverPhases
import           DynFlags
import           GHC
import           GhcMonad
import           Hooks
import           HscTypes             (mkHsSOName, mkSOName, CgGuts(..), HscEnv(..))
import           HscMain              (HscStatus(..))
import           Linker               (LibrarySpec (..), LocateLibHook (..))
import           LoadIface
import           Module
import           Panic
import           Packages
import           Platform
import           SysTools             (LinkDynLibHook (..), touch)
import qualified SysTools
import           SimplStg             (stg2stg)
import           UniqFM               (eltsUFM)

import           Control.Monad
import qualified Data.ByteString      as B
import           Data.List            (isInfixOf, isPrefixOf, sort)
import           System.Directory     (doesFileExist, copyFile,
                                       createDirectoryIfMissing)
import           System.FilePath

import           Compiler.Info
import           Compiler.Variants
import           Compiler.Utils
import qualified Compiler.Utils       as Utils
import qualified Gen2.PrimIface       as Gen2
import qualified Gen2.Foreign         as Gen2


installGhcjsHooks :: Bool        -- ^ Debug
                  -> [FilePath]  -- JS objects
                  -> DynFlags -> DynFlags
installGhcjsHooks debug js_objs =
    Gen2.installForeignHooks True
    . insertHookDfs LinkDynLibHook   ghcjsLinkDynLib
    . insertHookDfs LinkBinaryHook   (ghcjsLinkBinary debug js_objs)
    . insertHookDfs LocateLibHook     ghcjsLocateLib
    . insertHookDfs PackageHsLibsHook ghcjsPackageHsLibs
  where
    insertHookDfs h v d = d { hooks = insertHook h v (hooks d) }


installNativeHooks :: DynFlags -> DynFlags
installNativeHooks df =
  Gen2.installForeignHooks False $ df { hooks = hooks' }
    where hooks' = insertHook PackageHsLibsHook ghcjsPackageHsLibs
                 $ insertHook LocateLibHook ghcjsLocateLib
                 $ hooks df


-- we don't have dynamic libraries:
ghcjsLinkDynLib :: DynFlags -> [FilePath] -> [PackageId] -> IO ()
ghcjsLinkDynLib dflags o_files dep_packages = return ()

ghcjsLinkBinary :: Bool     -- Debug mode
                -> [FilePath]
                -> Bool     -- link statically
                -> DynFlags
                -> [FilePath]
                -> [PackageId]
                -> IO ()
ghcjsLinkBinary debug jsFiles static dflags objs dep_pkgs =
  void $ variantLink gen2Variant dflags debug exe [] deps objs jsFiles isRoot
    where
      isRoot _ = True
      deps     = map (\pkg -> (pkg, packageLibPaths pkg)) dep_pkgs'
      exe      = Utils.exeFileName dflags
      pidMap   = pkgIdMap (pkgState dflags)
      packageLibPaths :: PackageId -> [FilePath]
      packageLibPaths pkg = maybe [] libraryDirs (lookupPackage pidMap pkg)
      -- make sure we link ghcjs-prim even when it's not a dependency
      dep_pkgs' | any isGhcjsPrimPackage dep_pkgs = dep_pkgs
                | otherwise                       = ghcjsPrimPackage dflags : dep_pkgs

isGhcjsPrimPackage :: PackageId -> Bool
isGhcjsPrimPackage pkgId = "ghcjs-prim-" `isPrefixOf` packageIdString pkgId

ghcjsPrimPackage :: DynFlags -> PackageId
ghcjsPrimPackage dflags =
  case prims of
    (x:_) -> x
    _     -> error "Package `ghcjs-prim' is required to link executables"
  where
    prims = reverse . sort $ filter isGhcjsPrimPackage pkgIds
    pkgIds = map packageConfigId . eltsUFM . pkgIdMap . pkgState $ dflags

ghcjsLocateLib :: DynFlags -> Bool -> [FilePath] -> String -> IO LibrarySpec
ghcjsLocateLib dflags is_hs dirs lib
  | not is_hs
    -- For non-Haskell libraries (e.g. gmp, iconv):
    --   first look in library-dirs for a dynamic library (libfoo.so)
    --   then  look in library-dirs for a static library (libfoo.a)
    --   then  try "gcc --print-file-name" to search gcc's search path
    --       for a dynamic library (#5289)
    --   otherwise, assume loadDLL can find it
    --
  = findDll `orElse` findArchive `orElse` tryGcc `orElse` assumeDll

  | not cDYNAMIC_GHC_PROGRAMS
    -- When the GHC package was not compiled as dynamic library
    -- (=DYNAMIC not set), we search for .o libraries or, if they
    -- don't exist, .a libraries.
  = findObject `orElse` findArchive `orElse` assumeDll

  | otherwise
    -- When the GHC package was compiled as dynamic library (=DYNAMIC set),
    -- we search for .so libraries first.
  = findHSDll `orElse` findDynObject `orElse` assumeDll
   where
     mk_obj_path      dir = dir </> (lib <.> "o")
     mk_dyn_obj_path  dir = dir </> (lib <.> "dyn_o")
     mk_arch_path     dir = dir </> ("lib" ++ lib <.> "a")

     hs_dyn_lib_name = lib ++ ghcjsDynLibVersionTag
     mk_hs_dyn_lib_path dir = dir </> mkHsSOName platform hs_dyn_lib_name

     so_name = mkSOName platform lib
     mk_dyn_lib_path dir = dir </> so_name

     findObject     = liftM (fmap Object)  $ findFile mk_obj_path        dirs
     findDynObject  = liftM (fmap Object)  $ findFile mk_dyn_obj_path    dirs
     findArchive    = liftM (fmap Archive) $ findFile mk_arch_path       dirs
     findHSDll      = liftM (fmap DLLPath) $ findFile mk_hs_dyn_lib_path dirs
     findDll        = liftM (fmap DLLPath) $ findFile mk_dyn_lib_path    dirs
     tryGcc         = liftM (fmap DLLPath) $ searchForLibUsingGcc dflags so_name dirs

     assumeDll   = return (DLL lib)
     infixr `orElse`
     f `orElse` g = do m <- f
                       case m of
                           Just x -> return x
                           Nothing -> g

     platform = targetPlatform dflags

searchForLibUsingGcc :: DynFlags -> String -> [FilePath] -> IO (Maybe FilePath)
searchForLibUsingGcc dflags so dirs = do
   str <- SysTools.askCc dflags (map (SysTools.FileOption "-L") dirs
                          ++ [SysTools.Option "--print-file-name", SysTools.Option so])
   let file = case lines str of
                []  -> ""
                l:_ -> l
   if file == so
      then return Nothing
      else return (Just file)


--------------------------------------------------
-- GHC API gets libs like libHSpackagename-ghcversion
-- change that to libHSpackagename-ghcjsversion_ghcversion
-- We need to do that in two places:
--    - Arguments for the system linker
--    - GHCi dynamic linker
ghcDynLibVersionTag :: String
ghcDynLibVersionTag   = "-ghc" ++ cProjectVersion

-- for now, Cabal installs libs with the GHC version
ghcjsDynLibVersionTag :: String
ghcjsDynLibVersionTag = "-ghcjs" ++ cProjectVersion

ghcjsPackageHsLibs :: DynFlags -> PackageConfig -> [String]
ghcjsPackageHsLibs dflags p = map fixLib (packageHsLibs' dflags p)
  where
    fixLib lib | "HS" `isPrefixOf` lib &&
                 ghcDynLibVersionTag `isInfixOf` lib =
      replace ghcDynLibVersionTag ghcjsDynLibVersionTag lib
               | otherwise = lib

replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace from to xs = go xs
  where
    go [] = []
    go xxs@(x:xs)
      | from `isPrefixOf` xxs = to ++ go (drop (length from) xxs)
      | otherwise = x : go xs


--------------------------------------------------
-- Driver hooks

installDriverHooks :: Bool -> DynFlags -> DynFlags
installDriverHooks debug df = df { hooks = hooks' }
  where hooks' = insertHook GhcPrimIfaceHook Gen2.ghcjsPrimIface
               $ insertHook RunPhaseHook (runGhcjsPhase debug)
               $ hooks df


runGhcjsPhase :: Bool
              -> PhasePlus -> FilePath -> DynFlags
              -> CompPipeline (PhasePlus, FilePath)
runGhcjsPhase debug (HscOut src_flavour mod_name result) _ dflags = do

        location <- getLocation src_flavour mod_name
        setModLocation location

        let o_file = ml_obj_file location -- The real object file
            hsc_lang = hscTarget dflags
            next_phase = hscPostBackendPhase dflags src_flavour hsc_lang

        case result of
            HscNotGeneratingCode ->
                return (RealPhase next_phase,
                        panic "No output filename from Hsc when no-code")
            HscUpToDate ->
                do liftIO $ touchObjectFile dflags o_file
                   -- The .o file must have a later modification date
                   -- than the source file (else we wouldn't get Nothing)
                   -- but we touch it anyway, to keep 'make' happy (we think).
                   return (RealPhase StopLn, o_file)
            HscUpdateBoot ->
                do -- In the case of hs-boot files, generate a dummy .o-boot
                   -- stamp file for the benefit of Make
                   liftIO $ touchObjectFile dflags o_file
                   return (RealPhase next_phase, o_file)
            HscRecomp cgguts mod_summary
              -> do output_fn <- phaseOutputFilename next_phase

                    PipeState{hsc_env=hsc_env'} <- getPipeState

                    outputFilename <- liftIO $ ghcjsWriteModule debug hsc_env' cgguts mod_summary output_fn

                    return (RealPhase next_phase, outputFilename)
-- skip these, but copy the result
runGhcjsPhase _ (RealPhase ph) input dflags
  | Just next <- lookup ph skipPhases = do
    output <- phaseOutputFilename next
    liftIO (copyFile input output)
    when (ph == As) (liftIO $ doFakeNative dflags (dropExtension output))
    return (RealPhase next, output)
  where
    skipPhases = [ (CmmCpp, Cmm), (Cmm, As), (As, StopLn) ]

-- otherwise use default
runGhcjsPhase _ p input dflags = runPhase p input dflags

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path


ghcjsWriteModule :: Bool        -- ^ Debug
                   -> HscEnv      -- ^ Environment in which to compile
                   -- the module
                   -> CgGuts      
                   -> ModSummary   
                   -> FilePath    -- ^ Output path
                   -> IO FilePath
ghcjsWriteModule debug env core mod output = do
    B.writeFile output =<< ghcjsCompileModule debug env core mod
    return output

ghcjsCompileModule :: Bool        -- ^ Debug
                   -> HscEnv      -- ^ Environment in which to compile
                   -- the module
                   -> CgGuts      
                   -> ModSummary   
                   -> IO B.ByteString
ghcjsCompileModule debug env core mod 
  | WayDyn `elem` ways dflags = do
      return "GHCJS dummy output"
  | otherwise = do
      core_binds <- corePrepPgm dflags env (cg_binds core) (cg_tycons core)
      stg <- coreToStg dflags (cg_module core) core_binds
      (stg', _ccs) <- stg2stg dflags (cg_module core) stg
      let obj = variantRender gen2Variant debug dflags stg' (cg_module core)
      return obj
    where
      dflags = hsc_dflags env


doFakeNative :: DynFlags -> FilePath -> IO ()
doFakeNative df base = do
  b <- getEnvOpt "GHCJS_FAKE_NATIVE"
  when b $ do
    mapM_ backupExt ["hi", "o", "dyn_hi", "dyn_o"]
    mapM_ touchExt  ["hi", "o", "dyn_hi", "dyn_o"]
  where
    backupExt ext = copyNoOverwrite (base ++ ".backup_" ++ ext) (base ++ "." ++ ext)
    touchExt  ext = touchFile df (base ++ "." ++ ext)

