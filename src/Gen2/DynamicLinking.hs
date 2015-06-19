{-# LANGUAGE CPP, NondecreasingIndentation, TupleSections #-}
{-
  Various utilities for building and loading dynamic libraries, to make Template Haskell
  work in GHCJS
 -}

module Gen2.DynamicLinking ( ghcjsLink
                           , ghcjsCompileCoreExpr
                           , ghcjsGetValueSafely
                           , ghcjsDoLink
                           , isGhcjsPrimPackage
                           , ghcjsPrimPackage
                           ) where

import Id
import Name
import Outputable hiding ((<>))
import FastString
import HscTypes
import ByteCodeGen
import VarEnv
import Panic
import Util
import Exception
import Packages
import DynFlags
import Type
import CoreMonad hiding ( debugTraceMsg )
import DynamicLoading
import Module
import SrcLoc
import CoreSyn
import BasicTypes
import SimplCore
import CoreTidy
import SysTools hiding ( linkDynLib )
import Platform
import ErrUtils
import DriverPhases
import DriverPipeline hiding ( linkingNeeded )
import UniqFM
import Maybes hiding ( Succeeded )

import           Control.Applicative
import           Control.Monad

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as BS
import           Data.List ( isPrefixOf, sort, nub )
import           Data.Monoid
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Text as T

import           System.Directory
import           System.FilePath

import           Compiler.Compat
import           Compiler.Settings
import           Compiler.Variants
import qualified Compiler.Utils as Utils
import qualified Compiler.Info as Info

#if __GLASGOW_HASKELL__ >= 709
import           SysTools
import           Linker
#else
import           Gen2.GHC.Linker -- use our own version!
import           Gen2.GHC.SysTools ( ghcjsPackageHsLibs, linkDynLib )
#endif

import qualified Data.Yaml                as Yaml
import Compiler.Info (getLibDir)
import Gen2.Archive


-------------------------------------------------------------------------------------------
-- Link libraries

ghcjsLink :: GhcjsEnv
          -> GhcjsSettings
          -> [FilePath] -- ^ extra JS files
          -> Bool       -- ^ build JavaScript?
          -> GhcLink    -- ^ what to link
          -> DynFlags
          -> Bool
          -> HomePackageTable
          -> IO SuccessFlag
ghcjsLink env settings extraJs buildJs ghcLink dflags batch_attempt_linking pt
  | ghcLink == LinkInMemory || ghcLink == NoLink =
      return Succeeded
  | ghcLink == LinkStaticLib || ghcLink == LinkDynLib =
      if buildJs && isJust (gsLinkJsLib settings)
         then ghcjsLinkJsLib settings extraJs dflags pt
         else return Succeeded
  | otherwise = do
      when (buildJs && isJust (gsLinkJsLib settings))
        (void $ ghcjsLinkJsLib settings extraJs dflags pt) -- fixme use return value
      link' env settings extraJs buildJs dflags batch_attempt_linking pt

ghcjsLinkJsLib :: GhcjsSettings
               -> [FilePath] -- ^ extra JS files
               -> DynFlags
               -> HomePackageTable
               -> IO SuccessFlag
ghcjsLinkJsLib settings jsFiles dflags hpt
  | Just jsLib <- gsLinkJsLib settings = do
      let profSuff | WayProf `elem` ways dflags = "_p"
                   | otherwise                  = ""
          libFileName    = ("lib" ++ jsLib ++ profSuff) <.> "js_a"
          inOutputDir file =
            maybe file
                  (</>file)
                  (gsJsLibOutputDir settings `mplus` objectDir dflags)
          outputFile     = inOutputDir libFileName
          jsFiles' = nub (gsJsLibSrcs settings ++ jsFiles)
          meta    = Meta (opt_P dflags)
      jsEntries <- forM jsFiles' $ \file ->
        (JsSource file,) . B.fromStrict <$> BS.readFile file
      objEntries <- forM (eltsUFM hpt) $ \hmi -> do
        let mt    = T.pack . moduleNameString . moduleName . mi_module . hm_iface $ hmi
            files = maybe [] (\l -> [ o | DotO o <- linkableUnlinked l]) (hm_linkable hmi)
        -- fixme archive does not handle multiple files for a module yet
        forM files $ \file ->
          (Object mt,) . B.fromStrict <$> BS.readFile file
      B.writeFile outputFile (buildArchive meta (concat objEntries ++ jsEntries))
      -- we don't use shared js_so libraries ourselves, but Cabal expects that we
      -- generate one when building with --dynamic-too. Just write an empty file
      when (gopt Opt_BuildDynamicToo dflags || WayDyn `elem` ways dflags) $ do
        let sharedLibFileName =
              "lib" ++ jsLib ++ "-ghcjs" ++ Info.getCompilerVersion ++ profSuff <.> "js_so"
            sharedOutputFile = inOutputDir sharedLibFileName
        -- keep strip happy
        BS.writeFile sharedOutputFile =<< BS.readFile (topDir dflags </> "empty.o")
      return Succeeded
  | otherwise =
      return Succeeded

dumpHpt :: DynFlags -> HomePackageTable -> String
dumpHpt dflags pt = "hpt:\n" ++ unlines
  (map (\hmi -> (moduleNameString . moduleName . mi_module . hm_iface $ hmi) ++
                " -> " ++ maybe "<no linkable>" (showPpr dflags) (hm_linkable hmi))
       (eltsUFM pt))
ghcjsLinkJsBinary :: GhcjsEnv
                  -> GhcjsSettings
                  -> [FilePath]
                  -> DynFlags
                  -> [FilePath]
                  -> [PackageKey]
                  -> IO ()
ghcjsLinkJsBinary env settings jsFiles dflags objs dep_pkgs =
  void $ variantLink gen2Variant dflags env settings exe [] dep_pkgs objs' jsFiles isRoot S.empty
    where
      objs'    = map ObjFile objs
      isRoot _ = True
      exe      = Utils.exeFileName dflags
      packageLibPaths :: PackageKey -> [FilePath]
#if __GLASGOW_HASKELL__ >= 709
      packageLibPaths = maybe [] libraryDirs . lookupPackage dflags
#else
      packageLibPaths pkg = maybe [] libraryDirs (lookupPackage pidMap pkg)
      pidMap   = pkgIdMap (pkgState dflags)
#endif

isGhcjsPrimPackage :: DynFlags -> PackageKey -> Bool
isGhcjsPrimPackage dflags pkgKey
  =  getPackageName dflags pkgKey == "ghcjs-prim" ||
     (pkgKey == thisPackage dflags && 
      any (=="-DBOOTING_PACKAGE=ghcjs-prim") (opt_P dflags))

ghcjsPrimPackage :: DynFlags -> IO PackageKey
ghcjsPrimPackage dflags = do
  keys <- BS.readFile filename
  case Yaml.decodeEither keys of
    Left err -> error $ "could not read wired-in package keys from " ++ filename
    Right m -> case M.lookup "ghcjs-prim" m of
      Nothing -> error "Package `ghcjs-prim' is required to link executables"
      Just k -> return (stringToPackageKey k)
  where
    filename = getLibDir dflags </> "wiredinkeys" <.> "yaml"

link' :: GhcjsEnv
      -> GhcjsSettings
      -> [FilePath]              -- extra js files
      -> Bool                    -- building JavaScript
      -> DynFlags                -- dynamic flags
      -> Bool                    -- attempt linking in batch mode?
      -> HomePackageTable        -- what to link
      -> IO SuccessFlag

link' env settings extraJs buildJs dflags batch_attempt_linking hpt
   | batch_attempt_linking
   = do
        let
            staticLink = case ghcLink dflags of
                          LinkStaticLib -> True
                          _ -> platformBinariesAreStaticLibs (targetPlatform dflags)

            home_mod_infos = eltsUFM hpt

            -- the packages we depend on
            pkg_deps  = concatMap (map fst . dep_pkgs . mi_deps . hm_iface) home_mod_infos

            -- the linkables to link
            linkables = map (expectJust "link".hm_linkable) home_mod_infos

        debugTraceMsg dflags 3 (text "link: linkables are ..." $$ vcat (map ppr linkables))

        -- check for the -no-link flag
        if isNoLink (ghcLink dflags)
          then do debugTraceMsg dflags 3 (text "link(batch): linking omitted (-c flag given).")
                  return Succeeded
          else do

        let getOfiles (LM _ _ us) = map nameOfObject (filter isObject us)
            obj_files = concatMap getOfiles linkables

            exe_file = exeFileName staticLink dflags

        linking_needed <- linkingNeeded dflags staticLink linkables pkg_deps

        if not (gopt Opt_ForceRecomp dflags) && not linking_needed
           then do debugTraceMsg dflags 2 (text exe_file <+> ptext (sLit "is up to date, linking not required."))
                   return Succeeded
           else do

        when (not buildJs) $
           compilationProgressMsg dflags ("Linking " ++ exe_file ++ " ...")

        -- Don't showPass in Batch mode; doLink will do that for us.
        let link = case ghcLink dflags of
                LinkBinary    -> if buildJs then ghcjsLinkJsBinary env settings extraJs else linkBinary
                LinkStaticLib -> linkStaticLibCheck
                LinkDynLib    -> linkDynLibCheck
                other         -> panicBadLink other

        -- make sure we link ghcjs-prim even when it's not a dependency
        pkg_deps' <- if any (isGhcjsPrimPackage dflags) pkg_deps
                        then return pkg_deps
                        else (:pkg_deps) <$> ghcjsPrimPackage dflags

        link dflags obj_files pkg_deps'

        debugTraceMsg dflags 3 (text "link: done")

        -- linkBinary only returns if it succeeds
        return Succeeded

   | otherwise
   = do debugTraceMsg dflags 3 (text "link(batch): upsweep (partially) failed OR" $$
                                text "   Main.main not exported; not linking.")
        return Succeeded


linkingNeeded :: DynFlags -> Bool -> [Linkable] -> [PackageKey] -> IO Bool
linkingNeeded dflags staticLink linkables pkg_deps = do
        -- if the modification time on the executable is later than the
        -- modification times on all of the objects and libraries, then omit
        -- linking (unless the -fforce-recomp flag was given).
  let exe_file = exeFileName staticLink dflags
  e_exe_time <- tryIO $ getModificationUTCTime exe_file
  case e_exe_time of
    Left _  -> return True
    Right t -> do
        -- first check object files and extra_ld_inputs
        let extra_ld_inputs = [ f | FileOption _ f <- ldInputs dflags ]
        e_extra_times <- mapM (tryIO . getModificationUTCTime) extra_ld_inputs
        let (errs,extra_times) = splitEithers e_extra_times
        let obj_times =  map linkableTime linkables ++ extra_times
        if not (null errs) || any (t <) obj_times
            then return True
            else do

        -- next, check libraries. XXX this only checks Haskell libraries,
        -- not extra_libraries or -l things from the command line.
#if __GLASGOW_HASKELL__ >= 709
        let pkg_hslibs  = [ (libraryDirs c, lib)
                          | Just c <- map (lookupPackage dflags) pkg_deps,
                            lib <- packageHsLibs dflags c ]

        pkg_libfiles <- mapM (uncurry (findHSLib dflags)) pkg_hslibs
        if any isNothing pkg_libfiles then return True else do
        e_lib_times <- mapM (tryIO . getModificationUTCTime)
                          (catMaybes pkg_libfiles)
        let (lib_errs,lib_times) = splitEithers e_lib_times
        if not (null lib_errs) || any (t <) lib_times
           then return True
           else checkLinkInfo dflags pkg_deps exe_file
#else
        let pkg_map = pkgIdMap (pkgState dflags)
            pkg_hslibs  = [ (libraryDirs c, lib)
                          | Just c <- map (lookupPackage pkg_map) pkg_deps,
                            lib <- ghcjsPackageHsLibs dflags c ]
        pkg_libfiles <- mapM (uncurry (findHSLib dflags)) pkg_hslibs
        if any isNothing pkg_libfiles then return True else do
        e_lib_times <- mapM (tryIO . getModificationUTCTime)
                          (catMaybes pkg_libfiles)
        let (lib_errs,lib_times) = splitEithers e_lib_times
        if not (null lib_errs) || any (t <) lib_times
           then return True
           else checkLinkInfo dflags pkg_deps exe_file
#endif

panicBadLink :: GhcLink -> a
panicBadLink other = panic ("link: GHC not built to link this way: " ++
                            show other)

linkDynLibCheck :: DynFlags -> [String] -> [PackageKey] -> IO ()
linkDynLibCheck dflags o_files dep_packages
 = do
    when (haveRtsOptsFlags dflags) $ do
      log_action dflags dflags SevInfo noSrcSpan defaultUserStyle
          (text "Warning: -rtsopts and -with-rtsopts have no effect with -shared." $$
           text "    Call hs_init_ghc() from your main() function to set these options.")

    linkDynLib dflags o_files dep_packages

linkStaticLibCheck :: DynFlags -> [String] -> [PackageKey] -> IO ()
linkStaticLibCheck dflags o_files dep_packages
 = do
    when (platformOS (targetPlatform dflags) `notElem` [OSiOS, OSDarwin]) $
      throwGhcExceptionIO (ProgramError "Static archive creation only supported on Darwin/OS X/iOS")
    linkBinary' True dflags o_files dep_packages

findHSLib :: DynFlags -> [String] -> String -> IO (Maybe FilePath)
findHSLib dflags dirs lib = do
  let batch_lib_file = if gopt Opt_Static dflags
                       then "lib" ++ lib <.> "a"
                       else mkSOName (targetPlatform dflags) lib
  found <- filterM doesFileExist (map (</> batch_lib_file) dirs)
  case found of
    [] -> return Nothing
    (x:_) -> return (Just x)

touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path

haveRtsOptsFlags :: DynFlags -> Bool
haveRtsOptsFlags dflags =
         isJust (rtsOpts dflags) || case rtsOptsEnabled dflags of
                                        RtsOptsSafeOnly -> False
                                        _ -> True

linkBinary' :: Bool -> DynFlags -> [FilePath] -> [PackageKey] -> IO ()
linkBinary' staticLink dflags o_files dep_packages = do
    let platform = targetPlatform dflags
        mySettings = settings dflags
        verbFlags = getVerbFlags dflags
        output_fn = exeFileName staticLink dflags

    -- get the full list of packages to link with, by combining the
    -- explicit packages with the auto packages and all of their
    -- dependencies, and eliminating duplicates.

    full_output_fn <- if isAbsolute output_fn
                      then return output_fn
                      else do d <- getCurrentDirectory
                              return $ normalise (d </> output_fn)
    pkg_lib_paths <- getPackageLibraryPath dflags dep_packages
    let pkg_lib_path_opts = concatMap get_pkg_lib_path_opts pkg_lib_paths
        get_pkg_lib_path_opts l
         | osElfTarget (platformOS platform) &&
           dynLibLoader dflags == SystemDependent &&
           not (gopt Opt_Static dflags)
            = let libpath = if gopt Opt_RelativeDynlibPaths dflags
                            then "$ORIGIN" </>
                                 (l `makeRelativeTo` full_output_fn)
                            else l
                  rpath = if gopt Opt_RPath dflags
                          then ["-Wl,-rpath",      "-Wl," ++ libpath]
                          else []
                  -- Solaris 11's linker does not support -rpath-link option. It silently
                  -- ignores it and then complains about next option which is -l<some
                  -- dir> as being a directory and not expected object file, E.g
                  -- ld: elf error: file
                  -- /tmp/ghc-src/libraries/base/dist-install/build:
                  -- elf_begin: I/O error: region read: Is a directory
                  rpathlink = if (platformOS platform) == OSSolaris2
                              then []
                              else ["-Wl,-rpath-link", "-Wl," ++ l]
              in ["-L" ++ l] ++ rpathlink ++ rpath
         | otherwise = ["-L" ++ l]

    let lib_paths = libraryPaths dflags
    let lib_path_opts = map ("-L"++) lib_paths

    extraLinkObj <- mkExtraObjToLinkIntoBinary dflags
    noteLinkObjs <- mkNoteObjsToLinkIntoBinary dflags dep_packages

    pkg_link_opts <- do
        (package_hs_libs, extra_libs, other_flags) <- getPackageLinkOpts dflags dep_packages
        return $ if staticLink
            then package_hs_libs -- If building an executable really means making a static
                                 -- library (e.g. iOS), then we only keep the -l options for
                                 -- HS packages, because libtool doesn't accept other options.
                                 -- In the case of iOS these need to be added by hand to the
                                 -- final link in Xcode.
            else package_hs_libs ++ extra_libs ++ other_flags

    pkg_framework_path_opts <-
        if platformUsesFrameworks platform
        then do pkg_framework_paths <- getPackageFrameworkPath dflags dep_packages
                return $ map ("-F" ++) pkg_framework_paths
        else return []

    framework_path_opts <-
        if platformUsesFrameworks platform
        then do let framework_paths = frameworkPaths dflags
                return $ map ("-F" ++) framework_paths
        else return []

    pkg_framework_opts <-
        if platformUsesFrameworks platform
        then do pkg_frameworks <- getPackageFrameworks dflags dep_packages
                return $ concat [ ["-framework", fw] | fw <- pkg_frameworks ]
        else return []

    framework_opts <-
        if platformUsesFrameworks platform
        then do let frameworks = cmdlineFrameworks dflags
                -- reverse because they're added in reverse order from
                -- the cmd line:
                return $ concat [ ["-framework", fw]
                                | fw <- reverse frameworks ]
        else return []

        -- probably _stub.o files
    let extra_ld_inputs = ldInputs dflags

    -- Here are some libs that need to be linked at the *end* of
    -- the command line, because they contain symbols that are referred to
    -- by the RTS.  We can't therefore use the ordinary way opts for these.
    let
        debug_opts | WayDebug `elem` ways dflags = [
{-
#if defined(HAVE_LIBBFD)
                          "-lbfd", "-liberty"
#endif
-}
                         ]
                   | otherwise            = []

    let thread_opts
         | WayThreaded `elem` ways dflags =
            let os = platformOS (targetPlatform dflags)
            in if os == OSOsf3 then ["-lpthread", "-lexc"]
               else if os `elem` [OSMinGW32, OSFreeBSD, OSOpenBSD,
                                  OSNetBSD, OSHaiku, OSQNXNTO, OSiOS]
               then []
               else ["-lpthread"]
         | otherwise               = []

    rc_objs <- maybeCreateManifest dflags output_fn

    let link = if staticLink
                   then SysTools.runLibtool
                   else SysTools.runLink
    link dflags (
                       map SysTools.Option verbFlags
                      ++ [ SysTools.Option "-o"
                         , SysTools.FileOption "" output_fn
                         ]
                      ++ map SysTools.Option (
                         []

                      -- Permit the linker to auto link _symbol to _imp_symbol.
                      -- This lets us link against DLLs without needing an "import library".
                      ++ (if platformOS platform == OSMinGW32
                          then ["-Wl,--enable-auto-import"]
                          else [])

                      -- '-no_compact_unwind'
                      -- C++/Objective-C exceptions cannot use optimised
                      -- stack unwinding code. The optimised form is the
                      -- default in Xcode 4 on at least x86_64, and
                      -- without this flag we're also seeing warnings
                      -- like
                      --     ld: warning: could not create compact unwind for .LFB3: non-standard register 5 being saved in prolog
                      -- on x86.
                      ++ (if sLdSupportsCompactUnwind mySettings &&
                             not staticLink &&
                             (platformOS platform == OSDarwin || platformOS platform == OSiOS) &&
                             case platformArch platform of
                               ArchX86 -> True
                               ArchX86_64 -> True
                               ArchARM {} -> True
                               _ -> False
                          then ["-Wl,-no_compact_unwind"]
                          else [])

                      -- '-no_pie'
                      -- iOS uses 'dynamic-no-pic', so we must pass this to ld to suppress a warning; see #7722
                      ++ (if platformOS platform == OSiOS &&
                             not staticLink
                          then ["-Wl,-no_pie"]
                          else [])

                      -- '-Wl,-read_only_relocs,suppress'
                      -- ld gives loads of warnings like:
                      --     ld: warning: text reloc in _base_GHCziArr_unsafeArray_info to _base_GHCziArr_unsafeArray_closure
                      -- when linking any program. We're not sure
                      -- whether this is something we ought to fix, but
                      -- for now this flags silences them.
                      ++ (if platformOS   platform == OSDarwin &&
                             platformArch platform == ArchX86 &&
                             not staticLink
                          then ["-Wl,-read_only_relocs,suppress"]
                          else [])

                      ++ o_files
                      ++ lib_path_opts)
                      ++ extra_ld_inputs
                      ++ map SysTools.Option (
                         rc_objs
                      ++ framework_path_opts
                      ++ framework_opts
                      ++ pkg_lib_path_opts
                      ++ extraLinkObj:noteLinkObjs
                      ++ pkg_link_opts
                      ++ pkg_framework_path_opts
                      ++ pkg_framework_opts
                      ++ debug_opts
                      ++ thread_opts
                    ))

    -- parallel only: move binary to another dir -- HWL
    success <- runPhase_MoveBinary dflags output_fn
    unless success $
        throwGhcExceptionIO (InstallationError ("cannot move binary"))

ghcjsDoLink :: GhcjsEnv -> GhcjsSettings -> Bool -> DynFlags -> Phase -> [FilePath] -> IO ()
ghcjsDoLink env settings native dflags stop_phase o_files
  | not (isStopLn stop_phase)
  = return ()           -- We stopped before the linking phase
  | native
  = case ghcLink dflags of
        NoLink     -> return ()
        LinkBinary -> linkBinary      dflags o_files []
        LinkDynLib -> linkDynLibCheck dflags o_files []
        other      -> panicBadLink other
  | isJust (gsLinkJsLib settings)
  = void $ ghcjsLinkJsLib settings o_files dflags emptyHomePackageTable
  | otherwise =
    void $ ghcjsLink env settings o_files True (ghcLink dflags) dflags True emptyHomePackageTable

-------------------------------------------------------------------------------------------
-- Compile Core expression
--
-- installed as a hook in HscMain,
-- make sure we use our modified linker to load the correct shared libraries

ghcjsCompileCoreExpr :: HscEnv -> SrcSpan -> CoreExpr -> IO HValue
ghcjsCompileCoreExpr hsc_env srcspan ds_expr = error "ghcjsCompileCoreExpr"
{-
  | rtsIsProfiled
    = throwIO (InstallationError "You can't call hscCompileCoreExpr in a profiled compiler")
            -- Otherwise you get a seg-fault when you run it
    | otherwise
    = do { let dflags = hsc_dflags hsc_env
           {- Simplify it -}
         ; simpl_expr <- simplifyExpr dflags ds_expr
           {- Tidy it (temporary, until coreSat does cloning) -}
         ; let tidy_expr = tidyExpr emptyTidyEnv simpl_expr
           {- Prepare for codegen -}
         ; prepd_expr <- corePrepExpr dflags hsc_env tidy_expr
           {- Lint if necessary -}
         ; lintInteractiveExpr "hscCompileExpr" hsc_env prepd_expr 
           {- Convert to BCOs -}
         ; bcos <- coreExprToBCOs dflags (icInteractiveModule (hsc_IC hsc_env)) prepd_expr
           {- link it -}
         ; hval <- linkExpr hsc_env srcspan bcos
         ; return hval }
-}

-------------------------------------------------------------------------------------------
-- used for loading values when running TH
-- installed as a hook in DynamicLoading
-- use our own linker here instead

ghcjsGetValueSafely :: HscEnv -> Name -> Type -> IO (Maybe HValue)
ghcjsGetValueSafely hsc_env val_name expected_type = do
    forceLoadNameModuleInterface hsc_env (ptext (sLit "contains a name used in an invocation of getHValueSafely")) val_name
    -- Now look up the names for the value and type constructor in the type environment
    mb_val_thing <- lookupTypeHscEnv hsc_env val_name
    case mb_val_thing of
        Nothing -> throwCmdLineErrorS dflags $ missingTyThingError val_name
        Just (AnId id) -> do
            -- Check the value type in the interface against the type recovered from the type constructor
            -- before finally casting the value to the type we assume corresponds to that constructor
            if expected_type `eqType` idType id
             then do
                -- Link in the module that contains the value, if it has such a module
                case nameModule_maybe val_name of
                    Just mod -> do linkModule hsc_env mod
                                   return ()
                    Nothing ->  return ()
                -- Find the value that we just linked in and cast it given that we have proved it's type
                hval <- getHValue hsc_env val_name
                return (Just hval)
             else return Nothing
        Just val_thing -> throwCmdLineErrorS dflags $ wrongTyThingError val_name val_thing
   where dflags = hsc_dflags hsc_env

missingTyThingError :: Name -> SDoc
missingTyThingError name = hsep [ptext (sLit "The name"), ppr name, ptext (sLit "is not in the type environment: are you sure it exists?")]

wrongTyThingError :: Name -> TyThing -> SDoc
wrongTyThingError name got_thing = hsep [ptext (sLit "The name"), ppr name, ptext (sLit "is not that of a value but rather a"), pprTyThingCategory got_thing]

throwCmdLineErrorS :: DynFlags -> SDoc -> IO a
throwCmdLineErrorS dflags = throwCmdLineError . showSDoc dflags

throwCmdLineError :: String -> IO a
throwCmdLineError = throwGhcExceptionIO . CmdLineError

