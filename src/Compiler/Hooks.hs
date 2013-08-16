module Compiler.Hooks where

import           Config               (cDYNAMIC_GHC_PROGRAMS, cProjectVersion)
import           Distribution.Package (PackageName (..))
import           DriverPipeline
import           DynFlags
import           GHC
import           Hooks
import           HscTypes             (mkHsSOName, mkSOName)
import           Linker               (LibrarySpec (..), LocateLibHook (..))
import           Module
import           Packages
import           Platform
import           SysTools             (LinkDynLibHook (..))
import qualified SysTools
import           UniqFM               (eltsUFM)

import           Control.Monad
import           Data.List            (isInfixOf, isPrefixOf, sort)
import           System.Directory     (doesFileExist)
import           System.FilePath

import           Compiler.Info
import           Compiler.Variants
import qualified Gen2.Foreign         as Gen2


installGhcjsHooks :: Bool   -- ^ Debug
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
                -> DynFlags
                -> [FilePath]
                -> [PackageId]
                -> IO ()
ghcjsLinkBinary debug jsFiles dflags objs dep_pkgs =
  void $ variantLink gen2Variant dflags debug exe [] deps objs jsFiles isRoot
    where
      isRoot _ = True
      deps     = map (\pkg -> (pkg, packageLibPaths pkg)) dep_pkgs'
      exe      = exeFileName dflags
      pidMap   = pkgIdMap (pkgState dflags)
      packageLibPaths :: PackageId -> [FilePath]
      packageLibPaths pkg = maybe [] libraryDirs (lookupPackage pidMap pkg)
      -- make sure we link ghcjs-prim even when it's not a dependency
      dep_pkgs' | any isGhcjsPrimPackage dep_pkgs = dep_pkgs
                | otherwise                       = ghcjsPrimPackage dflags : dep_pkgs
      isGhcjsPrimPackage pkgId = "ghcjs-prim-" `isPrefixOf` packageIdString pkgId

ghcjsPrimPackage :: DynFlags -> PackageId
ghcjsPrimPackage dflags =
  case prims of
    (x:_) -> mkPackageId x
    _     -> error "Package `ghcjs-prim' is required to link executables"
  where
    prims = reverse . sort $ filter ((PackageName "ghcjs-prim"==) . pkgName) pkgIds
    pkgIds = map sourcePackageId . eltsUFM . pkgIdMap . pkgState $ dflags

exeFileName :: DynFlags -> FilePath
exeFileName dflags
  | Just s <- outputFile dflags =
      -- unmunge the extension
      let s' = dropPrefix "js_" (drop 1 $ takeExtension s)
      in if null s'
           then dropExtension s <.> "jsexe"
           else dropExtension s <.> s'
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
           then "main.jsexe"
           else "a.jsexe"
  where
    dropPrefix prefix xs
      | prefix `isPrefixOf` xs = drop (length prefix) xs
      | otherwise              = xs


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

findFile :: (FilePath -> FilePath)      -- Maps a directory path to a file path
         -> [FilePath]                  -- Directories to look in
         -> IO (Maybe FilePath)         -- The first file path to match
findFile _            [] = return Nothing
findFile mk_file_path (dir : dirs)
  = do let file_path = mk_file_path dir
       b <- doesFileExist file_path
       if b then return (Just file_path)
            else findFile mk_file_path dirs

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

ghcjsDynLibVersionTag :: String
ghcjsDynLibVersionTag = "-ghcjs" ++ getCompilerVersion
                        ++ "_ghc" ++ cProjectVersion

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


