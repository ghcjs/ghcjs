{-# LANGUAGE CPP #-}
module GHCJSMain (writeJavaScriptModule, linkJavaScript) where

import Id (Id)
import StgSyn (StgBinding)
import CostCentre (CollectedCCs)
import Module (ModuleName, PackageId)
import HscTypes (ModSummary(..), CgGuts (..))
import DynFlags (DynFlags(..))
import qualified Data.ByteString as B
#ifdef GHCJS_ENABLED
import Module (ml_hi_file, moduleNameString, moduleName)
import Distribution.Verbosity (normal)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import Module (mkModuleName)
import System.FilePath
       (takeBaseName, replaceExtension, dropExtension, (</>), (<.>))
import System.Directory (doesFileExist)
import Packages (getPreloadPackagesAnd, PackageConfig, importDirs)
import Data.List (nub)
import Util (notNull)
import ErrUtils (debugTraceMsg)
import Outputable ((<+>), ptext, text)
import FastString (sLit)
import Data.Maybe (catMaybes)

import qualified Generator.Link as Js (link)
import Compiler.Variants
       (variants, Variant(..))
import Control.Monad (forM_)
#endif

#ifndef GHCJS_ENABLED
writeJavaScriptModule :: ModSummary -> CgGuts
        -> ([(StgBinding,[(Id,[Id])])], CollectedCCs) -> IO ()
writeJavaScriptModule _ _ _ = return ()

linkJavaScript :: DynFlags -> [FilePath] -> [PackageId] -> [ModuleName] -> IO ()
linkJavaScript dyflags o_files dep_packages pagesMods = return ()
#else
writeJavaScriptModule :: ModSummary -> CgGuts
        -> ([(StgBinding,[(Id,[Id])])], CollectedCCs) -> IO ()
writeJavaScriptModule summary tidyCore (stg', _ccs) = do
    forM_ variants $ \variant -> do
        writeJavaScriptModule' variant summary tidyCore (stg', _ccs)

writeJavaScriptModule' :: Variant -> ModSummary -> CgGuts
        -> ([(StgBinding,[(Id,[Id])])], CollectedCCs) -> IO ()
writeJavaScriptModule' var summary _tidyCore (stg', _ccs) =
  do let (program, meta) = variantRender var stg' (ms_mod summary)
     putStrLn $ concat ["Writing module ", name, " (to ", outputFile vext, ")"]
     B.writeFile (outputFile vext) program
     case variantMetaExtension var of
       Nothing   -> return ()
       Just mext -> B.writeFile (outputFile mext) meta
   where
      vext = variantExtension var
      outputFile ext = replaceExtension (ml_hi_file . ms_location $ summary) ext
      name = moduleNameString . moduleName . ms_mod $ summary

linkJavaScript :: DynFlags -> [FilePath] -> [PackageId] -> [ModuleName] -> IO ()
linkJavaScript dyflags o_files dep_packages pagesMods = do
    forM_ variants $ \variant -> do
        linkJavaScript' variant dyflags o_files dep_packages pagesMods

linkJavaScript' :: Variant -> DynFlags -> [FilePath] -> [PackageId] -> [ModuleName] -> IO ()
linkJavaScript' var dyflags o_files dep_packages pagesMods = do
    let jsexe = jsexeFileName var dyflags
    importPaths <- getPackageImportPaths dyflags dep_packages
    debugTraceMsg dyflags 1 (ptext (sLit "JavaScript Linking") <+> text jsexe
                             <+> text "...")
    createDirectoryIfMissingVerbose normal False jsexe
    mbJsFiles <- mapM (mbFile . (flip replaceExtension ext)) o_files
    let jsFiles = catMaybes mbJsFiles
        pagesMods' = case pagesMods of
                        [] | any ((=="JSMain") . takeBaseName) jsFiles -> [mkModuleName "JSMain"]
                        []                                             -> [mkModuleName "Main"]
                        _                                              -> pagesMods
    closureArgs <- variantLink var jsexe importPaths jsFiles pagesMods'
    writeFile (jsexe </> "closure.args") $ unwords closureArgs
  where
    ext = variantExtension var
    mbFile f = do
        exists <- doesFileExist f
        if exists
            then return $ Just f
            else return Nothing

-- | Find all the import paths in these and the preload packages
getPackageImportPaths :: DynFlags -> [PackageId] -> IO [FilePath]
getPackageImportPaths dflags pkgs =
  collectImportPaths `fmap` getPreloadPackagesAnd dflags pkgs

collectImportPaths :: [PackageConfig] -> [FilePath]
collectImportPaths ps = nub (filter notNull (concatMap importDirs ps))

jsexeFileName :: Variant -> DynFlags -> FilePath
jsexeFileName var dflags
  | Just s <- outputFile dflags = dropExtension s ++ variantExeExtension var
  | otherwise =
#if defined(mingw32_HOST_OS)
        "main"
#else
        "a"
#endif
        ++ variantExeExtension var

#endif
