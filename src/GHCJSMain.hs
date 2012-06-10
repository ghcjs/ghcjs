{-# LANGUAGE CPP #-}
module GHCJSMain (writeJavaScriptModule, linkJavaScript, CallingConvention(..)) where

import Id (Id)
import HscTypes (ModSummary(..), CgGuts (..))
import StgSyn (StgBinding)
import CostCentre (CollectedCCs)
import Module (ml_hi_file, moduleNameString, moduleName)
import Distribution.Verbosity (normal)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import DynFlags (DynFlags(..))
import Module (PackageId, Module)
import System.FilePath (replaceExtension, dropExtension, replaceExtension, (</>), (<.>), takeBaseName)
import System.Directory (doesFileExist)
import Packages (getPreloadPackagesAnd, PackageConfig, importDirs)
import Data.List (nub)
import Util (notNull)
import ErrUtils (debugTraceMsg)
import Outputable ((<+>), ptext, text)
import FastString (sLit)
import Data.Maybe (catMaybes)

import qualified Generator.Helpers as Js (runGen, newGenState)
import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js
import qualified Generator.Link as Js (link)
import Compiler.Variants (Variant (..))

data CallingConvention = Plain | Trampoline

writeJavaScriptModule :: Variant -> ModSummary -> CgGuts
        -> ([(StgBinding,[(Id,[Id])])], CollectedCCs) -> IO ()
writeJavaScriptModule var summary tidyCore (stg', _ccs) =
  do let program = variantRender var stg' (ms_mod summary)
     putStrLn $ concat ["Writing module ", name, " (to ", outputFile, ")"]
     writeFile outputFile program
   where ext = variantExtension var
         outputFile = replaceExtension (ml_hi_file . ms_location $ summary) ext
         name = moduleNameString . moduleName . ms_mod $ summary

linkJavaScript :: Variant -> DynFlags -> [FilePath] -> [PackageId] -> [Module] -> IO ()
linkJavaScript var dyflags o_files dep_packages pagesMods = do
    let jsexe = jsexeFileName dyflags
    importPaths <- getPackageImportPaths dyflags dep_packages
    debugTraceMsg dyflags 1 (ptext (sLit "JavaScript Linking") <+> text jsexe
                             <+> text "...")
    createDirectoryIfMissingVerbose normal False jsexe
    mbJsFiles <- mapM (mbFile . (flip replaceExtension ext)) o_files
    let jsFiles = catMaybes mbJsFiles
    closureArgs <- Js.link var (jsexe++"/") importPaths jsFiles pagesMods []
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

jsexeFileName :: DynFlags -> FilePath
jsexeFileName dflags
  | Just s <- outputFile dflags = dropExtension s <.> "jsexe"
  | otherwise =
#if defined(mingw32_HOST_OS)
        "main.jsexe"
#else
        "a.jsexe"
#endif

