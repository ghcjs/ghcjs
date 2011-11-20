module GHCJSMain (writeJavaScriptModule, linkJavaScript, CallingConvention(..)) where

import Id (Id)
import HscTypes (ModSummary(..), CgGuts (..))
import StgSyn (StgBinding)
import CostCentre (CollectedCCs)
import Module (ml_hi_file, moduleNameString, moduleName)
import Distribution.Verbosity (normal)
import System.FilePath (replaceExtension)
import Distribution.Simple.Utils (createDirectoryIfMissingVerbose)
import DynFlags (DynFlags(..))
import Module (PackageId)
import System.FilePath (dropExtension, (</>), (<.>))
import Packages (getPreloadPackagesAnd, PackageConfig, importDirs)
import Data.List (nub)
import Util (notNull)
import ErrUtils (debugTraceMsg)
import Outputable ((<+>), ptext, text)
import FastString (sLit)

import qualified Generator.Helpers as Js (runGen, newGenState)
import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js
import qualified Generator.Link as Js (link)

data CallingConvention = Plain | Trampoline

writeJavaScriptModule :: CallingConvention -> ModSummary -> CgGuts
        -> ([(StgBinding,[(Id,[Id])])], CollectedCCs) -> IO ()
writeJavaScriptModule callingConvention summary tidyCore (stg', _ccs) =
  do program <- concreteJavascript callingConvention tidyCore (stg', _ccs)
     putStrLn $ concat ["Writing module ", name, " (to ", outputFile, ")"]
     writeFile outputFile program
  where outputFile = replaceExtension (ml_hi_file . ms_location $ summary) ".js"
        name = moduleNameString . moduleName . ms_mod $ summary

concreteJavascript :: CallingConvention -> CgGuts
        -> ([(StgBinding,[(Id,[Id])])], CollectedCCs) -> IO String
concreteJavascript callingConvention core (stg', _ccs) =
     let abstract :: Javascript js => js
         abstract = fst $ Js.runGen (Js.generate (cg_module core) stg') Js.newGenState in
     return $
       case callingConvention
       of Plain -> show (abstract :: Js.Formatted)
          Trampoline -> show (abstract :: Js.Trampoline Js.Formatted)

linkJavaScript :: DynFlags -> [PackageId] -> IO ()
linkJavaScript dyflags dep_packages = do
    let jsexe = jsexeFileName dyflags
    importPaths <- getPackageImportPaths dyflags dep_packages
    debugTraceMsg dyflags 1 (ptext (sLit "Java Script Linking") <+> text jsexe
                             <+> text "...")
    createDirectoryIfMissingVerbose normal False jsexe
    closureArgs <- Js.link (jsexe++"/") importPaths ["Main"] []
    writeFile (jsexe </> "closure.args") $ unwords closureArgs

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

