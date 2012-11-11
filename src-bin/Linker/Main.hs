module Main(main) where

import qualified Data.Set           as S

import           Data.List          (intercalate)

import           System.Environment (getArgs)
import           System.Exit        (ExitCode (..), exitFailure, exitWith)
import           System.Process     (system)

import           Compiler.Variants
import           Control.Monad      (forM)
import           Generator.Link     (link)

-- As well as the normal closure-compiler options this function accepts
--     --haskell_output_path_prefix (overrides --module_output_path_prefix for ghcjs-link output)
--     --hjs Specifies directories to look in for ghcjs generated .js
--     --hjs_file ghcjs generated .js file
--     --pages_module Main
-- A "page" is an function that we want to be able to load quickly on demand
--
main :: IO ()
main = do
    args <- getArgs
    let out = head (getOpts "--haskell_output_path_prefix" args
                 ++ getOpts "--module_output_path_prefix" args ++ ["all"])
        dirs = getOpts "--hjs" args
        files = getOpts "--hjs_file" args
        pageModules = getOpts "--pages_module" args
        isHaskellOpt "--hjs" = True
        isHaskellOpt "--hjs_file" = True
        isHaskellOpt "--haskell_output_path_prefix" = True
        isHaskellOpt "--pages_module" = True
        isHaskellOpt _ = False
        (initArgs', remainingArgs') = span (\a -> a /= "--hjs" && a /= "--hjs_file") args
        initArgs      = filterOut isHaskellOpt initArgs'
        remainingArgs = filterOut isHaskellOpt remainingArgs'
        pagesModules' = case pagesModules of
                            [] -> ["Main"]
                            _  -> pagesModules
    checkExit . forM variants $ \variant -> do
        closureArgs <- link variant out dirs files (mkModuleName pageModules')
        system . intercalate " " $ ["java"] ++ initArgs ++ closureArgs ++ remainingArgs
  where
    checkExit f = do
        results <- f
        case filter (/= ExitSuccess) results of
            []       -> return ()
            result:_ -> exitWith result

    getOpts opt (o:v:rest) | o == opt = v:(getOpts opt rest)
    getOpts opt (_:rest) = getOpts opt rest
    getOpts opt [] = []

    filterOut isOpt (o:_:rest) | isOpt o = filterOut isOpt rest
    filterOut isOpt (x:rest) = x:(filterOut isOpt rest)
    filterOut isOpt [] = []
