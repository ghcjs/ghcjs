module Main(main) where

import qualified Data.Set as S

import Data.List (intercalate)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Process (system)

import Generator.Link (link)

-- As well as the normal closure-compiler options this function accepts
--     --haskell_output_path_prefix (overrides --module_output_path_prefix for ghcjs-link output)
--     --hjs Specifies directories to look in for ghcjs generated .js
--     --pages_module Main
--     --page_function Main.main
-- A "page" is an function that we want to be able to load quickly on demand
--
main :: IO ()
main = do
    args <- getArgs
    let out = head (getOpts "--haskell_output_path_prefix" args
                 ++ getOpts "--module_output_path_prefix" args ++ ["all"])
        dirs = getOpts "--hjs" args
        pageModules = getOpts "--pages_module" args
        pageFunctions = getOpts "--page_function" args
        isHaskellOpt "--hjs" = True
        isHaskellOpt "--haskell_output_path_prefix" = True
        isHaskellOpt "--pages_module" = True
        isHaskellOpt "--page_function" = True
        isHaskellOpt _ = False
        (initArgs', remainingArgs') = span (/= "--hjs") args
        initArgs      = filterOut isHaskellOpt initArgs'
        remainingArgs = filterOut isHaskellOpt remainingArgs'

    closureArgs <- link out dirs pageModules pageFunctions
    checkExit . system . intercalate " " $ ["java"] ++ initArgs ++ closureArgs ++ remainingArgs
  where
    checkExit f = do
        result <- f
        case result of
            ExitSuccess -> return ()
            _           -> exitWith result

    getOpts opt (o:v:rest) | o == opt = v:(getOpts opt rest)
    getOpts opt (_:rest) = getOpts opt rest
    getOpts opt [] = []

    filterOut isOpt (o:_:rest) | isOpt o = filterOut isOpt rest
    filterOut isOpt (x:rest) = x:(filterOut isOpt rest)
    filterOut isOpt [] = []
