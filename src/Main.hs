module Main where

import GHC
import CoreToStg
import GHC.Paths ( libdir )
import DynFlags ( defaultDynFlags )
import HscTypes
import Panic
import CorePrep

import System.Environment
import Control.Monad
import Data.Maybe
import Data.List

import qualified Generator.TopLevel as JsGen (generate)

main :: IO ()
main = do 
    args <- getArgs
    when (elem "--help" args) $ ghcError (ProgramError usage)
    cfs <- defaultErrorHandler defaultDynFlags $ do
        runGhc (Just libdir) $ do
            sdflags <- getSessionDynFlags
            (dflags, fileargs', _) <- parseDynamicFlags sdflags (map noLoc args) 
            when (null fileargs') $ ghcError (UsageError "No input files.")
            _ <- setSessionDynFlags dflags
            let fileargs = map unLoc fileargs'
            targets <- mapM (\x -> guessTarget x Nothing) fileargs
            setTargets targets
            mgraph <- depanal [] False
            let files = filter (not . isSuffixOf "boot") 
                            . map (extractPath . ms_location) $ mgraph
                extractPath l = fromMaybe (ml_hi_file l) (ml_hs_file l)
            setTargets []
            cs <- mapM compileToCoreSimplified files
            return $ zip cs (map ((++".js") .  stripFileExt) files)
    putStrLn $ "Translating STG to JS for: " ++ show (map snd cfs)
    mapM_ (uncurry compileCore) cfs

compileCore :: CoreModule -> FilePath -> IO ()
compileCore c fp = do 
    let cmod = cm_module c
    core' <- corePrepPgm defaultDynFlags (cm_binds c) [] 
    stg <- coreToStg (modulePackageId cmod) core'
    prog <- JsGen.generate cmod (cm_imports c) stg 
    -- Custom output paths are ignored
    let program = show prog
    putStrLn $ "Writing " ++ fp
    writeFile fp program 

usage :: [Char]
usage = "Haskell to Javascript compiler (via GHC)\n\n\
        \\tUsage: ghcjs [command-line-options-and-input-files]\n"

stripFileExt :: String -> String 
stripFileExt fn = let safeLast x = if (null x) then Nothing else Just (last x)
                  in maybe fn (flip take fn) (safeLast . elemIndices '.' $ fn)

