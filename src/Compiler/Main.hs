module Main where

import GHC
import CoreToStg
import SimplStg
import GHC.Paths ( libdir )
import DynFlags ( defaultDynFlags )
import HscTypes
import Panic
import CorePrep

import System.Environment
import Control.Monad
import Data.Maybe
import Data.List

import qualified Generator.TopLevel as Js (generate)
import qualified Javascript.Simple as Js
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js

data Mode = Simple | Formatted | TailCalledSimple | TailCalledFormatted

parseArgs :: [String] -> (Mode, [String])
parseArgs ("s":rest) = (Simple, rest)
parseArgs ("f":rest) = (Formatted, rest)
parseArgs ("st":rest) = (TailCalledSimple, rest)
parseArgs ("ft":rest) = (TailCalledFormatted, rest)
parseArgs _ = error "Wrong mode!"

main :: IO ()
main =
  do args <- getArgs
     when (null args || elem "--help" args) $ ghcError (ProgramError usage)
     let (mode, args') = parseArgs args
     defaultErrorHandler defaultDynFlags $ runGhc (Just libdir) $
       do sdflags <- getSessionDynFlags
          (dflags, fileargs', _) <- parseDynamicFlags sdflags (map noLoc args') 
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
          flip mapM_ files $ \file ->
            do core <- compileToCoreSimplified file
               liftIO $
                 do core_binds <- corePrepPgm dflags (cm_binds core) (typeEnvTyCons . cm_types $ core)
                    stg <- coreToStg (modulePackageId . cm_module $ core) core_binds
                    (stg', _ccs) <- stg2stg dflags (cm_module core) stg
                    progft <- Js.generate (cm_module core) stg' :: IO (Js.TailCall Js.Formatted)
                    progf <- Js.generate (cm_module core) stg' :: IO Js.Formatted
                    progst <- Js.generate (cm_module core) stg' :: IO (Js.TailCall Js.Simple)
                    progs <- Js.generate (cm_module core) stg' :: IO Js.Simple
                    -- Custom output paths are ignored
                    let program =
                          case mode
                          of Simple -> show progs
                             Formatted -> show progf
                             TailCalledSimple -> show progst
                             TailCalledFormatted -> show progft
                        fp = (++".js") .  stripFileExt $ file
                    putStrLn $ "Writing " ++ fp
                    writeFile fp program 

usage :: [Char]
usage = "Haskell to Javascript compiler (via GHC)\n\n\
        \\tUsage: ghcjs mode [command-line-options-and-input-files]\n\
        \\t\t mode is one of\n\
        \\t\t\t* s - \"simple\"\n\
        \\t\t\t* f - \"formatted\"\n\
        \\t\t\t* st - \"simple\" with tail calls optimization through trampoline\n\
        \\t\t\t* s - \"formatted\" with tail calls optimization through trampoline"

stripFileExt :: String -> String 
stripFileExt fn = let safeLast x = if (null x) then Nothing else Just (last x)
                  in maybe fn (flip take fn) (safeLast . elemIndices '.' $ fn)

