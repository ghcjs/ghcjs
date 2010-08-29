{-# LANGUAGE TypeFamilies #-}
module Main where

import GHC
import CoreToStg
import SimplStg
import GHC.Paths ( libdir )
import DynFlags ( defaultDynFlags )
import HscTypes
import Panic
import CorePrep

import Control.Monad
import Data.Maybe
import Data.List
import System.Environment

import System.FilePath

import qualified Generator.TopLevel as Js (generate)
import Javascript.Language (Javascript)
import qualified Javascript.Formatted as Js
import qualified Javascript.Trampoline as Js

data CallingConvention = Plain | Trampoline

main :: IO ()
main =
  do args <- getArgs

     -- FIXME: I wasn't able to find any sane command line parsing
     --        library that allow sending unprocessed arguments to GHC...
     let (callingConvention, args') =
           case args
             of ("--calling-convention=plain":args) -> (Plain, args)
                ("--calling-convention=trampoline":args) -> (Trampoline, args)
                _ -> (Plain, args)
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
               HscTypes.liftIO $
                 do core_binds <- corePrepPgm dflags (cm_binds core) (typeEnvTyCons . cm_types $ core)
                    stg <- coreToStg (modulePackageId . cm_module $ core) core_binds
                    (stg', _ccs) <- stg2stg dflags (cm_module core) stg
                    let prog :: Javascript js => js
                        prog = Js.generate (cm_module core) stg'
                    -- Custom output paths are ignored
                    let program =
                          case callingConvention
                          of Plain -> show (prog :: Js.Formatted)
                             Trampoline -> show (prog :: Js.Trampoline Js.Formatted)
                        fp = replaceExtension file ".js"
                    putStrLn $ "Writing " ++ fp
                    writeFile fp program 

