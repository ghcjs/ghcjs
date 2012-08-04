module Main(main) where

import qualified Data.Set as S

import Data.List (intercalate)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Process (system)
import System.FilePath ((</>))
import Generator.Minify (minify, defaultMinify)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (jsexe:rest) -> do
            checkExit $ minify jsexe rest defaultMinify []
        _ -> putStrLn "Usage : ghcjs-min <jsexe> [<jsfile>,..]"
  where
    checkExit f = do
        result <- f
        case result of
            ExitSuccess -> return ()
            _           -> exitWith result

