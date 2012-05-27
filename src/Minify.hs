module Main(main) where

import qualified Data.Set as S

import Data.List (intercalate)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Process (system)
import System.FilePath ((</>))

import Closure.Paths

main :: IO ()
main = do
    args <- getArgs
    d <- closureLibraryPath
    cj <- closureCompilerJar
    case args of
        (jsexe:rest) -> do
            closureArgs <- readFile $jsexe </> "closure.args"
            checkExit . system . intercalate " " $ [
                "java",
                "-Xmx1G",
                "-jar", cj,
                "--js", d </> "closure/goog/base.js",
                "--js", d </> "closure/goog/object/object.js",
                "--js", d </> "closure/goog/string/string.js",
                "--js", d </> "closure/goog/debug/error.js",
                "--js", d </> "closure/goog/asserts/asserts.js",
                "--js", d </> "closure/goog/array/array.js",
                "--js", d </> "closure/goog/math/long.js",
                "--js", d </> "closure/goog/math/integer.js",
                "--js", d </> "closure/goog/debug/relativetimeprovider.js",
                "--js", d </> "closure/goog/debug/formatter.js",
                "--js", d </> "closure/goog/structs/structs.js",
                "--js", d </> "closure/goog/structs/collection.js",
                "--js", d </> "closure/goog/iter/iter.js",
                "--js", d </> "closure/goog/structs/map.js",
                "--js", d </> "closure/goog/structs/set.js",
                "--js", d </> "closure/goog/useragent/useragent.js",
                "--js", d </> "closure/goog/debug/debug.js",
                "--js", d </> "closure/goog/debug/logrecord.js",
                "--js", d </> "closure/goog/debug/logbuffer.js",
                "--js", d </> "closure/goog/debug/logger.js",
                "--js", d </> "closure/goog/debug/console.js",
                "--js", d </> "closure/goog/crypt/hash.js",
                "--js", d </> "closure/goog/crypt/md5.js",
                "--js", jsexe </> "rts.jso/rts-options.js",
                "--define='HS_DEBUG=false'",
                "--define='HS_TRACE=false'"
                ] ++ rest ++ [
                "--js", jsexe </> "rts.jso/rts-common.js",
                "--js", jsexe </> "rts.jso/rts-trampoline.js",
                "--module", "rts:24:",
                closureArgs,
                "--module", "main:1:rts",
                "--module_output_path_prefix", jsexe ++ "/",
                "--compilation_level", "ADVANCED_OPTIMIZATIONS"]
  where
    checkExit f = do
        result <- f
        case result of
            ExitSuccess -> return ()
            _           -> exitWith result

