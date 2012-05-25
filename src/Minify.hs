module Main(main) where

import qualified Data.Set as S

import Data.List (intercalate)

import System.Environment (getArgs)
import System.Exit (exitFailure, exitWith, ExitCode(..))
import System.Process (system)
import System.FilePath ((</>))

main :: IO ()
main = do
    args <- getArgs
    case args of
        (jsexe:rest) -> do
            closureArgs <- readFile $jsexe </> "closure.args"
            checkExit . system . intercalate " " $ [
                "java",
                "-Xmx1G",
                "-jar", "~/closure-compiler/compiler.jar",
                "--js", "~/closure-library/closure/goog/base.js",
                "--js", "~/closure-library/closure/goog/object/object.js",
                "--js", "~/closure-library/closure/goog/string/string.js",
                "--js", "~/closure-library/closure/goog/debug/error.js",
                "--js", "~/closure-library/closure/goog/asserts/asserts.js",
                "--js", "~/closure-library/closure/goog/array/array.js",
                "--js", "~/closure-library/closure/goog/math/long.js",
                "--js", "~/closure-library/closure/goog/math/integer.js",
                "--js", "~/closure-library/closure/goog/debug/relativetimeprovider.js",
                "--js", "~/closure-library/closure/goog/debug/formatter.js",
                "--js", "~/closure-library/closure/goog/structs/structs.js",
                "--js", "~/closure-library/closure/goog/structs/collection.js",
                "--js", "~/closure-library/closure/goog/iter/iter.js",
                "--js", "~/closure-library/closure/goog/structs/map.js",
                "--js", "~/closure-library/closure/goog/structs/set.js",
                "--js", "~/closure-library/closure/goog/useragent/useragent.js",
                "--js", "~/closure-library/closure/goog/debug/debug.js",
                "--js", "~/closure-library/closure/goog/debug/logrecord.js",
                "--js", "~/closure-library/closure/goog/debug/logbuffer.js",
                "--js", "~/closure-library/closure/goog/debug/logger.js",
                "--js", "~/closure-library/closure/goog/debug/console.js",
                "--js", "~/closure-library/closure/goog/crypt/hash.js",
                "--js", "~/closure-library/closure/goog/crypt/md5.js",
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

