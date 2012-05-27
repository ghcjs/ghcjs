module Generator.Minify (
    minify,
    MinifyOptions(..),
    defaultMinify
) where

import Control.Applicative ((<$>))
import System.FilePath ((</>))
import Data.List (stripPrefix, intercalate)
import System.Process (system)
import System.Exit (ExitCode)
import Closure.Paths
import Paths_ghcjs

data MinifyOptions = MinifyOptions {
    hsDebug :: Bool,
    hsTrace :: Bool,
    hsWeaks :: Bool }

defaultMinify :: MinifyOptions
defaultMinify = MinifyOptions {
    hsDebug = False,
    hsTrace = False,
    hsWeaks = False }

minify :: FilePath -> [FilePath] -> MinifyOptions -> [String] -> IO ExitCode
minify jsexe mainFiles options args = do
    firstLine <- head . lines <$> (readFile $ jsexe </> "hsloader.js")
    let bundleCount = case stripPrefix "// Bundle Count " firstLine of
                        Just n  -> read n
                        Nothing -> 0

    d <- closureLibraryPath
    cj <- closureCompilerJar
    rts <- getDataFileName "rts"
    system . intercalate " " $ [
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
        "--js", rts </> "rts-options.js",
        define "HS_DEBUG" hsDebug,
        define "HS_TRACE" hsTrace,
        define "HS_WEAKS" hsWeaks,
        "--js", rts </> "rts-common.js",
        "--js", rts </> "rts-trampoline.js",
        "--module", "rts:26:"]
        ++ (map (\x -> "--js " ++ jsexe </> ("hs" ++ show x ++ ".js") ++
                        " --module hs" ++ show x ++ "min:1:rts") [1..bundleCount])
        ++ ["--js", jsexe </> "hsloader.js"]
        ++ (map ("--js "++) mainFiles)
        ++ ["--module", "main:"++show (length mainFiles + 1)++":rts",
        "--module_output_path_prefix", jsexe ++ "/",
        "--compilation_level", "ADVANCED_OPTIMIZATIONS"] ++ args
  where
    define s f = "--define='" ++ s ++ if f options then "=true'" else "=false'"

