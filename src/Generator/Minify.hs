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
import Compiler.Variants
import RTS.Dependencies

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

    cj <- closureCompilerJar
    defOpts <- rtsDefaultOptions
    deps <- rtsDeps trampolineVariant
    system . intercalate " " $
                [ "java"
                , "-Xmx1G"
                , "-jar", cj
                ] ++ js defOpts                                    ++
                [ define "HS_DEBUG" hsDebug
                , define "HS_TRACE" hsTrace
                , define "HS_WEAKS" hsWeaks
                ] ++ jss deps                                      ++
                mod "rts" "" (length deps + 1)                     ++
                bundleMods bundleCount                             ++
                js (jsexe </> "hsloader.js")                       ++
                jss mainFiles                                      ++
                mod "main" "rts" (length mainFiles + 1)            ++
                [ "--module_output_path_prefix", jsexe ++ "/"
                , "--compilation_level", "ADVANCED_OPTIMIZATIONS"
                ] ++ args
  where
    define s f       = "--define='" ++ s ++ if f options then "=true'" else "=false'"
    js f             = ["--js", f]
    jss              = concatMap js
    mod m d n        = ["--module", intercalate ":" [m, show n, d]]
    jsMod m d n f    = ["--js", f] ++ mod m d n
    bundleMods count = concatMap
                         (\x -> jsMod ("hs"++show x++"min") "rts" 1
                                (jsexe </> "hs" ++ show x ++ ".js"))
                         [1..count]
