module Generator.Minify (
    minify,
    MinifyOptions(..),
    defaultMinify
) where

import Control.Applicative ((<$>))
import System.FilePath ((</>))
import Data.List (stripPrefix, intercalate)
import System.Process (system)
import System.Exit (ExitCode, ExitCode(..))

import Closure.Paths
import qualified GHCJS.HTerm as HT (getDataFileName)
import Compiler.Variants
import RTS.Dependencies
import System.Directory (setCurrentDirectory, doesFileExist, copyFile)
import qualified Data.ByteString.Lazy as B (readFile, writeFile)
import Paths_ghcjs
import Data.SourceMap
import Data.Attoparsec.ByteString.Lazy (many1, parse, Result(..))
import Control.Monad (forM, forM_, unless)
import Data.Aeson (json, fromJSON, Result(..), encode)
import Compiler.Variants (variants)

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
minify jsexebase mainFiles options args = do
    results <- forM variants $ \ variant -> do
        let jsexe = jsexebase ++ variantExeExtension variant
        closurePath <- closureLibraryPath
        htermPath <- HT.getDataFileName "js/"
        rtsPath <- getDataFileName "rts/"

        let copyIfNotThere f = do
              exists <- doesFileExist (jsexe </> f)
              unless exists $ copyFile (rtsPath </> f) (jsexe </> f)

        copyIfNotThere "hterm.html"
        copyIfNotThere "hterm.js"

        copyIfNotThere "console.html"
        copyIfNotThere "console.js"

        copyIfNotThere "index.html"

        let mainFiles' = case mainFiles of
                            [] -> [jsexe </> "hterm.js"]
                            _  -> mainFiles

        firstLine <- head . lines <$> (readFile $ jsexe </> "hsloader.js")
        let bundleCount = case stripPrefix "// Bundle Count " firstLine of
                            Just n  -> read n
                            Nothing -> 0

        cj <- closureCompilerJar
        defOpts <- rtsDefaultOptions
        deps <- rtsDeps variant
        ec <- system . intercalate " " $
                    [ "java"
                    , "-Xmx1G"
                    , "-jar", cj
                    ] ++ js defOpts                                    ++
                    [ define "HS_DEBUG" hsDebug
                    , define "HS_TRACE" hsTrace
                    , define "HS_WEAKS" hsWeaks
                    ] ++ jss deps                                      ++
                    mod "rts" "" (length deps + 1)                     ++
                    bundleMods jsexe bundleCount                             ++
                    js (jsexe </> "hsloader.js")                       ++
                    jss mainFiles'                                     ++
                    mod "main" "rts" (length mainFiles' + 1)           ++
                    [ "--module_output_path_prefix", jsexe ++ "/"
                    , "--compilation_level", "ADVANCED_OPTIMIZATIONS"
                    , "--create_source_map", jsexe </> "hsmin.js.allmaps"
                    , "--source_map_format", "V3"
                    ] ++ args

        -- Make paths in the source map relative and split it up
        let fixMap sm@SourceMap{sources = s} = sm {sources = map fixPath s}
            fixPath =   replacePrefix closurePath  ("../closure-library"</>)
                      . replacePrefix htermPath    ("../hterm/js"</>)
                      . replacePrefix rtsPath      ("../rts"</>)
                      . replacePrefix (jsexe++"/") id
            replacePrefix a with p = maybe p with (stripPrefix a p)
        case ec of
            ExitSuccess -> do
                allmaps <- B.readFile $ jsexe </> "hsmin.js.allmaps"
                case parse (many1 json) allmaps of
                    Fail _ _ctx msg -> do
                        putStrLn $ "Error processing source maps : " ++ msg
                        return $ ExitFailure 1
                    Done _ maps -> do
                        forM_ maps $ \m -> do
                            case fromJSON m of
                                Error msg -> do
                                    putStrLn $ "Error processing source map : " ++ msg
                                Success sm -> do
                                    B.writeFile (jsexe </> file sm ++ ".js.map") (encode $ fixMap sm)
                                    appendFile (jsexe </> file sm ++ ".js") ("\n//@ sourceMappingURL=" ++ file sm ++ ".js.map")
                        return ExitSuccess
            _ -> return ec
    case filter (/=ExitSuccess) results of
        (ec:_) -> return ec
        _      -> return ExitSuccess
  where
    define s f       = "--define='" ++ s ++ if f options then "=true'" else "=false'"
    js f             = ["--js", f]
    jss              = concatMap js
    mod m d n        = ["--module", intercalate ":" [m, show n, d]]
    jsMod m d n f    = ["--js", f] ++ mod m d n
    bundleMods jsexe count = concatMap
                         (\x -> jsMod ("hs"++show x++"min") "rts" 1
                                (jsexe </> "hs" ++ show x ++ ".js"))
                         [1..count]
