#!/usr/bin/runghc

import System.Environment (getArgs)
import System.Process (system)
import Data.List (intercalate)
import System.Exit (exitFailure, exitWith, ExitCode(..))

data BuildEnv =
  BuildEnv
    { ghcPath :: String
    , ghcJs :: String
    , dstPath :: String
    }

main = do
    args <- getArgs
    putStrLn $ show args
    let (conv, ghcp) = case args of
            (conv:ghcp:_) -> (conv, ghcp)
            []            -> ("trampoline", "../ghc")
            _             -> error "Invalid arguments for BuildTest"
        env =
               BuildEnv
                 { ghcPath = ghcp
                 , ghcJs = "dist/build/ghcjs/ghcjs"
                 , dstPath = "examples"
                 }
    mapM_ (checkExit . system . uncurry (buildCommand conv env)) packages
    checkExit . system . intercalate " " $ [ ghcJs env, "--calling-convention="++conv,
        "-o", concat [dstPath env, "/main.js"],
        concat ["-i", dstPath env, "/main"], "Test" ]
    checkExit . system . intercalate " " $ [
        "dist/build/ghcjs-link/ghcjs-link",
        "-Xmx1G", "-jar", "~/closure-compiler/compiler.jar",
        "--pages_module", "Test",
        "--js", "~/closure-library/closure/goog/base.js",
        "--js", "../ghcjs-rts/rts-common.js",
        "--define='HS_DEBUG=false'",
        "--js", concat ["../ghcjs-rts/rts-", conv, ".js"],
        "--module", "rts:3:",
        "--hjs", concat [dstPath env, "/ghc-prim"],
        "--hjs", concat [dstPath env, "/integer-simple"],
        "--hjs", concat [dstPath env, "/base"],
        "--hjs", concat [dstPath env, "/main"],
        "--js", "examples/TestJS.js",
        "--module", "test:2:rts",
        "--module_output_path_prefix", concat [dstPath env, "/"],
        "--compilation_level", "ADVANCED_OPTIMIZATIONS",
        "--manage_closure_dependencies"]
    checkExit $ system "open examples/test.html"
  where
    checkExit f = do
        result <- f
        case result of
            ExitSuccess -> return ()
            _           -> exitWith result

packages =
  [ ( "ghc-prim"
    , [ "GHC.Debug"
      , "GHC.Generics"
      , "GHC.IntWord64"
      , "GHC.Tuple"
      , "GHC.Types"
      , "GHC.Classes"
      ]
    )
  , ("integer-simple", ["GHC.Integer","GHC.Integer.Logarithms","GHC.Integer.Logarithms.Internals"])
  , ("base", ["Prelude"])
  ]

buildCommand conv env package modules =
  intercalate " " $
    [ ghcJs env
    , "--calling-convention="++conv
    , "-o", concat [dstPath env, "/", package, ".js"]
    , "-odir", concat [dstPath env, "/", package]
    , "-hidir", concat [dstPath env, "/", package]
    , "-package-name", package
    , extentionOptions
    , concat ["-I", ghcPath env, "/include"]
    , concat ["-I", ghcPath env, "/libraries/", package, "/include"]
    , concat ["-i", ghcPath env, "/libraries/", package, "/dist-install/build"]
    , concat ["-i", ghcPath env, "/libraries/", package]
    ] ++ modules

extentionOptions =
  intercalate " " $ map ("-X" ++) $
    [ "MagicHash"
    , "ExistentialQuantification"
    , "Rank2Types"
    , "ScopedTypeVariables"
    , "UnboxedTuples"
    , "ForeignFunctionInterface"
    , "UnliftedFFITypes"
    , "DeriveDataTypeable"
    , "GeneralizedNewtypeDeriving"
    , "FlexibleInstances"
    , "StandaloneDeriving"
    , "PatternGuards"
    , "EmptyDataDecls"
    , "NoImplicitPrelude"
    , "CPP"
    ]

