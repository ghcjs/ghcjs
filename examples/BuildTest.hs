{-# LANGUAGE CPP #-}
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

#if __GLASGOW_HASKELL__ == 700
defaultGhcSource = "../ghc-7.0.4"
#elif __GLASGOW_HASKELL__ == 702
defaultGhcSource = "../ghc-7.2.2"
#else
defaultGhcSource = "../ghc"
#endif

main = do
    args <- getArgs
    putStrLn $ show args
    let (conv, ghcp) = case args of
            (conv:ghcp:_) -> (conv, ghcp)
            []            -> ("trampoline",defaultGhcSource)
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
        "--js", concat [dstPath env, "/rts-common.js"],
        "--define='HS_DEBUG=false'",
        "--js", concat [dstPath env, "/rts-", conv, ".js"],
        "--module", "rts:3:",
        "--hjs", concat [dstPath env, "/ghc-prim"],
        "--hjs", concat [dstPath env, "/integer-simple"],
        "--hjs", concat [dstPath env, "/base"],
        "--hjs", concat [dstPath env, "/main"],
        "--js", "examples/TestJS.js",
        "--module", "test:2:rts",
        "--module_output_path_prefix", concat [dstPath env, "/out/"],
        "--compilation_level", "ADVANCED_OPTIMIZATIONS"]
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
#if __GLASGOW_HASKELL__ >= 703
      , "GHC.Classes"
#elif __GLASGOW_HASKELL__ >= 702
      , "GHC.Ordering"
      , "GHC.Unit"
      , "GHC.IntWord32"
#else
      , "GHC.Bool"
#endif
      ]
    )
  , ("integer-simple"
    , [ "GHC.Integer"
#if __GLASGOW_HASKELL__ >= 702
      , "GHC.Integer.Logarithms"
      , "GHC.Integer.Logarithms.Internals"
#endif
      ])
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

