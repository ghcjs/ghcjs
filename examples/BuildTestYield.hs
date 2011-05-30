#!/usr/bin/runghc

import System.Environment (getArgs)
import System.Process (system)
import Data.List (intercalate)

data BuildEnv =
  BuildEnv
    { ghcPath :: String
    , ghcJs :: String
    , dstPath :: String
    }

main =
  do (ghcp:_) <- getArgs
     let env =
           BuildEnv
             { ghcPath = ghcp
             , ghcJs = "../dist/build/ghcjs/ghcjs"
             , dstPath = "."
             }
     mapM_ (system . uncurry (buildCommand env)) packages
     system . intercalate " " $ [ ghcJs env, "--calling-convention=yield", concat ["-i", dstPath env, "/main"], "Test" ]

packages =
  [ ( "ghc-prim"
    , [ "GHC.Bool"
      , "GHC.Debug"
      , "GHC.Generics"
      , "GHC.IntWord32"
      , "GHC.IntWord64"
      , "GHC.Ordering"
      , "GHC.Tuple"
      , "GHC.Types"
      , "GHC.Unit"
      ]
    )
  , ("integer-simple", ["GHC.Integer"])
  , ("base", ["Prelude"])
  ]

buildCommand env package modules =
  intercalate " " $
    [ ghcJs env
    , "--calling-convention=yield"
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

