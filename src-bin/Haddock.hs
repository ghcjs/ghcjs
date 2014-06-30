{-# Language CPP #-}

{-
  Haddock wrapper for GHCJS
 -}

module Main where

import Data.List (partition, isPrefixOf)
import Data.Maybe

import System.Environment
import System.Exit
import System.FilePath
import System.Process

import Compiler.Info

pathSep :: String
#ifdef WINDOWS
pathSep = ";"
#else
pathSep = ":"
#endif

main = do
  args0 <- getFullArguments -- adds wrapper arguments for Windows
  let (minusB, args) = partition ("-B" `isPrefixOf`) $ args0
      mbMinusB       = listToMaybe . reverse $ minusB
  case args of
    ["--ghc-version"] -> putStrLn getCompilerVersion
    xs                -> do
      libDir  <- mkTopDir mbMinusB
      env     <- getEnvironment
      userDB  <- getUserPackageDir
      let extraArgs = "-B" : libDir : []
          -- fixme user db can also be a package.conf
          ghcjsPkgPath  = getGlobalPackageDB libDir ++ maybe "" (\d -> pathSep++d</>"package.conf.d") userDB
          pkgPath   = maybe ghcjsPkgPath (\x -> x ++ pathSep ++ ghcjsPkgPath) (lookup "GHCJS_PACKAGE_PATH" env)
          runEnv    = ("GHCJS_PACKAGE_PATH", pkgPath) : filter ((/="GHCJS_PACKAGE_PATH").fst) env
      exitWith =<< waitForProcess =<< runProcess ("haddock") (extraArgs ++ xs)
                                        Nothing (Just runEnv) Nothing Nothing Nothing

