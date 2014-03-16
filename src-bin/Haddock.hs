{-# Language CPP #-}

{- | Haddock wrapper for GHCJS
 -}
module Main where

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
  args <- getArgs
  case args of
    ["--ghc-version"] -> putStrLn getCompilerVersion
    xs                -> do
      baseDir <- getGlobalPackageBase
      libDir  <- getGlobalPackageInst
      env     <- getEnvironment
      let extraArgs = "-B" : libDir : []
          pkgConf d = d </> "package.conf.d"
          ghcjsPkgPath  = pkgConf baseDir ++ pathSep ++ pkgConf libDir
          pkgPath   = maybe ghcjsPkgPath (\x -> x ++ pathSep ++ ghcjsPkgPath) (lookup "GHC_PACKAGE_PATH" env)
          runEnv    = ("GHC_PACKAGE_PATH", pkgPath) : filter ((/="GHC_PACKAGE_PATH").fst) env
      exitWith =<< waitForProcess =<< runProcess ("haddock") (extraArgs ++ xs)
                                        Nothing (Just runEnv) Nothing Nothing Nothing

