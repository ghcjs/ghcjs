-- this module is based on lhc-pkg/Main.hs from LHC
module Main where

import System.Cmd
import System.Environment
import System.FilePath
import System.Directory
import System.Info
import System.Exit

import Compiler.Info

import Data.List (isPrefixOf)

import System.Process (runProcess, waitForProcess)

main :: IO ()
main = do gPkgConf <- getGlobalPackageDB
          uPkgConf <- getUserPackageDB
          args <- getArgs
          ghcjsPkg args gPkgConf uPkgConf

ghcjsPkg :: [String] -> String -> String -> IO ()
ghcjsPkg args gPkgConf uPkgConf
    | any (=="initglobal") args   =
        ghcPkg gPkgConf ["init", gPkgConf]
    | any (=="inituser") args     =
        ghcPkg gPkgConf ["init", uPkgConf]
    | any (=="--version") args    = do
        putStrLn $ "GHCJS package manager version " ++ getCompilerVersion
        exitSuccess
    | any ("--package-conf" `isPrefixOf`) args =
        ghcPkg gPkgConf args
    | any ("--global-conf" `isPrefixOf`) args =
        ghcPkgPlain args
    | any (=="--no-user-package-conf") args =
        ghcPkgPlain args
    | any (=="--global") args = -- if global, flip package conf arguments (rightmost one is used by ghc-pkg)
        ghcPkg gPkgConf $ args ++ [ "--package-conf=" ++ uPkgConf
                                  , "--package-conf=" ++ gPkgConf
                                  ]
    | otherwise                   =
        ghcPkg gPkgConf $ args ++ [ "--package-conf=" ++ gPkgConf
                                  , "--package-conf=" ++ uPkgConf
                                  ]

ghcPkg :: String -> [String] -> IO ()
ghcPkg globaldb args = do
  ph <- runProcess "ghc-pkg" args Nothing (Just [("GHC_PACKAGE_PATH", globaldb)]) Nothing Nothing Nothing
  exitWith =<< waitForProcess ph

ghcPkgPlain :: [String] -> IO ()
ghcPkgPlain args = do
  ph <- runProcess "ghc-pkg" args Nothing Nothing Nothing Nothing Nothing
  exitWith =<< waitForProcess ph

