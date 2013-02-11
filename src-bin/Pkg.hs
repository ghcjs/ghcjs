-- this module is based on lhc-pkg/Main.hs from LHC
module Main where

import Control.Monad (when)

import System.Cmd
import System.Environment
import System.FilePath
import System.Directory
import System.Info
import System.Exit

import Compiler.Info
import Compiler.Cache

import Data.List (partition, isPrefixOf, intercalate)

import System.Process (runProcess, waitForProcess)

main :: IO ()
main = do args <- getArgs
          logCmd <- getEnvOpt "GHCJS_LOG_COMMANDLINE"
          when logCmd $ do
            dir <- getAppUserDataDirectory "ghcjs"
            createDirectoryIfMissing True dir
            appendFile (dir </> "cmd.log") (intercalate " " ("ghcjs-pkg" : args) ++ "\n")
          gPkgConf <- getGlobalPackageDB
          uPkgConf <- getUserPackageDB
          let (pkgArgs, args') = partition ("--pkg-conf" `isPrefixOf`) args
          ghcjsPkg args' pkgArgs gPkgConf uPkgConf
          when (any (`elem` ["register", "update"]) args')
             (installFromCache >> emptyCache)

ghcjsPkg :: [String] -> [String] -> String -> String -> IO ()
ghcjsPkg args pkgArgs gPkgConf uPkgConf
    | any (=="initglobal") args   =
        ghcPkg gPkgConf ["init", gPkgConf]
    | any (=="inituser") args     =
        ghcPkg gPkgConf ["init", uPkgConf]
    | any (=="--version") args    = do
        putStrLn $ "GHCJS package manager version " ++ getGhcCompilerVersion
        exitSuccess
    | any ("--global-conf" `isPrefixOf`) args =
        ghcPkgPlain $ args ++ pkgArgs
    | any (=="--no-user-package-conf") args =
        ghcPkgPlain $ args ++ pkgArgs
    | any (=="--global") args  && not (any (=="--user") args) = -- if global, flip package conf arguments (rightmost one is used by ghc-pkg)
        ghcPkg gPkgConf $ args ++ [ "--package-conf=" ++ uPkgConf
                                  , "--package-conf=" ++ gPkgConf
                                  ] ++ pkgArgs
    | otherwise                   =
        ghcPkg gPkgConf $ args ++ [ "--package-conf=" ++ gPkgConf
                                  , "--package-conf=" ++ uPkgConf
                                  ] ++ pkgArgs

ghcPkg :: String -> [String] -> IO ()
ghcPkg globaldb args = do
  ph <- runProcess "ghc-pkg" args Nothing (Just [("GHC_PACKAGE_PATH", globaldb)]) Nothing Nothing Nothing
  exitWith =<< waitForProcess ph

ghcPkgPlain :: [String] -> IO ()
ghcPkgPlain args = do
  ph <- runProcess "ghc-pkg" args Nothing Nothing Nothing Nothing Nothing
  exitWith =<< waitForProcess ph

