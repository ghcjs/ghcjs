module Main where

import Control.Monad (when)

import System.Cmd
import System.Environment
import System.FilePath
import System.Directory
import System.Info
import System.Exit

import Compiler.Info

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

ghcjsPkg :: [String] -> [String] -> String -> String -> IO ()
ghcjsPkg args pkgArgs gPkgConf uPkgConf
    | any (=="initglobal") args   =
        ghcPkg gPkgConf uPkgConf ["init", gPkgConf]
    | any (=="inituser") args     =
        ghcPkg gPkgConf uPkgConf ["init", uPkgConf]
    | any (=="--version") args    = do
        putStrLn $ "GHCJS package manager version " ++ getGhcCompilerVersion
        exitSuccess
    | any (`elem`["--global-package-db","--no-user-package-conf","--no-user-package-db"]) args =
        ghcPkgPlain $ args ++ pkgArgs
    | any (=="update") args = do
        ghcPkg gPkgConf uPkgConf (args ++ ["--package-conf="++gPkgConf,"--package-conf="++uPkgConf])
    | otherwise                   =
        ghcPkg gPkgConf uPkgConf args

-- fixme correct separator for windows
ghcPkg globalDb userDb args0 = do
  ph <- runProcess "ghc-pkg" args2 Nothing (Just [("GHC_PACKAGE_PATH", path)]) Nothing Nothing Nothing
  exitWith =<< waitForProcess ph
   where
     gu  = ["--global", "--user"]
     args1 | all (`elem` args0) gu = filter (`notElem` gu) args0
           | otherwise = filter (/="--user") args0
     args2 = args1 ++ ["--global-package-db", globalDb]
     path  = userDb ++ ":" ++ globalDb

ghcPkgPlain :: [String] -> IO ()
ghcPkgPlain args = do
  ph <- runProcess "ghc-pkg" args Nothing Nothing Nothing Nothing Nothing
  exitWith =<< waitForProcess ph

