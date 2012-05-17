-- this module is based on lhc-pkg/Main.hs from LHC
module Main where

import System.Cmd
import System.Environment
import System.FilePath
import System.Directory
import System.Info
import System.Exit

import Compiler.Info

main :: IO ()
main = do pkgConf <- getGlobalPackageDB
          args <- getArgs
          if any (=="--version") args
            then putStrLn $ "GHCJS package manager version " ++ getCompilerVersion
            else exitWith =<< system (unwords (["ghc-pkg"]++ args ++
                                               ["--package-conf=" ++ pkgConf
                                               ,"--global-conf=" ++ pkgConf
                                               ,"--no-user-package-conf"]))


