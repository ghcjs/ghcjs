-- this module is based on lhc-pkg/Main.hs from LHC
module Main where

import System.Cmd
import System.Environment
import System.FilePath
import System.Directory
import System.Info
import System.Exit
import Data.Version as Version
import Paths_ghcjs

main :: IO ()
main = do pkgConf <- getPkgConf
          args <- getArgs
          case args of
            ["--version"] -> putStrLn $ "GHCJS package manager version " ++ Version.showVersion version
            _other        -> exitWith =<< system (unwords (["ghc-pkg"]++ args ++
                                                           ["--package-conf=" ++ pkgConf
                                                           ,"--global-conf=" ++ pkgConf
                                                           ,"--no-user-package-conf"]))

getPkgConf
    = do appdir <- getAppUserDataDirectory "ghcjs"
         let targetARCH = arch
             targetOS   = os
         let subdir = targetARCH ++ '-':targetOS ++ '-':Version.showVersion version
         return (appdir </> subdir </> "package.conf.d")

