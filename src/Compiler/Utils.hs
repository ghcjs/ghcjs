{-# LANGUAGE ScopedTypeVariables #-}

-- | Compiler utility functions, mainly dealing with IO and files
module Compiler.Utils
    (
      -- * File utils
      touchFile
    , copyNoOverwrite
    , findFile
    , exeFileName
    , mkGhcjsSuf
    , mkGhcjsOutput
      -- * Source code and JS related utilities
    , ghcjsSrcSpan
    , isJsFile
      -- * User interaction
    , compilationProgressMsg
      -- * Environment
    , getEnvMay
    , getEnvOpt
    ) where

import DynFlags
import DriverPhases
import GHC
import Platform
import SysTools
import SrcLoc
import Util          (looksLikeModuleName)

import qualified Control.Exception as Ex

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class

import Data.Char
import Data.List         (isPrefixOf, partition)

import System.Directory  (doesFileExist, copyFile)
import System.Environment
import System.FilePath

import Gen2.Utils

touchFile :: DynFlags -> FilePath -> IO ()
touchFile df file = do
  e <- doesFileExist file
  when e (touch df "keep build system happy" file)

copyNoOverwrite :: FilePath -> FilePath -> IO ()
copyNoOverwrite from to = do
  ef <- doesFileExist from
  et <- doesFileExist to
  when (ef && not et) (copyFile from to)

findFile :: (FilePath -> FilePath)      -- Maps a directory path to a file path
         -> [FilePath]                  -- Directories to look in
         -> IO (Maybe FilePath)         -- The first file path to match
findFile _            [] = return Nothing
findFile mk_file_path (dir : dirs)
  = do let file_path = mk_file_path dir
       b <- doesFileExist file_path
       if b then return (Just file_path)
            else findFile mk_file_path dirs

exeFileName :: DynFlags -> FilePath
exeFileName dflags
  | Just s <- outputFile dflags =
      -- unmunge the extension
      let s' = dropPrefix "js_" (drop 1 $ takeExtension s)
      in if null s'
           then dropExtension s <.> "jsexe"
           else dropExtension s <.> s'
  | otherwise =
      if platformOS (targetPlatform dflags) == OSMinGW32
           then "main.jsexe"
           else "a.jsexe"
  where
    dropPrefix prefix xs
      | prefix `isPrefixOf` xs = drop (length prefix) xs
      | otherwise              = xs

isJsFile :: FilePath -> Bool
isJsFile = (==".js") . takeExtension

mkGhcjsOutput :: String -> String
mkGhcjsOutput "" = ""
mkGhcjsOutput file
  | ext == ".hi"     = replaceExtension file ".js_hi"
  | ext == ".o"      = replaceExtension file ".js_o"
  | ext == ".dyn_hi" = replaceExtension file ".js_dyn_hi"
  | ext == ".dyn_o"  = replaceExtension file ".js_dyn_o"
  | otherwise        = replaceExtension file (".js_" ++ drop 1 ext)
  where
    ext = takeExtension file
mkGhcjsSuf :: String -> String
mkGhcjsSuf "o"      = "js_o"
mkGhcjsSuf "hi"     = "js_hi"
mkGhcjsSuf "dyn_o"  = "js_dyn_o"
mkGhcjsSuf "dyn_hi" = "js_dyn_hi"
mkGhcjsSuf xs       = "js_" ++ xs -- is this correct?

getEnvMay :: String -> IO (Maybe String)
getEnvMay xs = fmap Just (getEnv xs)
               `Ex.catch` \(_::Ex.SomeException) -> return Nothing

getEnvOpt :: MonadIO m => String -> m Bool
getEnvOpt xs = liftIO (maybe False ((`notElem` ["0","no"]).map toLower) <$> getEnvMay xs)
