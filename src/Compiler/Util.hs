-- | Compiler utility functions, mainly dealing with IO and files
module Compiler.Util where

import GHC
import SysTools
import DynFlags

import Control.Monad
import System.Directory  (doesFileExist, copyFile)
import System.FilePath

touchFile :: DynFlags -> FilePath -> IO ()
touchFile df file = do
--  putStrLn ("touchFile: " ++ file)
  e <- doesFileExist file
  when e (touch df "keep build system happy" file)

copyNoOverwrite :: FilePath -> FilePath -> IO ()
copyNoOverwrite from to = do
--  putStrLn ("copyNoOverWrite: " ++ from ++ " -> " ++ to)
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
