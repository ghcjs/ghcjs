{-# LANGUAGE ViewPatterns #-}

module Main where

import Gen2.Archive
import qualified Data.ByteString.Lazy as B
import System.FilePath
import qualified Data.Text as T
import System.Environment
import Control.Monad

import Module

main = do
  args <- getArgs
  case args of
   [archive] -> do
     meta <- readMeta archive
     putStrLn ("meta:\n" ++ show meta)
     index <- readIndex archive
     putStrLn "index:"
     mapM_ print index
     forM_ index $ extractIndexEntry archive
   _ -> putStrLn "usage: dumpArchive archiveFile"

extractIndexEntry :: FilePath -> IndexEntry -> IO ()
extractIndexEntry archive (ieEntry -> Object mod) = do
  let ms = T.unpack mod
      outFile = dropExtension archive ++ "_obj_" ++ ms <.> "js_o"
  putStrLn $ "extracing module object: " ++ ms ++ " ->\n  " ++ outFile
  bs <- readObject (mkModuleName ms) archive
  B.writeFile outFile bs
extractIndexEntry archive (ieEntry -> JsSource file) = do
  let outFile = dropExtension archive ++ "_src_" ++ map replaceChr file
      replaceChr c | c `elem` "\\/" = '_'
                   | otherwise      = c
  putStrLn $ "extracting js source: " ++ file ++ " ->\n  " ++ outFile
  bs <- readSource file archive
  B.writeFile outFile bs
