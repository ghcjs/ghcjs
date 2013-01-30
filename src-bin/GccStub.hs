{-
  GCC stub program that can be called by ghcjs
  just creates the output files in case someone checks
-}
module Main where

import Control.Monad
import System.Environment
import System.Exit

main = do
  args <- getArgs
  forM_ (findOutput args) (\file -> writeFile file "GHCJS output file")
  exitSuccess
  
findOutput :: [String] -> [String]
findOutput ("-o":o:xs) = o : findOutput xs
findOutput (_:xs)      = findOutput xs
findOutput []          = []

