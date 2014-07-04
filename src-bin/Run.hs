{-
   ghcjs-run runs a program compiled by ghcjs with node.js
 -}
module Main where

import Control.Applicative

import Data.Char

import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.Process

main = do
  args <- getArgs
  path <- getExecutablePath
  cd   <- getCurrentDirectory
  let jsExe  = dropExtension path <.> "jsexe"
      script = jsExe </> "all" <.> "js"
  node <- trim <$> readFile (jsExe </> "node")
  ph <- runProcess node (script:args) (Just cd) Nothing Nothing Nothing Nothing
  exitWith =<< waitForProcess ph

trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

