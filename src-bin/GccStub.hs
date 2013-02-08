{-# LANGUAGE ScopedTypeVariables #-}
{-
  GCC stub program that can be called by GHCJS
  just creates the output files in case someone checks
-}
module Main where

import Control.Applicative
import qualified Control.Exception as Ex
import Control.Monad
import Data.Char (toLower)
import System.Directory
import System.Environment
import System.Exit
import System.FilePath

main = do
  args <- getArgs
  forM_ (findOutput args) generateOutput
  exitSuccess
  
findOutput :: [String] -> [String]
findOutput ("-o":o:xs) = o : findOutput xs
findOutput (_:xs)      = findOutput xs
findOutput []          = []

generateOutput :: FilePath -> IO ()
generateOutput file
  | ".js_o" == takeExtension file = do
    let oFile = replaceExtension file ".o"
    e <- doesFileExist oFile
    if e then copyFile oFile file
         else dummyOutput file
    fakeNative <- getEnvOpt "GHCJS_FAKE_NATIVE"
    when fakeNative (doFakeNative file)
  | otherwise = dummyOutput file

dummyOutput :: FilePath -> IO ()
dummyOutput file = writeFile file "GHCJS output file"

-- when booting we don't generate native code, but we
-- still need to touch the output to keep the build system
-- from rebuilding multiple times
doFakeNative :: FilePath -> IO ()
doFakeNative file = do
  let oFile = replaceExtension file ".o"
      hiFile = replaceExtension file ".hi"
      bhiFile = replaceExtension file ".backup_hi"
  copyNoOverwrite bhiFile hiFile
  touchFile hiFile
  touchFile oFile

touchFile :: FilePath -> IO ()
touchFile file = do
  e <- doesFileExist file
  when e (appendFile file "")

copyNoOverwrite :: FilePath -> FilePath -> IO ()
copyNoOverwrite from to = do
  ef <- doesFileExist from
  et <- doesFileExist to
  when (ef && not et) (copyFile from to)

-- fixme move to library
getEnvOpt :: String -> IO Bool
getEnvOpt xs = maybe False ((`notElem` ["0","no"]).map toLower) <$> getEnvMay xs

getEnvMay :: String -> IO (Maybe String)
getEnvMay xs = fmap Just (getEnv xs)
               `Ex.catch` \(_::Ex.SomeException) -> return Nothing
