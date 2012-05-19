{-# LANGUAGE NoImplicitPrelude, OverloadedStrings, NoMonomorphismRestriction, ExtendedDefaultRules, ScopedTypeVariables #-}
{-
  A cabal-install wrapper for GHCJS
  - invokes cabal-install with the correct arguments to build ghcjs packages
  - uses the GHCJS cache to install .js files for installed packages
     (temporary fix until proper GHCJS support is merged into the Cabal library)
-}

module Main where

import Prelude hiding (FilePath, catch)

import System.Process (rawSystem, readProcess)
import System.Exit (exitWith)

import Shelly
import Crypto.Skein
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Filesystem.Path (extension, dropExtension, empty)
import qualified Data.Set as S
import qualified Data.Text.Lazy as L
import qualified Data.Text as T
import Data.List (isPrefixOf)
import System.Environment (getArgs)
import Data.Monoid (mappend)

import Compiler.Info
import System.Directory (removeDirectoryRecursive, createDirectoryIfMissing)
import Control.Exception (SomeException, catch)
import Crypto.Conduit (hashFile)
import qualified Data.Serialize as C

default (L.Text)
(<>) = mappend

extraArgs = [ "--with-compiler=ghcjs"
            , "--with-hc-pkg=ghcjs-pkg"
            , "--with-hsc2hs=hsc2hs"
            ]

main :: IO ()
main = do
  emptyCache
  args <- getArgs
  ec <- if any (`elem` ["install", "configure"]) args
    then rawSystem "cabal" (extraArgs ++ args)
    else rawSystem "cabal" args
  installFromCache
  exitWith ec

emptyCache :: IO ()
emptyCache = do
  cache <- getGlobalCache
  removeDirectoryRecursive cache `catch` (\(_::SomeException) -> return ())
  createDirectoryIfMissing True cache `catch` (\(_::SomeException) -> return ())

{-
   install from cache:
   1. search package dirs for .hi files without accompanying .js
   2. calculate skein hash for .hi file
   3. copy [skein].js file from the cache
-}
installFromCache :: IO ()
installFromCache = do
  hiFiles <- collectHiFiles
  cache <- fmap fromString getGlobalCache
  mapM_ (installCachedFile cache) hiFiles

installCachedFile :: FilePath -> FilePath -> IO ()
installCachedFile cache hiFile = do
  (hash :: Skein_512_512) <- hashFile (toString hiFile ++ ".hi")
  let hn = hashedName cache hash
  shelly $ do
    e <- test_f hn
    when e $ cp hn (hiFile <.> "js")

hashedName :: FilePath -> Skein_512_512 -> FilePath
hashedName cache hash = cache </> fromString basename <.> "js"
    where
      basename = C8.unpack . B16.encode . C.encode $ hash

collectHiFiles :: IO [FilePath]
collectHiFiles = do
  importDirs <- allImportDirs
  fmap concat $ mapM (fmap collectLonelyHi . allFiles . fromString) importDirs

allFiles :: FilePath -> IO [FilePath]
allFiles = shelly . find

-- paths of .hi files without corresponding .js files (without extension)
collectLonelyHi :: [FilePath] -> [FilePath]
collectLonelyHi files = S.toList (hiSet `S.difference` jsSet)
    where
      jsSet = S.fromList $ catMaybes $ map (retrieveBase "js") files
      hiSet = S.fromList $ catMaybes $ map (retrieveBase "hi") files


retrieveBase :: T.Text -> FilePath -> Maybe FilePath
retrieveBase ext path
    | extension path == Just ext = Just (dropExtension path)
    | otherwise                  = Nothing
    where

-- read the import dirs of all installed ghcjs packages
-- fixme: this is not a proper parser, fails for multiline import-dirs, but seems to work on cabal output
allImportDirs :: IO [String]
allImportDirs = do
  out <- readProcess "ghcjs-pkg" ["dump"] ""
  return $ filter (not.null) $ concatMap getImportDir $ lines out
      where
        prefix = "import-dirs:" :: String
        getImportDir line
          | prefix `isPrefixOf` line = [trim $ drop (length prefix) line]
          | otherwise                        = []



trim :: String -> String
trim = let f = reverse . dropWhile isSpace in f . f

fromString :: String -> FilePath
fromString = fromText . L.pack

toString :: FilePath -> String
toString = L.unpack . toTextIgnore
