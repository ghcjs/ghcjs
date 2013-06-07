{-# LANGUAGE CPP, TupleSections, ScopedTypeVariables, OverloadedStrings #-}

module Compiler.Cache (emptyCache, writeCachedFiles, installFromCache) where

import           Prelude hiding (FilePath, catch)
import qualified Prelude
import           Compiler.Variants
import           Compiler.Info

import           Control.Exception      (SomeException, catch)
import           Crypto.Conduit         (hashFile)
import qualified Data.Serialize         as C
import           System.Directory       (createDirectoryIfMissing,
                                         getAppUserDataDirectory,
                                         removeDirectoryRecursive)
import           System.Process         (rawSystem, readProcess)
import           Crypto.Skein
import qualified Data.ByteString as B
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as C8
import           Data.Char              (isSpace)
import           Data.Maybe             (catMaybes, fromMaybe)
import           Filesystem.Path        (dropExtension, empty, extension)
import           Filesystem.Path.CurrentOS (encodeString,decodeString)
import           Shelly
import           System.Directory       (getModificationTime, copyFile)
#if MIN_VERSION_directory(1,2,0)
import           Data.Time              (UTCTime)
#endif
import           Control.Monad          (forM_)
import           Data.List              (isPrefixOf)
import qualified Data.Map               as M
import           Data.Monoid            (mappend)
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as L
import           System.Environment     (getArgs)

import           DynFlags

emptyCache :: IO ()
emptyCache = do
  cache <- getGlobalCache
  removeDirectoryRecursive cache `catch` (\(_::SomeException) -> return ())
  createDirectoryIfMissing True cache `catch` (\(_::SomeException) -> return ())

{-
   temporary workaround for lacking Cabal support:
   - write .js source to cache, file name based on the skein hash of
     the corresponding .hi file. ghcjs-cabal picks this up to complete
     the package installation
-}
writeCachedFiles :: DynFlags -> Prelude.FilePath -> [(Variant, ByteString, ByteString)] -> IO ()
writeCachedFiles df jsFile0 variants = do
  let jsFile = decodeString jsFile0
      srcBase = dropExtension jsFile
      hiFile = srcBase <.> "hi" -- srcBase ++ ".hi" --  ++ hiSuf df
  (hash :: Skein_512_512) <- hashFile (toStr hiFile)
  cacheDir <- decodeString <$> getGlobalCache
  let basename = decodeString (C8.unpack . B16.encode . C.encode $ hash)
  createDirectoryIfMissing True (toStr cacheDir)
  copyFile (toStr $ srcBase <.> "js_hi") (toStr $ cacheDir </> basename <.> "js_hi")
  forM_ variants $ \(variant, program, meta) -> do
    B.writeFile (toStr $ cacheDir </> basename <.> removeDot (variantExtension variant)) program
    case variantMetaExtension variant of
      Nothing   -> return ()
      Just mext -> B.writeFile (toStr $ cacheDir </> basename <.> (removeDot mext)) meta

removeDot = L.pack . dropWhile (=='.')

toStr :: FilePath -> Prelude.FilePath
toStr = encodeString

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

-- fixme: should we also handle .hi_dyn ?
installCachedFile :: FilePath -> FilePath -> IO ()
installCachedFile cache hiFile = do
  (hash :: Skein_512_512) <- hashFile (toString hiFile ++ ".hi")
  let hns = hashedNames cache hash
  shelly $ forM_ hns $ \(hne, hn) -> do
    e <- test_f hn
    when e $ cp hn (hiFile `addExt` hne)

hashedNames :: FilePath -> Skein_512_512 -> [(String, FilePath)]
hashedNames cache hash = map (\v -> (v, base `addExt` v)) exts
    where
      exts     = map variantExtension variants ++
                 catMaybes (map variantMetaExtension variants) ++
                 [".js_hi"] -- , ".js_o"]
      base     = cache </> fromString basename
      basename = C8.unpack . B16.encode . C.encode $ hash

collectHiFiles :: IO [FilePath]
collectHiFiles = do
  importDirs <- allImportDirs
  fmap concat $ mapM (fmap collectLonelyHi . allFiles . fromString) importDirs

#if MIN_VERSION_directory(1,2,0)
allFiles :: FilePath -> IO [(FilePath, UTCTime)]
#endif
allFiles fp = do
  files <- shelly $ find fp
  mapM addModificationTime files

#if MIN_VERSION_directory(1,2,0)
addModificationTime :: FilePath -> IO (FilePath, UTCTime)
#endif
addModificationTime file = fmap (file,) $ getModificationTime (toString file)

-- paths of .hi files without corresponding .js files (without extension)
-- .js files older than the .hi file are counted as missing (reinstalls!)
#if MIN_VERSION_directory(1,2,0)
collectLonelyHi :: [(FilePath,UTCTime)] -> [FilePath]
#endif
collectLonelyHi files = map fst $ filter isLonely his
    where
      allMap = M.fromList files
      his    = catMaybes $ map (\(f,m) -> fmap (,m) $ retrieveBase "hi" f) files
      isLonely hi = any (variantMissing hi) variants
      variantMissing (hi, him) v = let ve = variantExtension v
                                   in  fromMaybe True $ do
                                         jsm <- M.lookup (hi `addExt` ve) allMap
                                         return (jsm < him)

addExt :: FilePath -> String -> FilePath
addExt fp e = fp <.> L.pack (tail e)


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
