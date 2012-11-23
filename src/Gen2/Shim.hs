{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-
  Shims are non-haskell dependencies, organized in the
  ghcjs-shims repository

  example shim yaml:
-}

{-
version:
js:
 - src/edefw.js
 - src/b.js
 - sub:
    - version: [ 0.5.3 , 0.8 )
      js:
        - js1
        - js2
    - version: 0.8.2 ..
      js:
        - js1
        - js2

-}

module Gen2.Shim where

import Prelude hiding (catch)
import Control.Exception (SomeException, catch)
import Control.Applicative hiding ((<|>))
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as B
import Data.Text (Text)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text
import qualified Data.Foldable as F
import Data.Set (Set)
import qualified Data.Set as S
import System.FilePath ((<.>), (</>))
import System.Directory (doesFileExist, canonicalizePath)
import qualified Data.List as L
import Data.Monoid

type Pkg     = Text
type Version = [Integer]

data Shim = Shim { versionRange   :: VersionRange
                 , dependencies   :: [FilePath]
                 , subs           :: [Shim]
                 } deriving (Eq, Ord, Show)

data VersionRange = SingleVersion Version     -- v == v0
                  | Interval (Maybe Version) (Maybe Version)  -- v in [v0,v1)
             deriving (Eq, Ord, Show)

emptyRange = Interval Nothing Nothing

inRange :: Version -> Shim -> Bool
inRange ver shim = case versionRange shim of
                     Interval lo hi  -> maybe True (<=ver) lo && maybe True (> ver) hi
                     SingleVersion v -> ver == v

emptyShim = Shim emptyRange [] []

instance FromJSON Shim where
  parseJSON (Object v) =
    Shim <$> v .:? "version" .!= (Interval Nothing Nothing)
         <*> v .:? "js"      .!= []
         <*> v .:? "subs"    .!= []
  parseJSON (Array xs) = Shim emptyRange [] <$> (mapM parseJSON $ F.toList xs)
  parseJSON _ = mempty

instance FromJSON VersionRange where
  parseJSON (String t) = maybe mempty pure (parseVersionRange t)
  parseJSON _          = mempty

collectShims :: FilePath          -- ^ the base path
             -> [(Text, Version)] -- ^ packages being linked
             -> IO Text           -- ^ collected shims
collectShims base pkgs = do
  putStrLn "collecting shims for:"
  print pkgs
  files <- S.unions <$> mapM (collectShim base) pkgs
  files' <- mapM (canonicalizePath . (base </>)) (S.toList files)
  T.unlines <$> mapM tryReadFile (map head . L.group . L.sort $ files')

tryReadFile :: FilePath -> IO Text
tryReadFile file = T.readFile file `catch` \(_::SomeException) -> do
                     putStrLn ("warning: could not read file: " <> file)
                     return mempty

collectShim :: FilePath -> (Pkg, Version) -> IO (Set FilePath)
collectShim base (pkgName, pkgVer) = do
  let configFile = base </> T.unpack pkgName <.> "yaml"
  putStrLn ("config file: " ++ configFile)
  e <- doesFileExist configFile
  if e then do
         putStrLn "it exists!"
         cfg <- B.readFile  configFile
         case Yaml.decodeEither cfg of
           Left err -> do
             putStrLn ("invalid shim config: " ++ configFile ++ "\n   " ++ err)
             return mempty
           Right shim -> return (foldShim pkgName pkgVer shim)
       else return mempty

foldShim :: Pkg -> Version -> Shim -> Set FilePath
foldShim pkg ver sh
  | inRange ver sh = S.unions $ S.fromList (dependencies sh) : 
                       map (foldShim pkg ver) (subs sh)
  | otherwise = S.empty

parseVersion :: Text -> Maybe Version
parseVersion tv = either (const Nothing) Just $ parse version "" (T.strip tv)

version :: Stream s m Char => ParsecT s u m Version
version = integer `sepBy1` char '.'

integer :: Stream s m Char => ParsecT s u m Integer
integer = read <$> many1 digit

parseVersionRange :: Text -> Maybe VersionRange
parseVersionRange tv = either (const Nothing) Just $
                        parse versionBounds "" (T.strip tv)
  where
    versionBounds = versionRange <|>
                    openRange1 <|>
                    try versionWildcard <|>
                    try openRange2 <|>
                    singleVersion

    -- 1.3.2
    singleVersion = SingleVersion <$> version

    -- 1.3.*
    versionWildcard = wildcardRange <$> (version <* string ".*")

    -- [ 1.1 , 1.4 )
    versionRange = do
      char '[' >> spaces
      start <- version
      spaces >> char ',' >> spaces
      end <- version
      spaces >> char ')'
      return (Interval (Just start) (Just end))

    -- .. 1.4
    openRange1 = Interval Nothing . Just <$>
                   (string ".." *> spaces *> version)
    -- 1.2 ..
    openRange2 = (\x -> Interval (Just x) Nothing) <$>
                    (version <* spaces <* string "..")

-- 1.2.* -> (Interval (Just [1,2]) (Just [1,3])) 
wildcardRange :: Version -> VersionRange
wildcardRange xs
  | (y:ys) <- reverse xs = Interval (Just xs) (Just . reverse $ (y+1):ys)
  | otherwise = SingleVersion []


