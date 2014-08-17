{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{- |
  Shims are non-Haskell dependencies, organized in the
  shims repository. Most of the GHCJS RTS is located there
  too (except the generated parts in "Gen2.Rts" and
  "Gen2.RtsApply").

  example shim yaml:

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

import           DynFlags
import qualified SysTools

import           Control.Applicative hiding ((<|>))
import           Control.Monad

import qualified Data.ByteString as B
import qualified Data.Foldable   as F
import qualified Data.List       as L
import           Data.Maybe (catMaybes)
import           Data.Monoid
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as T
import           Data.Text (Text)
import           Data.Aeson
import qualified Data.Yaml       as Yaml

import           System.FilePath ((<.>), (</>))
import           System.Directory (doesFileExist, canonicalizePath)

import           Text.Parsec
import           Text.Parsec.Text ()

import           Compiler.Settings
import qualified Compiler.Utils  as Utils

type Pkg     = Text
type Version = [Integer]

data Shim = Shim { versionRange   :: VersionRange
                 , dependencies   :: [FilePath]
                 , subs           :: [Shim]
                 } deriving (Eq, Ord, Show)

data VersionRange = SingleVersion Version
                  | Interval (Maybe Version) (Maybe Version)
             deriving (Eq, Ord, Show)

emptyRange :: VersionRange
emptyRange = Interval Nothing Nothing

inRange :: Version -> Shim -> Bool
inRange ver shim = case versionRange shim of
                     Interval lo hi  -> maybe True (<=ver) lo && maybe True (> ver) hi
                     SingleVersion v -> ver == v

emptyShim :: Shim
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

collectShims :: FilePath                                    -- ^ the base path
             -> [(Text, Version)]                           -- ^ packages being linked
             -> IO ([FilePath], [FilePath])                 -- ^ files to load (before, after) rts
collectShims base pkgs = do
  files <- mapM (collectShim base) pkgs
  let files' = map (base </>) (concat files)
      (beforeRts, afterRts) = splitFiles files'
  beforeRts' <- mapM canonicalizePath beforeRts
  afterRts'  <- mapM canonicalizePath afterRts
  return (uniq beforeRts', uniq afterRts')
    where
      splitFiles files = (before, map init after)
        where
          (after, before) = L.partition ("@" `L.isSuffixOf`) files
      uniq xs = let go (x:xs) s
                      | x `S.notMember` s = x : go xs (S.insert x s)
                      | otherwise         = go xs s
                    go [] _ = []
                in go xs mempty

tryReadShimFile :: DynFlags -> FilePath -> IO B.ByteString
tryReadShimFile dflags file = do
  exists <- doesFileExist file
  if not exists
    then putStrLn ("warning: " <> file <> " does not exist") >> return mempty
    else do
      let s = settings dflags
          -- suppress line numbers and enable extended syntax
          cppOpts opts = (Option "-P") : filter (/=(Option "-traditional")) opts
          s1 = s { sPgm_P = (fst (sPgm_P s), cppOpts (snd $ sPgm_P s))
                 , sOpt_P = filter (/="-traditional") (sOpt_P s)
                 }
          dflags1 = dflags { settings = s1 }
      outfile <- SysTools.newTempName dflags "jspp"
      Utils.doCpp dflags1 True False file outfile
      B.readFile outfile

readShim :: FilePath -> (Pkg, Version) -> IO (Maybe Shim)
readShim base (pkgName, pkgVer) = do
  checkShimsInstallation base
  let configFile = base </> T.unpack pkgName <.> "yaml"
  e <- doesFileExist configFile
  if e then do
         cfg <- B.readFile configFile
         case Yaml.decodeEither cfg of
           Left err -> do
             putStrLn ("invalid shim config: " ++ configFile ++ "\n   " ++ err)
             return Nothing
           Right shim -> return shim
       else return Nothing

collectShim :: FilePath -> (Pkg, Version) -> IO [FilePath]
collectShim base (pkgName, pkgVer) = do
  mbShim <- readShim base (pkgName, pkgVer)
  case mbShim of
    Just shim -> return (foldShim pkgName pkgVer shim)
    Nothing   -> return mempty

checkShimsInstallation :: FilePath -> IO ()
checkShimsInstallation base = do
  e <- doesFileExist (base </> "base.yaml")
  when (not e) (error $ "Shims repository not found in `" ++ base ++ "'.")

foldShim :: Pkg -> Version -> Shim -> [FilePath]
foldShim pkg ver sh
  | inRange ver sh = dependencies sh ++ concatMap (foldShim pkg ver) (subs sh)
  | otherwise      = []

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

versionRangeToBuildDep :: VersionRange -> Text
versionRangeToBuildDep (SingleVersion ver) = "== " <> showVersion ver
versionRangeToBuildDep (Interval lo hi) = T.intercalate " && " $ catMaybes
                                                    [((">= " <>) . showVersion) <$> lo,
                                                     (("< "  <>) . showVersion) <$> hi]

showVersion :: Version -> Text
showVersion = T.intercalate "." . map (T.pack . show)
