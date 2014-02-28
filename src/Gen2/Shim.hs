{-# LANGUAGE OverloadedStrings, TupleSections #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}
{-
  Shims are non-Haskell dependencies, organized in the
  shims repository

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
import Control.Monad
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

import Compiler.Settings
import DynFlags
import Packages (getPackageIncludePath)
import Config (cProjectVersionInt)
import qualified SysTools

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

collectShims :: DynFlags
             -> GhcjsSettings
             -> FilePath                                    -- ^ the base path
             -> [(Text, Version)]                           -- ^ packages being linked
             -> IO ((Text, [FilePath]), (Text, [FilePath])) -- ^ collected shims, to be included (before, after) rts
collectShims dflags settings base pkgs = do
  files <- mapM (collectShim base) pkgs
  let files' = map (base </>) (concat files)
      (beforeRts, afterRts) = splitFiles files'
  beforeRts' <- mapM canonicalizePath beforeRts
  afterRts' <- mapM canonicalizePath afterRts
  (,) <$> combineShims beforeRts' <*> combineShims afterRts'
    where
      splitFiles files = (before, map init after)
        where
          (after, before) = L.partition ("@" `L.isSuffixOf`) files
      combineShims files = ((,files).T.unlines) <$> mapM (tryReadShimFile dflags settings) (uniq files)
      uniq xs = let go (x:xs) s
                      | x `S.notMember` s = x : go xs (S.insert x s)
                      | otherwise         = go xs s
                    go [] _ = []
                in go xs mempty

tryReadShimFile :: DynFlags -> GhcjsSettings -> FilePath -> IO Text
tryReadShimFile dflags ghcjsSettings file = do
  exists <- doesFileExist file
  if not exists
    then putStrLn ("warning: " <> file <> " does not exist") >> return mempty
    else do
      let hscpp_opts = picPOpts dflags
      let s = settings dflags
          s1 = s { sPgm_P = (fst (sPgm_P s), filter ((/=) (Option "-traditional")) (snd $ sPgm_P s)) }
      let dflags1 = dflags { settings = s1 }
      outfile <- SysTools.newTempName dflags "jspp"
      let cmdline_include_paths = includePaths dflags
      pkg_include_dirs <- getPackageIncludePath dflags []
      let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                            (cmdline_include_paths ++ pkg_include_dirs)
      let verbFlags = getVerbFlags dflags
      let hsSourceCppOpts = [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
      SysTools.runCpp dflags1 (
                       map SysTools.Option verbFlags
                    ++ map SysTools.Option include_paths
                    ++ map SysTools.Option hscpp_opts
                    ++ [ SysTools.Option    "-P" ] -- suppress line number info
                    -- see compiler/main/DriverPipeline.hs for hacks here
                    ++ [ SysTools.Option     "-x"
                       , SysTools.Option     "assembler-with-cpp"
                       , SysTools.Option     file
                       , SysTools.Option     "-o"
                       , SysTools.FileOption "" outfile
                       ])
      T.readFile outfile

collectShim :: FilePath -> (Pkg, Version) -> IO [FilePath]
collectShim base (pkgName, pkgVer) = do
  checkShimsInstallation base
  let configFile = base </> T.unpack pkgName <.> "yaml"
  e <- doesFileExist configFile
  if e then do
         cfg <- B.readFile configFile
         case Yaml.decodeEither cfg of
           Left err -> do
             putStrLn ("invalid shim config: " ++ configFile ++ "\n   " ++ err)
             return mempty
           Right shim -> return (foldShim pkgName pkgVer shim)
       else return mempty

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


