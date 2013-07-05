{-
  GHCJS linker, manages dependencies with
    modulename.ji files, which contain function-level dependencies
    the source files (.js) contain function groups delimited by
    special markers
-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Gen2.Linker where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as S
import qualified Data.Vector              as V

import Data.Char (isDigit)
import qualified Data.Foldable            as F
import Data.List (partition, isSuffixOf, isPrefixOf)
import           Data.Serialize
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import           System.FilePath (dropExtension, splitPath, (<.>), (</>))
import           System.Directory (createDirectoryIfMissing, doesFileExist)
import           Config
import           Module                   (ModuleName, moduleNameString)
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)
import           Language.Javascript.JMacro

import           Gen2.StgAst
import           Gen2.Rts (rtsStr)
import           Gen2.Shim
import           Gen2.Printer (pretty)
import qualified Gen2.Compactor as Compactor
import           Gen2.ClosureInfo hiding (Fun)
import           Compiler.Info
import qualified Gen2.Object as Object

link :: String       -- ^ output file/directory
     -> [FilePath]   -- ^ directories to load package modules, end with package name
     -> [FilePath]   -- ^ the object files we're linking
     -> [ModuleName] -- ^ modules to use as roots (include all their functions and deps)
     -> IO [String]  -- ^ arguments for the closure compiler to minify our result
link out searchPath objFiles pageModules = do
  let (objFiles', extraFiles) = partition (".js" `isSuffixOf`) objFiles
  metas <- mapM (readDeps . metaFile) objFiles'
  let roots = filter ((`elem` mods) . depsModule) metas
  T.putStrLn ("linking " <> T.pack out <> ": " <> T.intercalate ", " (map depsModule roots))
  (allDeps, src, infos) <- collectDeps (lookup metas) (S.union rtsDeps (S.fromList $ concatMap modFuns roots))
  createDirectoryIfMissing False out
  BL.writeFile (out </> "out.js") (renderLinker src infos) -- (BL.fromChunks src)
  writeFile (out </> "rts.js") rtsStr
  getShims extraFiles allDeps (out </> "lib.js", out </> "lib1.js")
  writeHtml out
  combineFiles out
  return []
  where
    mods = map (T.pack . moduleNameString) pageModules
    pkgLookup       = M.fromList (map (\p -> (pkgFromPath p, p)) searchPath)
    -- pkg name-version is last path element, except when there's a ghc-version path element after
    pkgFromPath path | (a:b:_) <- reverse (splitPath' path) =
      if (("ghc-"++cProjectVersion) `isPrefixOf` a) then T.pack b else T.pack a
    pkgFromPath _ = mempty
    pkgLookupNoVer  = M.mapKeys dropVersion pkgLookup
    localLookup metas  =
      M.fromList $ zipWith (\m o -> (depsModule m, dropExtension o)) metas objFiles

    -- | lookup fun in known packages first, then object files specified on command
    --   line, otherwise files in the current dir
    lookup metas ext fun
      | Just path <- M.lookup (funPkgTxt      fun) pkgLookup = return (path </> modPath)
      | Just path <- M.lookup (funPkgTxtNoVer fun) pkgLookupNoVer = return (path </> modPath)
      | otherwise = return $ maybe ("." </> modPath) (<.> ext)
                             (M.lookup (funModule fun) (localLookup metas))
      where
        modPath = (T.unpack $ T.replace "." "/" (funModule fun)) <.> ext

renderLinker :: JStat -> [ClosureInfo] -> BL.ByteString
renderLinker stat infos = TLE.encodeUtf8 . displayT . renderPretty 0.8 150 . pretty $
                        Compactor.compact stat infos

splitPath' :: FilePath -> [FilePath]
splitPath' = map (filter (`notElem` "/\\")) . splitPath

getShims :: [FilePath] -> Set Fun -> (FilePath, FilePath) -> IO ()
getShims extraFiles deps (fileBefore, fileAfter) = do
  base <- (</> "shims") <$> getGlobalPackageBase
  ((before, beforeFiles), (after, afterFiles)) <- collectShims base pkgDeps
  T.writeFile fileBefore before
  writeFile (fileBefore <.> "files") (unlines beforeFiles)
  t' <- mapM T.readFile extraFiles
  T.writeFile fileAfter (T.unlines $ after : t')
  writeFile (fileAfter <.> "files") (unlines $ afterFiles ++ extraFiles)
    where
      pkgDeps = map (\(Package n v) -> (n, fromMaybe [] $ parseVersion v))
                  (S.toList $ S.map funPackage deps)

-- convenience: combine lib.js, rts.js, lib1.js, out.js to all.js that can be run
-- directly with node or spidermonkey
combineFiles :: FilePath -> IO ()
combineFiles fp = do
  files <- mapM (T.readFile.(fp</>)) ["lib.js", "rts.js", "lib1.js", "out.js"]
  T.writeFile (fp</>"all.js") (mconcat (files ++ [runMain]))

runMain :: Text
runMain = "\nh$main(h$mainZCMainzimain);\n"

basicHtml :: Text
basicHtml = "<!DOCTYPE html>\n\
            \<html>\n\
            \  <head>\n\
            \    <script language=\"javascript\" src=\"lib.js\"></script>\n\
            \    <script language=\"javascript\" src=\"rts.js\"></script>\n\
            \    <script language=\"javascript\" src=\"lib1.js\"></script>\n\
            \    <script language=\"javascript\" src=\"out.js\"></script>\n\
            \  </head>\n\
            \  <body>\n\
            \  </body>\n\
            \  <script language=\"javascript\">\n\
            \    " <> runMain <> "\n\
            \  </script>\n\
            \</html>\n"

writeHtml :: FilePath -> IO ()
writeHtml out = do
  e <- doesFileExist htmlFile
  when (not e) (T.writeFile htmlFile basicHtml)
  where
    htmlFile = out </> "index" <.> "html"

-- get the ji file for a js file
metaFile :: FilePath -> FilePath
metaFile = (<.> "ji") . dropExtension

-- drop the version from a package name
-- fixme this is probably a bit wrong but should only be necessary
-- for wired-in packages base, ghc-prim, integer-gmp, main, rts
dropVersion :: Text -> Text
dropVersion = fst . splitVersion

splitVersion :: Text -> (Text, Text)
splitVersion t
  | T.null ver || T.any (`notElem` "1234567890.") ver
      = (t, mempty)
  | T.null name = (mempty, mempty)
  | otherwise   = (T.reverse (T.tail name), T.reverse ver)
  where
    (ver, name) = T.break (=='-') (T.reverse t)


-- | dependencies for a single module
data Deps = Deps { depsPackage :: !Package
                 , depsModule  :: !Text
                 , depsDeps    :: Map Fun (Set Fun)
                 }

-- | get all functions in a module
modFuns :: Deps -> [Fun]
modFuns (Deps p m d) = map fst (M.toList d)

data Package = Package { packageName :: !Text
                       , packageVersion :: !Text
                       } deriving (Eq, Ord, Show)

instance Serialize Package where
  get = Package <$> getText <*> getText
  put (Package n v) = mapM_ putText [n,v]

data Fun = Fun { funPackage :: !Package
               , funModule  :: !Text
               , funSymbol  :: !Text
               } deriving (Eq, Ord, Show)

data IndexedFun = IndexedFun { ifunPackage :: !Int
                             , ifunModule  :: !Text
                             , ifunSymbol  :: !Text
                             }

instance Serialize IndexedFun where
  get             = IndexedFun <$> get <*> getText <*> getText
  put (IndexedFun p m s) = put p >> mapM_ putText [m,s]

toIndexedFun :: Map Package Int -> Fun -> IndexedFun
toIndexedFun pkgs (Fun p m s) = (IndexedFun idx m s)
  where
    idx = fromMaybe (error $ "missing package in deps set: " ++ show p) (M.lookup p pkgs)

fromIndexedFun :: V.Vector Package -> IndexedFun -> Fun
fromIndexedFun pkgs (IndexedFun pi m s) = (Fun (pkgs V.! pi) m s)

-- set of packages and set of funs must contain at least everything in Deps
serializeDeps :: Set Package -> Set Fun -> Putter Deps --  -> Putter ByteString
serializeDeps pkgs funs (Deps p m d) = do
  put p
  putText m
  put (S.toAscList pkgs)
  let pkgs' = M.fromList $ zip (S.toAscList pkgs) [(0::Int)..]
      funs' = M.fromList $ zip (S.toAscList funs) [(0::Int)..]
  put $ map (toIndexedFun pkgs') (S.toAscList funs)
  let depIndex d = fromMaybe (error $ "missing symbol in deps set: " ++ show d) (M.lookup d funs')
  put $ map (\(d,ds) -> (depIndex d, map depIndex $ S.toList ds)) (M.toAscList d)

-- reads back a deps file, uses sharing for efficiency so be careful with the result
unserializeDeps :: Get Deps
unserializeDeps = do
  p    <- get
  m    <- getText
  pkgs <- V.fromList <$> get
  funs <- V.fromList . map (fromIndexedFun pkgs) <$> get
  deps <- M.fromList . map (\(d,ds) -> (funs V.! d, S.fromList $ map (funs V.!) ds)) <$> get
  return (Deps p m deps)

-- | get all dependencies for a given set of roots
getDeps :: (String -> Fun -> IO FilePath) -> Set Fun -> IO (Set Fun)
getDeps lookup fun = go S.empty M.empty (S.toList fun)
  where
    go :: Set Fun -> Map (Package,Text) Deps -> [Fun] -> IO (Set Fun)
    go result _    []         = return result
    go result deps ffs@(f:fs) =
      let key = (funPackage f, funModule f)
      in  case M.lookup key deps of
            Nothing -> lookup "ji" f >>= readDeps >>=
                           \d -> go result (M.insert key d deps) ffs
            Just (Deps _ _ d)  -> let ds = filter (`S.notMember` result)
                                           (maybe [] S.toList $ M.lookup f d)
                              in  go (S.insert f result) deps (ds++fs)

-- | get all modules used by the roots and deps
getDepsSources :: (String -> Fun -> IO FilePath) -> Set Fun -> IO (Set Fun, [(FilePath, Set Fun)])
getDepsSources lookup funs = do
  allDeps <- getDeps lookup funs
  allPaths <- mapM (\x -> (,S.singleton x) <$> lookup "js" x) (S.toList allDeps)
  return $ (allDeps, M.toList (M.fromListWith S.union allPaths))

-- | collect source snippets
collectDeps :: (String -> Fun -> IO FilePath) -> Set Fun -> IO (Set Fun, JStat, [ClosureInfo])
collectDeps lookup roots = do
  (allDeps, srcs) <- getDepsSources lookup roots
  (stats, infos) <- unzip <$> mapM (uncurry extractDeps) srcs
  return (allDeps, mconcat stats, concat infos)

extractDeps :: FilePath -> Set Fun -> IO (JStat, [ClosureInfo])
extractDeps file funs = do
  let symbs = S.fromList $ map funSymbol (S.toList funs)
  l <- Object.readObjectKeys (any (`S.member` symbs)) <$> BL.readFile file
  return (mconcat (map Object.oiStat l), concatMap Object.oiClInfo l)

getText :: Get Text
getText = do
  l <- getWord32le
  TE.decodeUtf8With TE.lenientDecode <$> getByteString (fromIntegral l)

putText :: Text -> Put
putText t =
  let bs = TE.encodeUtf8 t
  in  putWord32le (fromIntegral $ B.length bs) >> putByteString bs

-- | read the modulename.ji file
readDeps :: FilePath -> IO Deps
readDeps file = do
  either error id . runGet unserializeDeps <$> B.readFile file

dumpDeps :: Deps -> String
dumpDeps (Deps p m d) = T.unpack $
  "package: " <> showPkg p <> "\n" <>
  "module: " <> m <> "\n" <>
  "deps:\n" <> T.unlines (map (uncurry dumpDep) (M.toList d))
  where
    dumpDep s ds = funSymbol s <> " -> \n" <>
      F.foldMap (\(Fun fp fm fs) -> "   "
        <> showPkg fp <> ":" <> fm <> "." <> fs <> "\n") ds

showPkg :: Package -> Text
showPkg (Package name ver)
  | T.null ver = name
  | otherwise  = name <> "-" <> ver

-- Fun -> packagename-packagever
funPkgTxt :: Fun -> Text
funPkgTxt = showPkg . funPackage

-- Fun -> packagename
funPkgTxtNoVer :: Fun -> Text
funPkgTxtNoVer = packageName . funPackage

-- dependencies for the RTS, these need to be always linked
rtsDeps :: Set Fun
rtsDeps =
 let mkDep (p,m,s) = Fun (Package p "") m s
 in S.fromList $ map mkDep
     [ ("base",     "GHC.Conc.Sync", "h$baseZCGHCziConcziSynczireportError")
     , ("base",     "Control.Exception.Base", "h$baseZCControlziExceptionziBasezinonTermination" )
     ]

