{-
  GHCJS linker, manages dependencies with
    modulename.gen2.ji files, which contain function-level dependencies
    the source files (gen2.js) contain function groups delimited by
    special markers
-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Gen2.Linker where

import           Control.Applicative
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Lazy     as BL
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe)
import           Data.Monoid
import           Data.Set                 (Set)
import qualified Data.Set                 as S

import qualified Data.Foldable            as F
import           Data.Serialize
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.IO             as T
import qualified Data.Text.Encoding       as TE
import qualified Data.Text.Encoding.Error as TE
import           System.FilePath (dropExtension, takeFileName, (<.>), (</>))
import           System.Directory (createDirectoryIfMissing)

import           Module                   (ModuleName, moduleNameString)

import           Gen2.StgAst
import           Gen2.Rts (rtsStr)

startMarker :: Text
startMarker = "// GHCJS_BLOCK_START<"

endMarker :: Text
endMarker = "// GHCJS_BLOCK_END<"

link :: String       -- ^ output file/directory
     -> [FilePath]   -- ^ directories to load package modules, end with package name
     -> [FilePath]   -- ^ the object files we're linking
     -> [ModuleName] -- ^ modules to use as roots (include all their functions and deps)
     -> IO [String]  -- ^ arguments for the closure compiler to minify our result
link out searchPath objFiles pageModules = do
  metas <- mapM (readDeps . metaFile) objFiles
  let roots = filter ((`elem` mods) . depsModule) metas
  T.putStrLn ("linking: " <> T.intercalate ", " (map depsModule roots))
  src <- collectDeps (lookup metas) (S.fromList $ concatMap modFuns roots)
  createDirectoryIfMissing False out
  BL.writeFile (out </> "out.js") (BL.fromChunks src)
  writeFile (out </> "rts.js") rtsStr
  return []
  where
    mods = map (T.pack . moduleNameString) pageModules
    pkgLookup       = M.fromList (map (\p -> (T.pack . takeFileName $ p, p)) searchPath)
    pkgLookupNoVer  = M.mapKeys dropVersion pkgLookup
    localLookup metas  =
      M.fromList $ zipWith (\m o -> (depsModule m, dropExtension o)) metas objFiles

    -- | lookup fun in known packages first, then object files specified on command
    --   line, otherwise files in the current dir
    lookup metas ext fun
      | Just path <- M.lookup (funPackage fun) pkgLookup = return (path </> modPath)
      | Just path <- M.lookup (funPackage fun) pkgLookupNoVer = return (path </> modPath)
      | otherwise = return $ maybe ("." </> modPath) (<.> ext)
                             (M.lookup (funModule fun) (localLookup metas))
      where
        modPath = (T.unpack $ T.replace "." "/" (funModule fun)) <.> "gen2" <.> ext

-- get the ji file for a js file
metaFile :: FilePath -> FilePath
metaFile = (<.> "ji") . dropExtension

-- drop the version from a package name
-- fixme this is probably a bit wrong but should only be necessary
-- for wired-in packages base, ghc-prim, integer-gmp, main, rts
dropVersion :: Text -> Text
dropVersion t
  | T.null rdrop = t
  | otherwise    = T.reverse (T.tail rdrop )
  where
    rdrop = T.dropWhile (/='-') (T.reverse t)


-- | dependencies for a single module
data Deps = Deps { depsPackage :: !Text
                 , depsModule  :: !Text
                 , depsDeps    :: Map Text (Set Fun)
                 }

-- | get all functions in a module
modFuns :: Deps -> [Fun]
modFuns (Deps p m d) = map (Fun p m . fst) (M.toList d)

data Fun = Fun { funPackage :: !Text
               , funModule  :: !Text
               , funSymbol  :: !Text
               } deriving (Eq, Ord, Show)

instance Serialize Deps where
  get = Deps <$> getText
             <*> getText
             <*> getMapOf getText (getSetOf get)
  put (Deps p m d) = putText p >> putText m >> putMapOf putText (putSetOf put) d


instance Serialize Fun where
  get             = Fun <$> getText <*> getText <*> getText
  put (Fun p m s) = mapM_ putText [p,m,s]

-- | get all dependencies for a given set of roots
getDeps :: (String -> Fun -> IO FilePath) -> Set Fun -> IO (Set Fun)
getDeps lookup fun = go S.empty M.empty (S.toList fun)
  where
    go :: Set Fun -> Map (Text,Text) Deps -> [Fun] -> IO (Set Fun)
    go result _    []         = return result
    go result deps ffs@(f:fs) =
      let key = (funPackage f, funModule f)
      in  case M.lookup key deps of
            Nothing -> lookup "ji" f >>= readDeps >>=
                           \d -> go result (M.insert key d deps) ffs
            Just (Deps _ _ d)  -> let ds = filter (`S.notMember` result)
                                           (maybe [] S.toList $ M.lookup (funSymbol f) d)
                              in  go (S.insert f result) deps (ds++fs)


-- | get all modules used by the roots and deps
getDepsSources :: (String -> Fun -> IO FilePath) -> Set Fun -> IO [(FilePath, Set Fun)]
getDepsSources lookup funs = do
  allDeps <- getDeps lookup funs
  allPaths <- mapM (\x -> (,S.singleton x) <$> lookup "js" x) (S.toList allDeps)
  return $ M.toList (M.fromListWith S.union allPaths)

-- | collect source snippets
collectDeps :: (String -> Fun -> IO FilePath) -> Set Fun -> IO [ByteString]
collectDeps lookup roots = do
  srcs <- getDepsSources lookup roots
  mapM (uncurry extractDeps) srcs

extractDeps :: FilePath -> Set Fun -> IO ByteString
extractDeps file funs = do
  blocks <- collectBlocks <$> T.readFile file
  let funs' = F.foldMap (S.singleton . funSymbol) funs
      src   = concatMap snd . filter (any (`S.member` funs') . fst) $ blocks
  return (TE.encodeUtf8 $ T.unlines src)

-- | get the delimited blocks from a js file, each
--   block can contain multiple symbols
collectBlocks :: Text -> [([Text],[Text])]
collectBlocks t = go [] [] (T.lines t)
  where
    go _            _          []     = []
    go []           _          (l:ls) =
      case isStartLine l of
        Nothing   -> go []   [] ls
        Just funs -> go funs [] ls
    go currentBlock blockLines (l:ls)
      | isEndLine currentBlock l = (currentBlock, reverse blockLines) : go [] [] ls
      | otherwise                = go currentBlock (l:blockLines) ls

isStartLine :: Text -> Maybe [Text]
isStartLine t | startMarker `T.isPrefixOf` t
  = Just $ T.splitOn "," (T.takeWhile (/='>') $ T.drop (T.length startMarker) t)
              | otherwise = Nothing

isEndLine :: [Text] -> Text -> Bool
isEndLine ts t =
     endMarker `T.isPrefixOf` t
  && ts == T.splitOn "," (T.takeWhile (/='>') $ T.drop (T.length endMarker) t)

getText :: Get Text
getText = do
  l <- getWord32le
  TE.decodeUtf8With TE.lenientDecode <$> getByteString (fromIntegral l)

putText :: Text -> Put
putText t =
  let bs = TE.encodeUtf8 t
  in  putWord32le (fromIntegral $ B.length bs) >> putByteString bs

-- | write module dependencies file
writeDeps :: FilePath -> Deps -> IO ()
writeDeps file = B.writeFile file . runPut . put

-- | read the modulename.gen2.ji file
readDeps :: FilePath -> IO Deps
readDeps file = do
  either error id . runGet get <$> B.readFile file

dumpDeps :: Deps -> String
dumpDeps (Deps p m d) = T.unpack $
  "package: " <> p <> "\n" <>
  "module: " <> m <> "\n" <>
  "deps:\n" <> T.unlines (map (uncurry dumpDep) (M.toList d))
  where
    dumpDep s ds = s <> " -> \n" <>
      F.foldMap (\(Fun fp fm fs) -> "   " <> fp <> ":" <> fm <> "." <> fs <> "\n") ds