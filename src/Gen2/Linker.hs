{-
  GHCJS linker, manages dependencies with
    modulename.gen2.ji files, which contain function-level dependencies
    the source files contain function groups delimited by special markers
-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Gen2.Linker where

import           Control.Applicative
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe               (fromMaybe)
import           Data.Set                 (Set)
import qualified Data.Set                 as S

import qualified Data.Foldable            as F
import           Data.Serialize
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO             as T

startMarker :: Text
startMarker = "// GHCJS_BLOCK_START<"

endMarker :: Text
endMarker = "// GHCJS_BLOCK_END<"

-- | dependencies for a single module
data Deps = Deps { depsDeps :: Map Text (Set Fun)
--                 , depsGroups :: Map Text (Set Text)
                 }
{-
data Module = Module { modPackage :: !Text
                     , modName    :: !Text
                     } deriving (Eq, Ord, Show)
-}

data Fun = Fun { funPackage :: !Text
               , funModule  :: !Text
               , funSymbol  :: !Text
               } deriving (Eq, Ord, Show)

instance Serialize Deps where
  get   = Deps <$> getMapOf getText (getSetOf get)
--               <*> getMapOf getText (getSetOf getText)
  put d = putMapOf putText (putSetOf put) (depsDeps d) -- >> putGroups (depsGroups d)

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
            Just (Deps d)  -> let ds = filter (`S.notMember` result)
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
  return (B.concat $ map T.encodeUtf8 src)

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
  T.decodeUtf8With T.lenientDecode <$> getByteString (fromIntegral l)

putText :: Text -> Put
putText t =
  let bs = T.encodeUtf8 t
  in  putWord32le (fromIntegral $ B.length bs) >> putByteString bs

-- | write module dependencies file
writeDeps :: FilePath -> Deps -> IO ()
writeDeps file = B.writeFile file . runPut . put

-- | read the modulename.gen2.ji file
readDeps :: FilePath -> IO Deps
readDeps file = either error id . runGet get <$> B.readFile file

