{-# LANGUAGE StandaloneDeriving,
             OverloadedStrings,
             LambdaCase,
             TypeSynonymInstances,
             FlexibleInstances,
             TupleSections #-}

{-
  Serialization/deserialization for the binary .jso files

  The JSO files contain dependency information and generated code

  all strings are mapped to a central string table, which helps reduce
  file size and gives us efficient hash consing on read

  Binary intermediate JavaScript object files:
    serialized [Text] -> ([ClosureInfo], JStat) blocks

  file layout:
  - header ["GHCJSOBJ", length of symbol table, length of dependencies, length of index]
  - symbol table
  - dependency info
  - closureinfo index
  - closureinfo data (offsets described by index)
-}

module Gen2.Object ( object
                   , object'
                   , showDeps
                   , readDepsFile
                   , readDeps
                   , showObject
                   , readObjectFile
                   , readObjectFileKeys
                   , readObject
                   , readObjectKeys
                   , serializeStat
                   , emptySymbolTable
                   , SymbolTable
                   , ObjUnit (..)
                   , Deps (..)
                   , Fun (..)
                   , Package (..)
                   ) where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Exception (bracket)
import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans
import qualified Control.Monad.Trans.State.Strict as SS
import           Control.Monad.Trans.Reader

import           Data.Array
import qualified Data.Binary     as DB
import qualified Data.Binary.Get as DB
import qualified Data.Binary.Put as DB
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString as BS
import           Data.Data.Lens
import           Data.Function (on)
import qualified Data.Foldable as F
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Int
import           Data.List (sortBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Binary ()
import qualified Data.Text.Lazy as TL
import           Data.Word

import           Language.Javascript.JMacro
import           System.IO (openBinaryFile, hClose, hSeek, SeekMode(..), IOMode(..) )
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)

import           Gen2.Printer (pretty)
import           Gen2.ClosureInfo hiding (Fun)
import           Gen2.Utils

data Header = Header { symbsLen :: Int64
                     , depsLen  :: Int64
                     , idxLen   :: Int64
                     } deriving (Eq, Ord, Show)

-- | dependencies for a single module
data Deps = Deps { depsPackage :: !Package
                 , depsModule  :: !Text
                 , depsDeps    :: Map Fun (Set Fun)
                 }

data Fun = Fun { funPackage :: Package
               , funModule  :: Text
               , funSymbol  :: Text
               } deriving (Eq, Ord, Show)

data Package = Package { packageName    :: Text
                       , packageVersion :: Text
                       } deriving (Eq, Ord, Show)

type SymbolTable  = HashMap Text Int

emptySymbolTable :: SymbolTable
emptySymbolTable = HM.empty

data SymbolTableR = SymbolTableR { strText   :: Array Int Text
                                 , strString :: Array Int String
                                 }

type PutSM = SS.StateT SymbolTable DB.PutM
type PutS  = PutSM ()
type GetS  = ReaderT SymbolTableR DB.Get

class Objectable a where
  put :: a -> PutS
  get :: GetS a
  putListOf :: [a] -> PutS
  putListOf = putList put
  getListOf :: GetS [a]
  getListOf = getList get


runGetS :: SymbolTableR -> GetS a -> ByteString -> a
runGetS st m bs = DB.runGet (runReaderT m st) bs

runPutS :: SymbolTable -> PutS -> (SymbolTable, ByteString)
runPutS st ps = DB.runPutM (SS.execStateT ps st)

-- one toplevel block in the object file
data ObjUnit = ObjUnit { oiSymbols :: [Text]         -- toplevel symbols (stored in index)
                       , oiClInfo  :: [ClosureInfo]  -- closure information of all closures in block
                       , oiStat    :: JStat          -- the code
                       }

object :: Deps -> [ObjUnit] -> ByteString
object ds units = object' symbs ds xs
  where
    (xs, symbs) = go HM.empty units
    go st0 (ObjUnit sy cl st : ys) =
      let (st1, bs)  = serializeStat st0 cl st
          (bss, st2) = go st1 ys
      in  ((sy,bs):bss, st2)
    go st0 [] = ([], st0)

serializeStat :: SymbolTable -> [ClosureInfo] -> JStat -> (SymbolTable, ByteString)
serializeStat st ci s = runPutS st (put ci >> put s)

object' :: SymbolTable -> Deps -> [([Text],ByteString)] -> ByteString
object' st0 deps0 os = hdr <> symbs <> deps1 <> idx <> mconcat (map snd os)
  where
    hdr          = putHeader (Header (bl symbs) (bl deps1) (bl idx))
    bl           = fromIntegral . B.length
    (st1, deps1) = putDeps  st0 deps0
    (st2, idx)   = putIndex st1 os
    symbs        = putSymbolTable st2

putIndex :: SymbolTable -> [([Text], ByteString)] -> (SymbolTable, ByteString)
putIndex st xs = runPutS st (put $ zip symbols offsets)
  where
    (symbols, values) = unzip xs
    offsets = scanl (+) 0 (map B.length values)

getIndex :: SymbolTableR -> ByteString -> [([Text], Int64)]
getIndex st bs = runGetS st get bs

putDeps :: SymbolTable -> Deps -> (SymbolTable, ByteString)
putDeps st deps = runPutS st (put deps)

getDeps :: SymbolTableR -> ByteString -> Deps
getDeps st bs = runGetS st get bs

instance Objectable Deps where
  put (Deps p m d) = put p >> put m >> put d
  get = Deps <$> get <*> get <*> get

-- | reads only the part necessary to get the dependencies
--   so it's potentially more efficient than readDeps <$> B.readFile file
readDepsFile :: FilePath -> IO Deps
readDepsFile file = bracket (openBinaryFile file ReadMode) hClose $ \h -> do
  mhdr <- getHeader <$> B.hGet h headerLength
  case mhdr of
    Nothing -> error ("readDepsFile: not a valid GHCJS object: " ++ file)
    Just hdr -> do
      bs <- B.hGet h (fromIntegral $ symbsLen hdr + depsLen hdr)
      let symbs = getSymbolTable bs
          deps  = getDeps symbs (B.drop (fromIntegral (symbsLen hdr)) bs)
      return deps

-- | call with contents of the file
readDeps :: String -> ByteString -> Deps
readDeps name bs = case getHeader bs of
                     Nothing -> error ("readDeps: not a valid GHCJS object: " ++ name)
                     Just hdr ->
                       let bsymbs = B.drop (fromIntegral headerLength) bs
                           bdeps  = B.drop (fromIntegral (symbsLen hdr)) bsymbs
                           symbs  = getSymbolTable bsymbs
                       in getDeps symbs bdeps

-- | extract the linkable units from an object file
readObjectFile :: FilePath -> IO [ObjUnit]
readObjectFile = readObjectFileKeys (const True)

readObjectFileKeys :: ([Text] -> Bool) -> FilePath -> IO [ObjUnit]
readObjectFileKeys p file = bracket (openBinaryFile file ReadMode) hClose $ \h -> do
  mhdr <- getHeader <$> B.hGet h headerLength
  case mhdr of
    Nothing -> error ("readObjectFileKeys: not a valid GHCJS object: " ++ file)
    Just hdr -> do
      bss <- B.hGet h (fromIntegral $ symbsLen hdr)
      hSeek h RelativeSeek (fromIntegral $ depsLen hdr)
      bsi <- B.fromStrict <$> BS.hGetContents h
      return $ readObjectKeys' p (getSymbolTable bss) bsi (B.drop (fromIntegral $ idxLen hdr) bsi)

readObject :: String -> ByteString -> [ObjUnit]
readObject name = readObjectKeys name (const True)

readObjectKeys :: String -> ([Text] -> Bool) -> ByteString -> [ObjUnit]
readObjectKeys name p bs =
  case getHeader bs of
    Nothing  -> error ("readObjectKeys: not a valid GHCJS object: " ++ name)
    Just hdr ->
      let bssymbs = B.drop (fromIntegral headerLength) bs
          bsidx   = B.drop (fromIntegral $ symbsLen hdr + depsLen hdr) bssymbs
          bsobjs  = B.drop (fromIntegral $ idxLen hdr) bsidx
      in readObjectKeys' p (getSymbolTable bssymbs) bsidx bsobjs

readObjectKeys' :: ([Text] -> Bool) -> SymbolTableR -> ByteString -> ByteString -> [ObjUnit]
readObjectKeys' p st bsidx bsobjs = catMaybes (map readObj idx)
    where
      idx = getIndex st bsidx
      readObj (x,off)
        | p x       = let (ci, s) = runGetS st ((,) <$> get <*> get) (B.drop off bsobjs)
                      in  Just (ObjUnit x ci s)
        | otherwise = Nothing

getSymbolTable :: ByteString -> SymbolTableR
getSymbolTable bs = SymbolTableR (listArray (0,n-1) xs) (listArray (0,n-1) (map T.unpack xs))
  where
    (n,xs) = DB.runGet getter bs
    getter :: DB.Get (Int, [Text])
    getter = do
      l <- DB.getWord32le
      let l' = fromIntegral l
      (l',) <$> replicateM l' DB.get

putSymbolTable :: SymbolTable -> ByteString
putSymbolTable hm = DB.runPut $ do
  DB.putWord32le (fromIntegral $ length xs)
  mapM_ DB.put xs
    where
      xs :: [Text]
      xs = map fst . sortBy (compare `on` snd) . HM.toList $ hm

headerLength :: Int
headerLength = 32

getHeader :: ByteString -> Maybe Header
getHeader bs | B.length bs < fromIntegral headerLength = Nothing
             | magic /= "GHCJSOBJ"                     = Nothing
             | otherwise                               = Just header
   where
     g = fromIntegral <$> DB.getWord64le
     (magic, header) = DB.runGet ((,) <$> DB.getByteString 8 <*> (Header <$> g <*> g <*> g)) bs

putHeader :: Header -> ByteString
putHeader (Header sl dl il) = DB.runPut $ do
  DB.putByteString "GHCJSOBJ"
  mapM_ (DB.putWord64le . fromIntegral) [sl, dl, il]

-- prettyprint object similar to how the old text based
-- objects worked
showObject :: [ObjUnit] -> TL.Text
showObject xs = mconcat (map showSymbol xs)
  where
    showSymbol (ObjUnit symbs cis stat)
      | "h$debug" `elem` symbs = 
           "/*\n" <> (TL.fromStrict $ T.unlines ( stat ^.. template . _JStr )) <> "\n*/"
      | otherwise = TL.unlines
        [ "// begin: [" <> TL.intercalate "," (map TL.fromStrict symbs) <> "]"
        , displayT . renderPretty 0.8 150 . pretty $ (stat <> mconcat (map toStat cis))
        , "// end: [" <> TL.intercalate "," (map TL.fromStrict symbs) <> "]"
        ]

showDeps :: Deps -> TL.Text
showDeps (Deps p m d) =
  "package: " <> showPkg p <> "\n" <>
  "module: " <> TL.fromStrict m <> "\n" <>
  "deps:\n" <> TL.unlines (map (uncurry dumpDep) (M.toList d))
  where
    dumpDep s ds = TL.fromStrict (funSymbol s) <> " -> \n" <>
      F.foldMap (\(Fun fp fm fs) -> "   "
        <> showPkg fp <> ":" <> TL.fromStrict fm <> "." <> TL.fromStrict fs <> "\n") ds

showPkg :: Package -> TL.Text
showPkg (Package name ver)
  | T.null ver = TL.fromStrict name
  | otherwise  = TL.fromStrict name <> "-" <> TL.fromStrict ver

tag :: Word8 -> PutS
tag x = lift (DB.putWord8 x)

getTag :: GetS Word8
getTag = lift DB.getWord8

instance (Objectable a, Objectable b) => Objectable (a, b) where
  put (x, y) = put x >> put y
  get = (,) <$> get <*> get

instance Objectable a => Objectable [a] where
  put = putListOf
  get = getListOf

instance Objectable Char where
  put = lift . DB.putWord32le . fromIntegral . fromEnum
  get = toEnum . fromIntegral <$> lift DB.getWord32le
  putListOf = put . T.pack
  getListOf = do
    st <- ask
    n <- lift DB.getWord32le
    return (strString st ! fromIntegral n)

putList :: (a -> PutS) -> [a] -> PutS
putList p xs = do
  lift (DB.putWord32le (fromIntegral $ length xs))
  mapM_ p xs

getList :: GetS a -> GetS [a]
getList g = do
  l <- lift DB.getWord32le
  replicateM (fromIntegral l) g

instance (Ord k, Objectable k, Objectable v) => Objectable (Map k v) where
  put = put . M.toList
  get = M.fromList <$> get

instance (Ord a, Objectable a) => Objectable (Set a) where
  put = put . S.toList
  get = S.fromList <$> get

instance Objectable Word64 where
  put = lift . DB.putWord64le
  get = lift DB.getWord64le

instance Objectable Int64 where
  put = lift . DB.putWord64le . fromIntegral
  get = fromIntegral <$> lift DB.getWord64le

instance Objectable Word32 where
  put = lift . DB.putWord32le
  get = lift DB.getWord32le

instance Objectable Int32 where
  put = lift . DB.putWord32le . fromIntegral
  get = fromIntegral <$> lift DB.getWord32le

instance Objectable a => Objectable (Maybe a) where
  put Nothing  = tag 1
  put (Just x) = tag 2 >> put x
  get = getTag >>= \case
                      1 -> pure Nothing
                      2 -> Just <$> get
                      n -> error ("Objectable get Maybe: invalid tag: " ++ show n)

instance Objectable Text where
  put t = do
    symbols <- SS.get
    case HM.lookup t symbols of
      Just i  -> lift (DB.putWord32le $ fromIntegral i)
      Nothing -> do
        let n = HM.size symbols
        SS.put (HM.insert t n symbols)
        lift (DB.putWord32le $ fromIntegral n)
  get = do
    st <- ask
    n <- lift DB.getWord32le
    return (strText st ! fromIntegral n)

instance Objectable JStat where
  put (DeclStat i _)       = tag 1  >> put i
  put (ReturnStat e)       = tag 2  >> put e
  put (IfStat e s1 s2)     = tag 3  >> put e  >> put s1 >> put s2
  put (WhileStat b e s)    = tag 4  >> put b  >> put e  >> put s
  put (ForInStat b i e s)  = tag 5  >> put b  >> put i  >> put e  >> put s
  put (SwitchStat e ss s)  = tag 6  >> put e  >> put ss >> put s
  put (TryStat s1 i s2 s3) = tag 7  >> put s1 >> put i  >> put s2 >> put s3
  put (BlockStat xs)       = tag 8  >> put xs
  put (ApplStat e es)      = tag 9  >> put e  >> put es
  put (PPostStat b s e)    = tag 10 >> put b  >> put s  >> put e
  put (AssignStat e1 e2)   = tag 11 >> put e1 >> put e2
  put (UnsatBlock {})      = error "put JStat: UnsatBlock"
  put (AntiStat xs)        = tag 12 >> put xs
  put (ForeignStat i _)    = tag 13 >> put i
  put (LabelStat l s)      = tag 14 >> put l  >> put s
  put (BreakStat ml)       = tag 15 >> put ml
  put (ContinueStat ml)    = tag 16 >> put ml
  get = getTag >>= \case
                      1  -> DeclStat     <$> get <*> pure Nothing
                      2  -> ReturnStat   <$> get
                      3  -> IfStat       <$> get <*> get <*> get
                      4  -> WhileStat    <$> get <*> get <*> get
                      5  -> ForInStat    <$> get <*> get <*> get <*> get
                      6  -> SwitchStat   <$> get <*> get <*> get
                      7  -> TryStat      <$> get <*> get <*> get <*> get
                      8  -> BlockStat    <$> get
                      9  -> ApplStat     <$> get <*> get
                      10 -> PPostStat    <$> get <*> get <*> get
                      11 -> AssignStat   <$> get <*> get
                      12 -> AntiStat     <$> get
                      13 -> ForeignStat  <$> get <*> pure ([], JTImpossible)
                      14 -> LabelStat    <$> get <*> get
                      15 -> BreakStat    <$> get
                      16 -> ContinueStat <$> get
                      n -> error ("Objectable get JStat: invalid tag: " ++ show n)

instance Objectable JExpr where
  put (ValExpr v)          = tag 1 >> put v
  put (SelExpr e i)        = tag 2 >> put e  >> put i
  put (IdxExpr e1 e2)      = tag 3 >> put e1 >> put e2
  put (InfixExpr xs e1 e2) = tag 4 >> put xs >> put e1 >> put e2
  put (PPostExpr b xs e)   = tag 5 >> put b  >> put xs >> put e
  put (IfExpr e1 e2 e3)    = tag 6 >> put e1 >> put e2 >> put e3
  put (ApplExpr e es)      = tag 7 >> put e  >> put es
  put (UnsatExpr {})       = error "put JExpr: UnsatExpr"
  put (AntiExpr xs)        = tag 8 >> put xs
  put (TypeExpr b e _)     = tag 9 >> put b  >> put e -- don't serialize the type
  get = getTag >>= \case
                      1 -> ValExpr   <$> get
                      2 -> SelExpr   <$> get <*> get
                      3 -> IdxExpr   <$> get <*> get
                      4 -> InfixExpr <$> get <*> get <*> get
                      5 -> PPostExpr <$> get <*> get <*> get
                      6 -> IfExpr    <$> get <*> get <*> get
                      7 -> ApplExpr  <$> get <*> get
                      8 -> AntiExpr  <$> get
                      9 -> TypeExpr  <$> get <*> get <*> pure ([], JTImpossible)
                      n -> error ("Objectable get JExpr: invalid tag: " ++ show n)

instance Objectable JVal where
  put (JVar i)      = tag 1 >> put i
  put (JList es)    = tag 2 >> put es
  put (JDouble d)   = tag 3 >> put d
  put (JInt i)      = tag 4 >> put i
  put (JStr xs)     = tag 5 >> put xs
  put (JRegEx xs)   = tag 6 >> put xs
  put (JHash m)     = tag 7 >> put (M.toList m)
  put (JFunc is s)  = tag 8 >> put is >> put s
  put (UnsatVal {}) = error "put JVal: UnsatVal"
  get = getTag >>= \case
                      1 -> JVar    <$> get
                      2 -> JList   <$> get
                      3 -> JDouble <$> get
                      4 -> JInt    <$> get
                      5 -> JStr    <$> get
                      6 -> JRegEx  <$> get
                      7 -> JHash . M.fromList  <$> get
                      8 -> JFunc   <$> get <*> get
                      n -> error ("Objectable get JVal: invalid tag: " ++ show n)

instance Objectable Ident where
  put (TxtI xs) = put xs
  get = TxtI <$> get

instance Objectable Integer where
  put = lift . DB.put
  get = lift DB.get

-- we need to preserve NaN and infinities, unfortunately the Binary instance for Double does not do this
instance Objectable SaneDouble where
  put (SaneDouble d)
    | isNaN d               = tag 1
    | isInfinite d && d > 0 = tag 2
    | isInfinite d && d < 0 = tag 3
    | isNegativeZero d      = tag 4
    | otherwise             = tag 5 >> lift (DB.put d)
  get = getTag >>= \case
                      1 -> pure $ SaneDouble (0    / 0)
                      2 -> pure $ SaneDouble (1    / 0)
                      3 -> pure $ SaneDouble ((-1) / 0)
                      4 -> pure $ SaneDouble (-0)
                      5 -> SaneDouble <$> lift DB.get
                      n -> error ("Objectable get SaneDouble: invalid tag: " ++ show n)

instance Objectable ClosureInfo where
  put (ClosureInfo v regs name layo typ static) = do
    put v >> put regs >> put name >> put layo >> put typ >> put static
  get = ClosureInfo <$> get <*> get <*> get <*> get <*> get <*> get

instance Objectable VarType where
  put = putEnum
  get = getEnum

-- 16 bit sizes should be enough...
instance Objectable CILayout where
  put CILayoutVariable           = tag 1
  put (CILayoutPtrs size ptrs)   = tag 2 >> putIW16 size >> putList putIW16 ptrs
  put (CILayoutFixed size types) = tag 3 >> putIW16 size >> put types
  get = getTag >>= \case
                      1 -> pure CILayoutVariable
                      2 -> CILayoutPtrs  <$> getIW16 <*> getList getIW16
                      3 -> CILayoutFixed <$> getIW16 <*> get
                      n -> error ("Objectable get CILayout: invalid tag: " ++ show n)

instance Objectable CIStatic where
  put (CIStaticRefs refs) = tag 1 >> put refs
  put CINoStatic          = tag 2
  get = getTag >>= \case
                      1 -> CIStaticRefs <$> get
                      2 -> pure CINoStatic
                      n -> error ("Objectable get CIStatic: invalid tag: " ++ show n)

instance Objectable CIType where
  put (CIFun arity regs) = tag 1 >> putIW16 arity >> putIW16 regs
  put CIThunk            = tag 2
  put (CICon conTag)     = tag 3 >> putIW16 conTag
  put (CIPap size)       = tag 4 >> putIW16 size
  put CIBlackhole        = tag 5
  get = getTag >>= \case
                      1 -> CIFun <$> getIW16 <*> getIW16
                      2 -> pure CIThunk
                      3 -> CICon <$> getIW16
                      4 -> CIPap <$> getIW16
                      5 -> pure CIBlackhole
                      n -> error ("Objectable get CIType: invalid tag: " ++ show n)

-- put an Int as a Word16, little endian. useful for many small values
putIW16 :: Int -> PutS
putIW16 i | i > 65535 || i < 0 = error ("putIW16: out of range: " ++ show i)
          | otherwise          = lift $ DB.putWord16le (fromIntegral i)

getIW16 :: GetS Int
getIW16 = lift (fmap fromIntegral DB.getWord16le)

instance Objectable Fun where
  put (Fun pkg modu symb) = put pkg >> put modu >> put symb
  get = Fun <$> get <*> get <*> get

instance Objectable Package where
  put (Package name version) = put name >> put version
  get = Package <$> get <*> get

putEnum :: Enum a => a -> PutS
putEnum x | n > 65535 = error ("putEnum: out of range: " ++ show n)
          | otherwise = putIW16 n
  where n = fromEnum x

getEnum :: Enum a => GetS a
getEnum = toEnum <$> getIW16

instance Objectable Bool where
  put False = tag 1
  put True  = tag 2
  get = getTag >>= \case
                      1 -> return False
                      2 -> return True
                      n -> error ("Objectable get Bool: invalid tag: " ++ show n)
