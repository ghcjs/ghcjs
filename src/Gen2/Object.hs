{-# LANGUAGE StandaloneDeriving, OverloadedStrings, LambdaCase #-}

{-
  Binary intermediate JavaScript object files:
    serialized [Text] -> ([ClosureInfo], JStat) blocks

-}

module Gen2.Object ( object
                   , object'
                   , readObject
                   , readObjectKeys
                   , showObject
                   , serializeStat
                   , ObjUnit (..)
                   ) where

import           Control.Applicative ((<$>), (<*>), liftA2, pure)
import           Control.Lens
import           Control.Monad

import           Data.Data.Lens
import qualified Data.Map as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Language.Javascript.JMacro
import           Language.Javascript.JMacro.Types
import           Text.PrettyPrint.Leijen.Text (displayT, renderPretty)
import qualified Data.ByteString.Lazy as B
import           Data.ByteString.Lazy (ByteString)
import           Data.Int
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Maybe (catMaybes)
import           Data.Text.Binary ()

import           Gen2.Printer (pretty)
import           Gen2.ClosureInfo
import           Gen2.Utils

-- one toplevel block in the object file
data ObjUnit = ObjUnit { oiSymbols :: [Text]         -- toplevel symbols (stored in index)
                       , oiClInfo  :: [ClosureInfo]  -- closure information of all closures in block
                       , oiStat    :: JStat          -- the code
                       }

object :: [ObjUnit] -> ByteString
object = object' . map (\(ObjUnit sy cl st) -> (sy, serializeStat cl st))

serializeStat :: [ClosureInfo] -> JStat -> ByteString
serializeStat ci s = runPut (putList putCI ci >> putJStat s)

object' :: [([Text],ByteString)] -> ByteString
object' os = hdr <> idx <> mconcat (map snd os)
  where
    hdr   = runPut (putWord64le (fromIntegral $ B.length idx))
    idx   = makeIndex os

makeIndex :: [([Text], ByteString)] -> ByteString
makeIndex xs = runPut (put $ zip symbols offsets)
  where
    (symbols, values) = unzip xs
    offsets = scanl (+) 0 (map B.length values)

readIndex :: ByteString -> [([Text], Int64)]
readIndex bs = runGet get bs

readObject :: ByteString -> [ObjUnit]
readObject = readObjectKeys (const True)

readObjectKeys :: ([Text] -> Bool) -> ByteString -> [ObjUnit]
readObjectKeys p bs = catMaybes (map readObj idx)
    where
      offset = idxLen + 8
      idxLen = fromIntegral (runGet getWord64le bs)
      idx    = readIndex (B.drop 8 bs)
      readObj (x,off)
        | p x       = let (ci, s) = runGet ((,) <$> getList getCI <*> getS) (B.drop (off+offset) bs)
                      in  Just (ObjUnit x ci s)
        | otherwise = Nothing

-- prettyprint object similar to how the old text based
-- objects worked
showObject :: ByteString -> TL.Text
showObject bs = mconcat (map showSymbol $ readObject bs)
  where
    showSymbol (ObjUnit symbs cis stat)
      | "h$debug" `elem` symbs = "/*\n" <> TL.pack (unlines ( stat ^.. template . _JStr )) <> "\n*/"
      | otherwise = TL.unlines
        [ "// begin: [" <> TL.intercalate "," (map TL.fromStrict symbs) <> "]"
        , displayT . renderPretty 0.8 150 . pretty $ (stat <> mconcat (map toStat cis))
        , "// end: [" <> TL.intercalate "," (map TL.fromStrict symbs) <> "]"
        ]


tag :: Word8 -> Put
tag = putWord8

getTag :: Get Word8
getTag = getWord8

putList :: (a -> Put) -> [a] -> Put
putList f xs = putWord32le (fromIntegral $ length xs) >>
               mapM_ f xs

getList :: Get a -> Get [a]
getList g = do
  l <- getWord32le
  replicateM (fromIntegral l) g

putMaybe :: (a -> Put) -> Maybe a -> Put
putMaybe _ Nothing  = tag 1
putMaybe f (Just x) = tag 2 >> f x

getMaybe :: (Get a) -> Get (Maybe a)
getMaybe g = getTag >>= \case
                           1 -> pure Nothing
                           2 -> Just <$> g

putString :: String -> Put
putString = put . T.pack

getString :: Get String
getString = T.unpack <$> get

putJStat :: JStat -> Put
putJStat = putS

instance Binary ClosureInfo where
  put = putCI
  get = getCI

-- not automatically derived because we cannot serialize the generators
putS :: JStat -> Put
putS (DeclStat i _)       = tag 1 >> putI i
putS (ReturnStat e)       = tag 2 >> putE e
putS (IfStat e s1 s2)     = tag 3 >> putE e >> putS s1 >> putS s2
putS (WhileStat b e s)    = tag 4 >> put b >> putE e >> putS s
putS (ForInStat b i e s)  = tag 5 >> put b >> putI i >> putE e >> putS s
putS (SwitchStat e ss s) =
  tag 6 >> putE e >> putList (\(x,y) -> putE x >> putS y) ss >> putS s
putS (TryStat s1 i s2 s3) = tag 7 >> putS s1 >> putI i >> putS s2 >> putS s3
putS (BlockStat xs)       = tag 8 >> putList putS xs
putS (ApplStat e es)      = tag 9 >> putE e >> putList putE es
putS (PPostStat b s e)    = tag 10 >> put b >> putString s >> putE e
putS (AssignStat e1 e2)   = tag 11 >> putE e1 >> putE e2
putS (UnsatBlock {})      = error "putS: UnsatBlock"
putS (AntiStat xs)        = tag 12 >> putString xs
putS (ForeignStat i t)    = tag 13 >> putI i >> putLT t
putS (LabelStat l s)      = tag 14 >> putString l >> putS s
putS (BreakStat ml)       = tag 15 >> putMaybe putString ml
putS (ContinueStat ml)    = tag 16 >> putMaybe putString ml

getS :: Get JStat
getS = getTag >>= \case
                     1  -> DeclStat     <$> getI <*> pure Nothing
                     2  -> ReturnStat   <$> getE
                     3  -> IfStat       <$> getE <*> getS <*> getS
                     4  -> WhileStat    <$> get  <*> getE <*> getS
                     5  -> ForInStat    <$> get  <*> getI <*> getE <*> getS
                     6  -> SwitchStat   <$> getE <*> getList (liftA2 (,) getE getS) <*> getS
                     7  -> TryStat      <$> getS <*> getI <*> getS <*> getS
                     8  -> BlockStat    <$> getList getS
                     9  -> ApplStat     <$> getE <*> getList getE
                     10 -> PPostStat    <$> get  <*> getString <*> getE
                     11 -> AssignStat   <$> getE <*> getE
                     12 -> AntiStat     <$> getString
                     13 -> ForeignStat  <$> getI <*> getLT
                     14 -> LabelStat    <$> getString <*> getS
                     15 -> BreakStat    <$> getMaybe getString
                     16 -> ContinueStat <$> getMaybe getString
putE :: JExpr -> Put
putE (ValExpr v)          = tag 1  >> putV v
putE (SelExpr e i)        = tag 2  >> putE e >> putI i
putE (IdxExpr e1 e2)      = tag 3  >> putE e1 >> putE e2
putE (InfixExpr xs e1 e2) = tag 4  >> putString xs >> putE e1 >> putE e2
putE (PPostExpr b xs e)   = tag 5  >> put b >> putString xs >> putE e
putE (IfExpr e1 e2 e3)    = tag 6  >> putE e1 >> putE e2 >> putE e3
putE (NewExpr e)          = tag 7  >> putE e
putE (ApplExpr e es)      = tag 8  >> putE e >> putList putE es
putE (UnsatExpr {})       = error "putE: UnsatExpr"
putE (AntiExpr xs)        = tag 9  >> putString xs
putE (TypeExpr b e t)     = tag 10 >> put b >> putE e >> putLT t

getE :: Get JExpr
getE = getTag >>= \case
                     1  -> ValExpr <$> getV
                     2  -> SelExpr <$> getE <*> getI
                     3  -> IdxExpr <$> getE <*> getE
                     4  -> InfixExpr <$> getString <*> getE <*> getE
                     5  -> PPostExpr <$> get <*> getString <*> getE
                     6  -> IfExpr <$> getE <*> getE <*> getE
                     7  -> NewExpr <$> getE
                     8  -> ApplExpr <$> getE <*> getList getE
                     9  -> AntiExpr <$> getString
                     10 -> TypeExpr <$> get <*> getE <*> getLT

putV :: JVal -> Put
putV (JVar i)      = tag 1 >> putI i
putV (JList es)    = tag 2 >> putList putE es
putV (JDouble d)   = tag 3 >> putSD d
putV (JInt i)      = tag 4 >> put i
putV (JStr xs)     = tag 5 >> putString xs
putV (JRegEx xs)   = tag 6 >> putString xs
putV (JHash m)     = tag 7 >> putList (\(k,v) -> putString k >> putE v) (M.toList m)
putV (JFunc is s)  = tag 8 >> putList putI is >> putS s
putV (UnsatVal {}) = error "putE: UnsatVal"

getV :: Get JVal
getV = getTag >>= \case
                     1 -> JVar    <$> getI
                     2 -> JList   <$> getList getE
                     3 -> JDouble <$> getSD
                     4 -> JInt    <$> get
                     5 -> JStr    <$> getString
                     6 -> JRegEx  <$> getString
                     7 -> JHash   <$> (M.fromList <$> getList ((,) <$> getString <*> getE))
                     8 -> JFunc   <$> getList getI <*> getS

-- todo: perhaps switch to a separate symbol table to save space?
putI :: Ident -> Put
putI (StrI xs) = putString xs

getI :: Get Ident
getI = StrI <$> getString

-- we need to preserve NaN and infinities, unfortunately the Binary instance for Double does not do this
putSD :: SaneDouble -> Put
putSD (SaneDouble d)
  | isNaN d               = tag 1
  | isInfinite d && d > 0 = tag 2
  | isInfinite d && d < 0 = tag 3
  | isNegativeZero d      = tag 4
  | otherwise             = tag 5 >> put d

getSD :: Get SaneDouble
getSD = getTag >>= \case 
                      1 -> pure $ SaneDouble (0    / 0)
                      2 -> pure $ SaneDouble (1    / 0)
                      3 -> pure $ SaneDouble ((-1) / 0)
                      4 -> pure $ SaneDouble (-0)
                      5 -> SaneDouble <$> get

-- we do not need these atm
putLT :: JLocalType -> Put
putLT _ = return ()

getLT :: Get JLocalType
getLT = return ([], JTImpossible)

putCI :: ClosureInfo -> Put
putCI (ClosureInfo v regs name layout typ static) = put v >> putList putVT regs >> put name >> putCIL layout >> putCIT typ >> putCIS static

getCI :: Get ClosureInfo
getCI = ClosureInfo <$> get <*> getList getVT <*> get <*> getCIL <*> getCIT <*> getCIS

putVT :: VarType -> Put
putVT = putWord8 . fromIntegral . fromEnum

getVT :: Get VarType
getVT = fmap (toEnum . fromIntegral) getWord8

-- 16 bit sizes should be enough...
putCIL :: CILayout -> Put
putCIL CILayoutVariable           = tag 1
putCIL (CILayoutPtrs size ptrs)   = tag 2 >> putIW16 size >> putList putIW16 ptrs
putCIL (CILayoutFixed size types) = tag 3 >> putIW16 size >> putList putVT types

getCIL :: Get CILayout
getCIL = getTag >>= \case
                       1 -> pure CILayoutVariable
                       2 -> CILayoutPtrs  <$> getIW16 <*> getList getIW16
                       3 -> CILayoutFixed <$> getIW16 <*> getList getVT

putCIS :: CIStatic -> Put
putCIS (CIStaticRefs refs) = tag 1 >> putList put refs
putCIS CINoStatic          = tag 2

getCIS :: Get CIStatic
getCIS = getTag >>= \case
                       1 -> CIStaticRefs <$> getList get
                       2 -> pure CINoStatic

putCIT :: CIType -> Put
putCIT (CIFun arity regs) = tag 1 >> putIW16 arity >> putIW16 regs
putCIT CIThunk            = tag 2
putCIT (CICon conTag)     = tag 3 >> putIW16 conTag
putCIT (CIPap size)       = tag 4 >> putIW16 size
putCIT CIBlackhole        = tag 5

getCIT :: Get CIType
getCIT = getTag >>= \case
                       1 -> CIFun <$> getIW16 <*> getIW16
                       2 -> pure CIThunk
                       3 -> CICon <$> getIW16
                       4 -> CIPap <$> getIW16
                       5 -> pure CIBlackhole

-- put an Int as a Word16, little endian. useful for many small values
putIW16 :: Int -> Put
putIW16 = putWord16le . fromIntegral

getIW16 :: Get Int
getIW16 = fmap fromIntegral getWord16le
