{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, TupleSections, GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Gen2.Archive ( Entry(..), Index, IndexEntry(..), Meta(..)
                    , buildArchive
                    , readMeta, readIndex
                    , readSource, readAllData, readAllSources
                    , readObject, withObject, withAllObjects
                    ) where

import           Control.Monad
import           Data.Semigroup (Semigroup)
import           Control.Applicative ((<|>))

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Char8 as C
import           Data.Data
import           Data.Int
import           Data.Text (Text)
import qualified Data.Text as T

import           GHC.Generics hiding (Meta)

import           System.IO

import           Module

import           Gen2.Object ( versionTag, versionTagLength, readDeps, depsModule )

-- entry, offset in data section, length
type Index = [IndexEntry]

data IndexEntry = IndexEntry { ieEntry  :: Entry
                             , ieOffset :: Int64
                             , ieLength :: Int64
                             } deriving (Show, Typeable, Generic)

instance Binary IndexEntry

data Entry = Object    Text -- module name
           | JsSource  FilePath
           deriving (Show, Typeable, Generic)

instance Binary Entry

data Meta = Meta { metaCppOptions :: [String]
                 } deriving (Show, Typeable, Generic)

instance Binary Meta

-- sizes of the sections in bytes
data Sections = Sections { sectionIndex :: !Word64
                         , sectionMeta  :: !Word64
                         , sectionData  :: !Word64
                         } deriving (Eq, Ord, Generic)

instance Binary Sections where
  put (Sections i m d) = putWord64le i >> putWord64le m >> putWord64le d
  get = Sections <$> getWord64le <*> getWord64le <*> getWord64le

sectionsLength :: Int
sectionsLength = 24

buildArchive :: Meta -> [(Entry, ByteString)] -> ByteString
buildArchive meta entries =
  versionTag <> sections <> index <> meta' <> entries'
  where
    bl       = fromIntegral . B.length
    sections = runPut . put $ Sections (bl index) (bl meta') (bl entries')
    meta'    = runPut (put meta)
    index    = runPut . put $ scanl1 (\(IndexEntry _ o l) (IndexEntry e _ l') -> (IndexEntry e (o+l) l')) $
                              map (\(e,b) -> IndexEntry e 0 (B.length b)) entries
    entries' = mconcat (map snd entries)

readMeta :: FilePath -> IO Meta
readMeta _ = return $ Meta []
-- withFile file ReadMode $ \h -> do
--   sections <- hReadHeader ("readMeta " ++ file) h
--   hSeek h RelativeSeek (toInteger $ sectionIndex sections)
--   m <- B.hGet h (fromIntegral $ sectionMeta sections)
--   return $! runGet get m

readIndex :: FilePath -> IO Index
readIndex _ = undefined

readSource :: FilePath -> FilePath -> IO ByteString
readSource source file = undefined
-- withArchive "readSource" file $
--   withEntry ("readSource " ++ file)
--             ("source file " ++ source)
--             selectSrc
--             (\h l -> B.hGet h $ fromIntegral l)
--   where
--     selectSrc (JsSource src) = src == source
--     selectSrc _              = False


readAllData :: FilePath -> IO [ByteString]
readAllData file = return [] -- withAllObjects file $ \_ h l -> B.hGet h (fromIntegral l)

readAllSources :: FilePath -> IO [(FilePath, ByteString)]
readAllSources file = undefined
-- withArchive "readAllSources" file $ \archive h ->
--   forM [ (o, l, src) | IndexEntry (JsSource src) o l <- index ] $ \(o, l, src) -> do
--     hSeek h AbsoluteSeek (fromIntegral $ dataSectionStart sections + fromIntegral o)
--     (src,) <$> B.hGet h (fromIntegral l)

readObject :: ModuleName -> FilePath -> IO ByteString
readObject m file = do
  putStrLn ("Archive::readObject: " ++ file)
  withArchive "readObject" file $
    withModuleObject ("readObject " ++ file) m (\h l -> B.hGet h $ fromIntegral l)

-- | seeks to the starting position of the object in the file
withObject :: ModuleName -> FilePath -> (Handle -> Int64 -> IO a) -> IO a
withObject m file f = withArchive "withObject" file $
  withModuleObject ("withObject " ++ file) m f

newtype Archive = Archive [ArchiveEntry]
  deriving (Eq, Show, Semigroup, Monoid)

data ArchiveEntry = ArchiveEntry
    { filename :: String       -- ^ File name.
    , filemodule :: String     -- ^ File module name.
    , filetime :: Int          -- ^ File modification time.
    , fileown  :: Int          -- ^ File owner.
    , filegrp  :: Int          -- ^ File group.
    , filemode :: Int          -- ^ File mode.
    , fileoffset :: Int        -- ^ File offset.
    , filesize :: Int          -- ^ File size.
    } deriving (Eq, Show)

getArchMagic :: Get ()
getArchMagic = do
  magic <- liftM C.unpack $ getByteString 8
  if magic /= "!<arch>\n"
    then fail $ "Invalid magic number " ++ show magic
    else return ()

getBSDArchEntries :: Get [ArchiveEntry]
getBSDArchEntries = do
  empty <- isEmpty
  if empty then
      return []
   else do
      name    <- getByteString 16
      when ('/' `C.elem` name && C.take 3 name /= "#1/") $
        fail "Looks like GNU Archive"
      time    <- getPaddedInt <$> getByteString 12
      own     <- getPaddedInt <$> getByteString 6
      grp     <- getPaddedInt <$> getByteString 6
      mode    <- getPaddedInt <$> getByteString 8
      st_size <- getPaddedInt <$> getByteString 10
      end     <- getByteString 2
      when (end /= "\x60\x0a") $
        fail ("[BSD Archive] Invalid archive header end marker for name: " ++
              C.unpack name)
      off1    <- liftM fromIntegral bytesRead :: Get Int
      -- BSD stores extended filenames, by writing #1/<length> into the
      -- name field, the first @length@ bytes then represent the file name
      -- thus the payload size is filesize + file name length.
      name    <- if C.unpack (C.take 3 name) == "#1/" then
                      liftM (C.unpack . C.takeWhile (/= '\0')) (getByteString $ read $ C.unpack $ C.drop 3 name)
                  else
                      return $ C.unpack $ C.takeWhile (/= ' ') name
      off2    <- liftM fromIntegral bytesRead :: Get Int
      file <- getByteString (st_size - (off2 - off1)) -- file size
      -- data sections are two byte aligned (see Trac #15396)
      when (odd st_size) $
        void (getByteString 1)

      rest    <- getBSDArchEntries
      let mod = T.unpack . depsModule $ readDeps name (B.fromStrict file)
      return $ (ArchiveEntry name mod time own grp mode off2 (st_size - (off2 - off1))) : rest

getGNUArchEntries :: Maybe ArchiveEntry -> Get [ArchiveEntry]
getGNUArchEntries extInfo = do
  empty <- isEmpty
  if empty
    then return []
    else
    do
      name    <- getByteString 16
      time    <- getPaddedInt <$> getByteString 12
      own     <- getPaddedInt <$> getByteString 6
      grp     <- getPaddedInt <$> getByteString 6
      mode    <- getPaddedInt <$> getByteString 8
      st_size <- getPaddedInt <$> getByteString 10
      end     <- getByteString 2
      when (end /= "\x60\x0a") $
        fail ("[BSD Archive] Invalid archive header end marker for name: " ++
              C.unpack name)
      off <- liftM fromIntegral bytesRead :: Get Int
      file <- getByteString st_size
      -- data sections are two byte aligned (see Trac #15396)
      when (odd st_size) $
        void (getByteString 1)
      name <- return . C.unpack $
        if C.unpack (C.take 1 name) == "/"
        then case C.takeWhile (/= ' ') name of
                name@"/"  -> name               -- symbol table
                name@"//" -> name               -- extendedn file names table
                name      -> getExtName extInfo (read . C.unpack $ C.drop 1 name) file
        else C.takeWhile (/= '/') name
      let mod = T.unpack . depsModule $ readDeps name (B.fromStrict file)
      case name of
        "/"  -> getGNUArchEntries extInfo
        "//" -> getGNUArchEntries (Just (ArchiveEntry name mod time own grp mode off st_size))
        _    -> (ArchiveEntry name mod time own grp mode off st_size :) <$> getGNUArchEntries extInfo
  where
    getExtName :: Maybe ArchiveEntry -> Int -> C.ByteString -> C.ByteString
    getExtName Nothing _ _ = error "Invalid extended filename reference."
    getExtName (Just info) offset filedata = C.takeWhile (/= '/') . C.drop offset $ filedata


-- | Archives have numeric values padded with '\x20' to the right.
getPaddedInt :: C.ByteString -> Int
getPaddedInt = read . C.unpack . C.takeWhile (/= '\x20')


getArch :: Get Archive
getArch = Archive <$> do
  getArchMagic
  getBSDArchEntries <|> getGNUArchEntries Nothing

-- withAllObjects :: FilePath -> (ModuleName -> Handle -> Int64 -> IO a) -> IO [a]
-- withAllObjects file f = do
--   putStrLn ("Archive::withAllObjects: " ++ file)
--   withArchive "withAllObjects" file $ \sections index h ->
--     forM [ (o, l, mn) | IndexEntry (Object mn) o l <- index ] $ \(o, l, mn) -> do
--       hSeek h AbsoluteSeek (fromIntegral $ dataSectionStart sections + fromIntegral o)
--       f (mkModuleName (T.unpack mn)) h l

withAllObjects :: FilePath -- ^ ar Archive (BSD Format)
               -> (ModuleName -- ^ Name of the module. Something like archive.a(object.o)
                -> Handle -- ^ handle with proper offset
                -> Int64 -- ^ length
                -> IO a) -- ^ callback to apply to each element in the archive
               -> IO [a]
withAllObjects file f = do
  putStrLn $ "Archive::withAllObjects:" ++ file
  withArchive "withAllObjects" file $ \(Archive archive) h ->
    forM [ (o, l, file ++ "(" ++ fn ++ ")")
         | ArchiveEntry { filename = fn, fileoffset = o, filesize = l } <- archive ] $ \(o, l, mn) -> do
          hSeek h AbsoluteSeek (fromIntegral o)
          f (mkModuleName mn) h (fromIntegral l)


---------------------------------------------------------------------------------

-- withArchive :: String -> FilePath -> (Sections -> Index -> Handle -> IO a) -> IO a
-- withArchive name file f = withFile file ReadMode $ \h -> do
--   let name' = name ++ " " ++ file
--   sections <- hReadHeader name' h
--   index <- hReadIndex name' sections h
--   f sections index h

withArchive :: String    -- name
            -> FilePath  --
            -> (Archive -> Handle -> IO a)
            -> IO a
withArchive name file f = do
  arch <- runGet getArch <$> B.readFile file
  withFile file ReadMode $ \h -> do
    f arch h

-- | seeks to start of entry data in file, then runs the action
--   exactly one matching entry is expected
withEntry :: String -> String
          -> (ArchiveEntry -> Bool) -> (Handle -> Int64 -> IO a)
          -> Archive -> Handle
          -> IO a
withEntry name entryName p f (Archive archive) h =
  case filter p archive of
    [] -> error (name ++ ": cannot find " ++ entryName)
    [ArchiveEntry { fileoffset = o, filesize = l }] -> do
      hSeek h AbsoluteSeek (fromIntegral o)
      f h (fromIntegral l)
    _ -> error (name ++ ": multiple matches for " ++ entryName)

withModuleObject :: String -> ModuleName -> (Handle -> Int64 -> IO a)
                 -> Archive -> Handle
                 -> IO a
withModuleObject name m f =
  withEntry name ("object for module " ++ ms) selectEntry f
  where
    ms = moduleNameString m
    selectEntry (ArchiveEntry { filemodule = m' }) = ms == m'
    selectEntry _                                = False

-- | expects Handle to be positioned at the start of the header
--   Handle is positioned at start of index after return
hReadHeader :: String -> Handle -> IO Sections
hReadHeader name h = do
  ts <- B.hGet h (versionTagLength + sectionsLength)
  when (B.take (fromIntegral versionTagLength) ts /= versionTag)
       (error $ name ++ ": version tag mismatch")
  return $! runGet get (B.drop (fromIntegral versionTagLength) ts)

-- | expects Handle to be positioned at the start of the index
--   Handle is positioned at start of metadata section after return
hReadIndex :: String -> Sections -> Handle -> IO Index
hReadIndex name s h = do
  i <- B.hGet h (fromIntegral $ sectionIndex s)
  return $! runGet get i

-- start of data section in file
dataSectionStart :: Sections -> Integer
dataSectionStart s = toInteger (versionTagLength + sectionsLength)
                   + toInteger (sectionIndex s + sectionMeta s)
