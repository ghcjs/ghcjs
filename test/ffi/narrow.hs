{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
{-
  Numeric values returned through the JavaScriptFFI are
  automatically "sanitized": Non-numeric values are
  coerced to numbers and the values are truncated to
  fall within the range of the target type.

  This only happens for lifted return types. If you use
  UnliftedFFITypes to return Int# or Word# directly,
  you need to do the check yourself.
 -}
module Main where

import Data.Int
import Data.Word

main :: IO ()
main = do
  -- unsafe
  t   "jsu_int"      jsu_int
  t   "jsu_int32"    jsu_int32
  t   "jsu_int16"    jsu_int16
  t   "jsu_int8"     jsu_int8
  tio "jsu_intIO"    jsu_intIO
  tio "jsu_int32IO"  jsu_int32IO
  tio "jsu_int16IO"  jsu_int16IO
  tio "jsu_int8IO"   jsu_int8IO
  t   "jsu_word"     jsu_word
  t   "jsu_word32"   jsu_word32
  t   "jsu_word16"   jsu_word16
  t   "jsu_word8"    jsu_word8
  tio "jsu_wordIO"   jsu_wordIO
  tio "jsu_word32IO" jsu_word32IO
  tio "jsu_word16IO" jsu_word16IO
  tio "jsu_word8IO"  jsu_word8IO
  -- safe
  t   "jss_int"      jss_int
  t   "jss_int32"    jss_int32
  t   "jss_int16"    jss_int16
  t   "jss_int8"     jss_int8
  tio "jss_intIO"    jss_intIO
  tio "jss_int32IO"  jss_int32IO
  tio "jss_int16IO"  jss_int16IO
  tio "jss_int8IO"   jss_int8IO
  t   "jss_word"     jss_word
  t   "jss_word32"   jss_word32
  t   "jss_word16"   jss_word16
  t   "jss_word8"    jss_word8
  tio "jss_wordIO"   jss_wordIO
  tio "jss_word32IO" jss_word32IO
  tio "jss_word16IO" jss_word16IO
  tio "jss_word8IO"  jss_word8IO
  -- bool
  b pure "jsu_bone"   jsu_bone
  b pure "jss_bone"   jss_bone
  b id   "jsu_boneIO" jsu_boneIO
  b id   "jss_boneIO" jss_boneIO
  b pure "jsu_bzero"   jsu_bzero
  b pure "jss_bzero"   jss_bzero
  b id   "jsu_bzeroIO" jsu_bzeroIO
  b id   "jss_bzeroIO" jss_bzeroIO

-- 
tio :: (Show a, Integral a) => String -> IO a -> IO ()
tio name x = x >>= \y -> putStrLn (name ++ ": " ++ show y ++ " " ++ show (y == fromInteger c))

t :: (Show a, Integral a) => String -> a -> IO ()
t name x = tio name (pure x)

c :: Integer
c = 123456

b :: (a -> IO Bool) -> String -> a -> IO ()
b f name x = f x >>= \y -> putStrLn (name ++ ": " ++ show y ++ " " ++ show (y == False) ++ " " ++ show (y == True))

-- unsafe
foreign import javascript unsafe "123456.789" jsu_int      :: Int
foreign import javascript unsafe "123456.789" jsu_int32    :: Int32
foreign import javascript unsafe "123456.789" jsu_int16    :: Int16
foreign import javascript unsafe "123456.789" jsu_int8     :: Int8

foreign import javascript unsafe "123456.789" jsu_intIO    :: IO Int
foreign import javascript unsafe "123456.789" jsu_int32IO  :: IO Int32
foreign import javascript unsafe "123456.789" jsu_int16IO  :: IO Int16
foreign import javascript unsafe "123456.789" jsu_int8IO   :: IO Int8

foreign import javascript unsafe "123456.789" jsu_word     :: Word
foreign import javascript unsafe "123456.789" jsu_word32   :: Word32
foreign import javascript unsafe "123456.789" jsu_word16   :: Word16
foreign import javascript unsafe "123456.789" jsu_word8    :: Word8

foreign import javascript unsafe "123456.789" jsu_wordIO   :: IO Int
foreign import javascript unsafe "123456.789" jsu_word32IO :: IO Word32
foreign import javascript unsafe "123456.789" jsu_word16IO :: IO Word16
foreign import javascript unsafe "123456.789" jsu_word8IO  :: IO Word8

-- safe
foreign import javascript safe   "123456.789" jss_int      :: Int
foreign import javascript safe   "123456.789" jss_int32    :: Int32
foreign import javascript safe   "123456.789" jss_int16    :: Int16
foreign import javascript safe   "123456.789" jss_int8     :: Int8

foreign import javascript safe   "123456.789" jss_intIO    :: IO Int
foreign import javascript safe   "123456.789" jss_int32IO  :: IO Int32
foreign import javascript safe   "123456.789" jss_int16IO  :: IO Int16
foreign import javascript safe   "123456.789" jss_int8IO   :: IO Int8

foreign import javascript safe   "123456.789" jss_word     :: Word
foreign import javascript safe   "123456.789" jss_word32   :: Word32
foreign import javascript safe   "123456.789" jss_word16   :: Word16
foreign import javascript safe   "123456.789" jss_word8    :: Word8

foreign import javascript safe   "123456.789" jss_wordIO   :: IO Word 
foreign import javascript safe   "123456.789" jss_word32IO :: IO Word32
foreign import javascript safe   "123456.789" jss_word16IO :: IO Word16
foreign import javascript safe   "123456.789" jss_word8IO  :: IO Word8

-- bool
foreign import javascript unsafe "1" jsu_bone    :: Bool
foreign import javascript safe   "1" jss_bone    :: Bool
foreign import javascript unsafe "1" jsu_boneIO  :: IO Bool
foreign import javascript safe   "1" jss_boneIO  :: IO Bool

foreign import javascript unsafe "0" jsu_bzero    :: Bool
foreign import javascript safe   "0" jss_bzero    :: Bool
foreign import javascript unsafe "0" jsu_bzeroIO  :: IO Bool
foreign import javascript safe   "0" jss_bzeroIO  :: IO Bool

