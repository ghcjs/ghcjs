{-# LANGUAGE MagicHash, DeriveDataTypeable, CPP #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
#endif

module GHCJS.Prim ( JSVal(..)
                  , JSException(..)
                  , WouldBlockException(..)
#ifdef ghcjs_HOST_OS
                  , mkJSException
                  , fromJSString
                  , toJSString
                  , toJSArray
                  , fromJSArray
                  , fromJSInt
                  , toJSInt
                  , isNull
                  , isUndefined
                  , jsNull
                  , getProp
                  , getProp'
#endif
                  ) where

import           Data.Typeable (Typeable)
import           Unsafe.Coerce (unsafeCoerce)

import           GHC.Prim
import qualified GHC.Exception as Ex
import qualified GHC.Exts as Exts
{-
  JSVal is a boxed type that can be used as FFI
  argument or result.
-}
#ifdef ghcjs_HOST_OS
data JSVal = JSVal ByteArray#
#else
data JSVal = JSVal Addr#
#endif

{-
  When a JavaScript exception is raised inside
  a safe or interruptible foreign call, it is converted
  to a JSException
 -}
data JSException = JSException JSVal String
  deriving (Typeable)

instance Ex.Exception JSException

instance Show JSException where
  show (JSException _ xs) = "JavaScript exception: " ++ xs

#ifdef ghcjs_HOST_OS

mkJSException :: JSVal -> IO JSException
mkJSException ref =
  return (JSException (unsafeCoerce ref) (fromJSString ref))

{- | Low-level conversion utilities for packages that cannot
     depend on ghcjs-base
 -}

{- | returns an empty string if the JSVal does not contain
     a string
 -}
fromJSString :: JSVal -> String
fromJSString = unsafeCoerce . js_fromJSString
{-# INLINE fromJSString #-}

toJSString :: String -> JSVal
toJSString = js_toJSString . unsafeCoerce . seqList
{-# INLINE toJSString #-}

fromJSArray :: JSVal -> IO [JSVal]
fromJSArray = unsafeCoerce . js_fromJSArray
{-# INLINE fromJSArray #-}

toJSArray :: [JSVal] -> IO JSVal
toJSArray = js_toJSArray . unsafeCoerce . seqList
{-# INLINE toJSArray #-}

{- | returns zero if the JSVal does not contain a number
 -}
fromJSInt :: JSVal -> Int
fromJSInt = js_fromJSInt
{-# INLINE fromJSInt #-}

toJSInt :: Int -> JSVal
toJSInt = js_toJSInt
{-# INLINE toJSInt #-}

isNull :: JSVal -> Bool
isNull = js_isNull
{-# INLINE isNull #-}

isUndefined :: JSVal -> Bool
isUndefined = js_isUndefined
{-# INLINE isUndefined #-}

jsNull :: JSVal
jsNull = js_null
{-# INLINE jsNull #-}

getProp :: JSVal -> String -> IO JSVal
getProp o p = js_getProp o (unsafeCoerce $ seqList p)
{-# INLINE getProp #-}

getProp' :: JSVal -> JSVal -> IO JSVal
getProp' o p = js_getProp' o p
{-# INLINE getProp' #-}

-- reduce the spine and all list elements to whnf
seqList :: [a] -> [a]
seqList xs = go xs `seq` xs
  where go (x:xs) = x `seq` go xs
        go []     = ()

seqListSpine :: [a] -> [a]
seqListSpine xs = go xs `seq` xs
  where go (x:xs) = go xs
        go []     = ()

foreign import javascript unsafe "h$toHsString($1)"
  js_fromJSString :: JSVal -> Exts.Any

foreign import javascript unsafe "h$fromHsString($1)"
  js_toJSString :: Exts.Any -> JSVal

foreign import javascript unsafe "h$toHsListJSVal($1)"
  js_fromJSArray :: JSVal -> IO Exts.Any

foreign import javascript unsafe "h$fromHsListJSVal($1)"
  js_toJSArray :: Exts.Any -> IO JSVal

foreign import javascript unsafe "$1 === null"
  js_isNull :: JSVal -> Bool

foreign import javascript unsafe "$1 === undefined"
  js_isUndefined :: JSVal -> Bool

foreign import javascript unsafe "$r = typeof($1) === 'number' ? ($1|0) : 0;"
  js_fromJSInt :: JSVal -> Int

foreign import javascript unsafe "$r = $1;"
  js_toJSInt :: Int -> JSVal

foreign import javascript unsafe "$r = null;"
  js_null :: JSVal

foreign import javascript unsafe "$1[h$fromHsString($2)]"
  js_getProp :: JSVal -> Exts.Any -> IO JSVal

foreign import javascript unsafe "$1[$2]"
  js_getProp' :: JSVal -> JSVal -> IO JSVal

#endif

{- | If a synchronous thread tries to do something that can only
     be done asynchronously, and the thread is set up to not
     continue asynchronously, it receives this exception.
 -}
data WouldBlockException = WouldBlockException
  deriving (Typeable)

instance Show WouldBlockException where
  show _ = "thread would block"

instance Ex.Exception WouldBlockException

