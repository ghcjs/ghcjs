{-# LANGUAGE MagicHash, DeriveDataTypeable, CPP #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI #-}
#endif

module GHCJS.Prim ( JSRef(..)
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

{-
  JSRef is a boxed type that can be used as FFI
  argument or result.
-}
#ifdef ghcjs_HOST_OS
data JSRef a = JSRef ByteArray#
#else
data JSRef a = JSRef Addr#
#endif

{-
  When a JavaScript exception is raised inside
  a safe or interruptible foreign call, it is converted
  to a JSException
 -}
data JSException = JSException (JSRef ()) String
  deriving (Typeable)

instance Ex.Exception JSException

instance Show JSException where
  show (JSException _ xs) = "JavaScript exception: " ++ xs

#ifdef ghcjs_HOST_OS

mkJSException :: JSRef a -> IO JSException
mkJSException ref =
  return (JSException (unsafeCoerce ref) (fromJSString ref))

{- | Low-level conversion utilities for packages that cannot
     depend on ghcjs-base
 -}

{- | returns an empty string if the JSRef does not contain
     a string
 -}
fromJSString :: JSRef a -> String
fromJSString = unsafeCoerce . js_fromJSString
{-# INLINE fromJSString #-}

toJSString :: String -> JSRef a
toJSString = js_toJSString . unsafeCoerce . seqList
{-# INLINE toJSString #-}

fromJSArray :: JSRef a -> IO [JSRef a]
fromJSArray = unsafeCoerce . js_fromJSArray
{-# INLINE fromJSArray #-}

toJSArray :: [JSRef a] -> IO (JSRef b)
toJSArray = js_toJSArray . unsafeCoerce . seqList
{-# INLINE toJSArray #-}

{- | returns zero if the JSRef does not contain a number
 -}
fromJSInt :: JSRef a -> Int
fromJSInt = js_fromJSInt
{-# INLINE fromJSInt #-}

toJSInt :: Int -> JSRef a
toJSInt = js_toJSInt
{-# INLINE toJSInt #-}

isNull :: JSRef a -> Bool
isNull = js_isNull
{-# INLINE isNull #-}

isUndefined :: JSRef a -> Bool
isUndefined = js_isUndefined
{-# INLINE isUndefined #-}

jsNull :: JSRef a
jsNull = js_null
{-# INLINE jsNull #-}

getProp :: JSRef a -> String -> IO (JSRef b)
getProp o p = js_getProp o (unsafeCoerce $ seqList p)
{-# INLINE getProp #-}

getProp' :: JSRef a -> JSRef b -> IO (JSRef c)
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
  js_fromJSString :: JSRef a -> Double

foreign import javascript unsafe "h$fromHsString($1)"
  js_toJSString :: Double -> JSRef a

foreign import javascript unsafe "h$toHsListJSRef($1)"
  js_fromJSArray :: JSRef a -> IO Double

foreign import javascript unsafe "h$fromHsListJSRef($1)"
  js_toJSArray :: Double -> IO (JSRef a)

foreign import javascript unsafe "$1 === null"
  js_isNull :: JSRef a -> Bool

foreign import javascript unsafe "$1 === undefined"
  js_isUndefined :: JSRef a -> Bool

foreign import javascript unsafe "$r = typeof($1) === 'number' ? ($1|0) : 0;"
  js_fromJSInt :: JSRef a -> Int

foreign import javascript unsafe "$r = $1;"
  js_toJSInt :: Int -> JSRef a

foreign import javascript unsafe "$r = null;"
  js_null :: JSRef a

foreign import javascript unsafe "$1[h$fromHsString($2)]"
  js_getProp :: JSRef a -> Double -> IO (JSRef b)

foreign import javascript unsafe "$1[$2]"
  js_getProp' :: JSRef a -> JSRef b -> IO (JSRef c)

#endif

{- | If a synchronous thread tries to do something that can only
     be done asynchronously, and the thread is set up to not
     continue asynchronously, it receives this exception.
 -}
data WouldBlockException = WouldBlockException String
  deriving (Show, Typeable)

instance Ex.Exception WouldBlockException

