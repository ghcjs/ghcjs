{-# LANGUAGE MagicHash, DeriveDataTypeable #-}
#ifdef __GHCJS__
{-# LANGUAGE JavaScriptFFI, GHCForeignImportPrim #-}
#endif

#include "foreign-compat.h"

module GHCJS.Prim ( JSVal(..)
                  , JSException(..)
                  , WouldBlockException(..)
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
getProp o p = js_getPropFromString o (unsafeCoerce $ seqList p)
{-# INLINE getProp #-}

getProp' :: JSVal -> JSVal -> IO JSVal
getProp' o p = js_getProp o p
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

FOREIGN_IMPORT(unsafe, js_fromJSString, JSVal -> Exts.Any, "h$toHsString($1)")
FOREIGN_IMPORT(unsafe, js_toJSString, Exts.Any -> JSVal, "h$fromHsString($1)")
FOREIGN_IMPORT(unsafe, js_fromJSArray, JSVal -> IO Exts.Any, "h$toHsListJSVal($1)")
FOREIGN_IMPORT(unsafe, js_toJSArray, Exts.Any -> IO JSVal, "h$fromHsListJSVal($1)")
FOREIGN_IMPORT(unsafe, js_isNull, JSVal -> Bool, "$1 === null")
FOREIGN_IMPORT(unsafe, js_isUndefined, JSVal -> Bool, "$1 === undefined")
FOREIGN_IMPORT(unsafe, js_fromJSInt, JSVal -> Int, "$r = typeof($1) === 'number' ? ($1|0) : 0;")
FOREIGN_IMPORT(unsafe, js_toJSInt, Int -> JSVal, "$r = $1;")
FOREIGN_IMPORT(unsafe, js_null, JSVal, "$r = null;")
FOREIGN_IMPORT(unsafe, js_getPropFromString, JSVal -> Exts.Any {- String -} -> IO JSVal, "$1[h$fromHsString($2)]")
FOREIGN_IMPORT(unsafe, js_getProp, JSVal -> JSVal -> IO JSVal, "$1[$2]")

{- | If a synchronous thread tries to do something that can only
     be done asynchronously, and the thread is set up to not
     continue asynchronously, it receives this exception.
 -}
data WouldBlockException = WouldBlockException
  deriving (Typeable)

instance Show WouldBlockException where
  show _ = "thread would block"

instance Ex.Exception WouldBlockException

