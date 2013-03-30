{-# LANGUAGE ForeignFunctionInterface, CPP, OverloadedStrings #-}

module Main where

import Foreign.Ptr
import GHCJS.Types
import GHCJS.Foreign
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TE
import qualified Data.ByteString as B
import Data.Monoid

#ifdef __GHCJS__
foreign import javascript unsafe "log($1)"
  js_log :: JSString -> IO ()

foreign import javascript unsafe "$r = ''; for(var i=0;i< $1 ;i++) { $r += 'xyz'; }"
  js_makeSomeStr :: Int -> IO JSString
#else
js_log :: JSString -> IO ()
js_log = error "only available from JavaScript"

js_makeSomeStr :: Int -> IO JSString
js_makeSomeStr = error "only available from JavaScript"
#endif

x :: T.Text
x = "abc"

y :: T.Text
y = "In the programming-language world, one rule of survival is simple: dance or die."

main = do
  js_log . toJSString $ y
  js_log . toJSString . mconcat . replicate 10 $ x
  putStrLn . T.unpack . fromJSString =<< js_makeSomeStr 10
  putStrLn . T.unpack . TE.decodeUtf8With TE.ignore . B.pack $ [63..80] ++ [194,162,226,130,172]
