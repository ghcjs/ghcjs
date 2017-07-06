{-# LANGUAGE OverloadedStrings, JavaScriptFFI #-}

module Main where

import GHCJS.Types
import GHCJS.Foreign
import GHCJS.Marshal
import qualified Data.JSString as JSS

jx, jy, jz :: JSString
jx = "x"
jy = "xy"
jz = "xyz"

hx, hy, hz :: String
hx = "X"
hy = "XY"
hz = "XYZ"

foreign import javascript unsafe "h$log($1);" clog  :: JSString -> IO ()
foreign import javascript unsafe "$r = $1;"   jsstr :: JSString -> IO JSString

main = do
  mapM_ clog                    [jx, jy, jz]
  mapM_ (clog . JSS.pack)       [hx, hy, hz]
  mapM_ putStrLn                [hx, hy, hz]
  mapM_ (putStrLn . JSS.unpack) [jx, jy, jz]
