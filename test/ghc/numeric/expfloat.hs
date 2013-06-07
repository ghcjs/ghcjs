{-# LANGUAGE MagicHash #-}
-- tests that expFloat# works (had linking problems on Windows)

import GHC.Exts

main = do
  print (D# (expDouble# 3.45##))
