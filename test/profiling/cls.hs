import GHC.Stack

mkCls :: Integer -> (Integer -> Integer)
mkCls a = {-# SCC outer #-} \b -> {-# SCC inner #-} errorWithStackTrace "err"

main = do
  print =<< whoCreated mkCls
  let a = mkCls 10
  print =<< whoCreated a
  let b = a 20
  print =<< whoCreated b
  print b
