main = do
  print (sumTo 100000 0 :: Double)

sumTo 0 acc = acc
sumTo n acc = sumTo (n - 1) (acc + n)
