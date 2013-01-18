withTwo :: (Int -> Int) -> Int
withTwo f = f 2

main = do
  print $ (* 3) (2::Int)
  print $ (7 `div`) (2::Int)
  print $ withTwo (4*)
