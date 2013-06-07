main = do
  print $ fst $ properFraction 1.5
  print $ snd $ properFraction 1.5
  print $ truncate (-1.5)
  print $ round (-1.5)
  print $ ceiling (-1.5)
  print $ floor (-1.5)
  return ()
