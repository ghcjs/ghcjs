quote s = "\"" ++ s ++ "\""

main = do
  mapM_ (putStrLn . quote) $ words "  this  is\ta\n\r\ftest  "
  putStrLn $ quote $ unwords ["this", "is", "too"]
  mapM_ (putStrLn . quote) $ lines " \n  testing\n\n"
  putStrLn $ quote $ unlines ["testing this", "as well"]
