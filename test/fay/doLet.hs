main = do
  first
  second
  third
  fourth
  fifth
  sixth

first = do
  let x = 123
  print x

second = do
  let x = 123
  x <- return 10
  print x

third = do
  x <- return 123
  let x = 777
  print x

fourth = do
  x <- return 123
  let y = 777
  print x
  print y

fifth = do
  print 123
  x <- return 123
  print 10
  let y = 777
  print y
  print x

sixth = do
  print 123
  Just x <- return (Just 123)
  print 10
  let y = 777
  print y
  print x
