data Person1 = Person1 String String Int
data Person2 = Person2 { fname :: String, sname :: String, age :: Int }
data Person3 = Person3 { slot3 :: String, slot2 :: String, slot1 :: Int }

p1  = Person1 "Chris" "Done" 13
p2  = Person2 "Chris" "Done" 13
p2a = Person2 { fname = "Chris", sname = "Done", age = 13 }
p3  = Person3 "Chris" "Done" 13

main = do
  putStrLn (case p1 of Person1 "Chris" "Done" 13 -> "Hello!")
  putStrLn (case p2 of Person2 "Chris" "Done" 13 -> "Hello!")
  putStrLn (case p2a of Person2 "Chris" "Done" 13 -> "Hello!")
  putStrLn (case p3 of Person3 "Chris" "Done" 13 -> "Hello!")
