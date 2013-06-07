data Person = Person String String Int

main = putStrLn (case Person "Chris" "Done" 14 of
                   Person "Chris" "Done" 13 -> "Hello!"
                   _ -> "World!")
