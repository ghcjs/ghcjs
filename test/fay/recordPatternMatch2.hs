data Person = Person String String Int

main = putStrLn (case Person "Chris" "Done" 14 of
                   Person "Chris" "Done" 13 -> "Foo!"
                   Person "Chris" "Barf" 14 -> "Bar!"
                   Person "Chris" "Done" 14 -> "Hello!"
                   _ -> "World!")
