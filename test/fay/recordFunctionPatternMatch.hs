data Person = Person String String Int

main = putStrLn (foo (Person "Chris" "Done" 14))

foo (Person "Chris" "Done" 13) = "Foo!"
foo (Person "Chris" "Barf" 14) = "Bar!"
foo (Person "Chris" "Done" 14) = "Hello!"
foo _ = "World!"
