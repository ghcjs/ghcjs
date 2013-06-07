{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

data C = C { a :: Int, b :: Int, c :: Int, d :: Int } deriving Show

data X = X { foo :: Int } | Y { foo :: Int }

f :: C -> Int
f (C {a, ..}) = a + d

test_fun :: C
test_fun = let {a=10; b=20; c=30; d=40} in C{..}

test2 :: X -> Int
test2 X{..} = foo

main = do
    let r = C{a=1, b=2, c=3, d=4}
    print (f r)
    print test_fun

    let x = X{foo=9}
    print (test2 x)
