{-# LANGUAGE NamedFieldPuns #-}

data SomeRec = SomeRec { a :: Integer, b :: Integer } | Y | X deriving Show

fun :: SomeRec -> SomeRec
fun SomeRec{a} = SomeRec{a=a+1, b=10}

fun2 :: SomeRec -> SomeRec
fun2 r = let a = 5 in r{a}

main = do
    let r = SomeRec{a=1, b=2}
    print r
    print (fun r)
    print (fun2 r)

    -- https://github.com/faylang/fay/issues/121
    let t = Y
    putStrLn $ case t of
                    SomeRec{a} -> "Bad"
                    Y -> "OK."
