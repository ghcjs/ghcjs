data SomeRec = SomeRec { a :: Integer, b :: Integer } | Y | X

main = do
    -- https://github.com/faylang/fay/issues/121
    putStrLn $ case Y of
                    SomeRec _ _ -> "Bad"
                    Y -> "OK."

    let t = Y
    putStrLn $ case t of
                    SomeRec _ _ -> "Bad"
                    Y -> "OK."
