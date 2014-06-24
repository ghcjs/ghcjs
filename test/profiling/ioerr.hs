{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
import GHC.Stack
import Control.Exception.Base
import Data.Typeable

data MyException = MyException deriving (Show, Typeable)

instance Exception MyException

err1 :: IO Integer
err1 = {-# SCC error1 #-} err2 >>= return

err2 :: IO Integer
err2 = {-# SCC error2 #-} throw MyException

main = print =<< whoCreated =<< catch err1 =<< evaluate (\(_ :: MyException) -> return (42 :: Integer))
