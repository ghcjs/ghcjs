-- This file is understood to be in the public domain.

module MonadTrans where


{-
 - This provides a way of accessing a monad that is inside
 - another monad.
 -}

class MonadTrans t where
   lift :: Monad m => m a -> t m a

--liftTrans :: (MonadTrans t) => (a -> t m b) -> (t m a -> t m b)
--liftTrans f m = do { a <- m ; f a }
