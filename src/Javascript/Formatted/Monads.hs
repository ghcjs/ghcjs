module Javascript.Formatted.Monads (
        Writer, execWriter, tell,
        ReaderT, runReaderT, ask, local, asks, liftReaderT
  ) where

import GhcMonad
import MonadUtils
import Control.Monad
import Data.Monoid

-- | The parameterizable reader monad.
--
-- Computations are functions of a shared environment.
--
-- The 'return' function ignores the environment, while @>>=@ passes
-- the inherited environment to both subcomputations.
type Reader r = ReaderT r Identity

-- | Constructor for computations in the reader monad.
reader :: (r -> a) -> Reader r a
reader f = ReaderT (Identity . f)

-- | Runs a @Reader@ and extracts the final value from it.
runReader :: Reader r a         -- ^ A @Reader@ to run.
    -> r                        -- ^ An initial environment.
    -> a
runReader m = runIdentity . runReaderT m

-- | Transform the value returned by a @Reader@.
mapReader :: (a -> b) -> Reader r a -> Reader r b
mapReader f = mapReaderT (Identity . f . runIdentity)

-- | Execute a computation in a modified environment
-- (a specialization of 'withReaderT').
withReader
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> Reader r a       -- ^ Computation to run in the modified environment.
    -> Reader r' a
withReader = withReaderT

-- | The reader monad transformer,
-- which adds a read-only environment to the given monad.
--
-- The 'return' function ignores the environment, while @>>=@ passes
-- the inherited environment to both subcomputations.
newtype ReaderT r m a = ReaderT {
        -- | The underlying computation, as a function of the environment.
        runReaderT :: r -> m a
    }

-- | Transform the computation inside a @ReaderT@.
mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

-- | Execute a computation in a modified environment
-- (a  more general version of 'local').
withReaderT
    :: (r' -> r)        -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r' m a
withReaderT f m = ReaderT $ runReaderT m . f

instance (Functor m) => Functor (ReaderT r m) where
    fmap f  = mapReaderT (fmap f)

instance (Monad m) => Monad (ReaderT r m) where
    return   = liftReaderT . return
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = ReaderT (fail msg)

instance (MonadPlus m) => MonadPlus (ReaderT r m) where
    mzero       = liftReaderT mzero
    m `mplus` n = ReaderT $ \ r -> runReaderT m r `mplus` runReaderT n r

instance (MonadFix m) => MonadFix (ReaderT r m) where
    mfix f = ReaderT $ \ r -> mfix $ \ a -> runReaderT (f a) r

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = liftReaderT . liftIO

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

-- | Fetch the value of the environment.
ask :: (Monad m) => ReaderT r m r
ask = ReaderT return

-- | Execute a computation in a modified environment
-- (a specialization of 'withReaderT').
local :: (Monad m)
    => (r -> r)         -- ^ The function to modify the environment.
    -> ReaderT r m a    -- ^ Computation to run in the modified environment.
    -> ReaderT r m a
local = withReaderT

-- | Retrieve a function of the current environment.
asks :: (Monad m)
    => (r -> a)         -- ^ The selector function to apply to the environment.
    -> ReaderT r m a
asks f = liftM f ask



-- | Identity functor and monad.
newtype Identity a = Identity { runIdentity :: a }

-- ---------------------------------------------------------------------------
-- Identity instances for Functor and Monad

instance Functor Identity where
    fmap f m = Identity (f (runIdentity m))

instance Monad Identity where
    return a = Identity a
    m >>= k  = k (runIdentity m)



-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by the type @w@ of output to accumulate.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
type Writer w = WriterT w Identity

-- | Construct a writer computation from a (result, output) pair.
-- (The inverse of 'runWriter'.)
writer :: (a, w) -> Writer w a
writer = WriterT . Identity

-- | Unwrap a writer computation as a (result, output) pair.
-- (The inverse of 'writer'.)
runWriter :: Writer w a -> (a, w)
runWriter = runIdentity . runWriterT

-- | Extract the output from a writer computation.
--
-- * @'execWriter' m = 'snd' ('runWriter' m)@
execWriter :: Writer w a -> w
execWriter m = snd (runWriter m)

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriter' ('mapWriter' f m) = f ('runWriter' m@)
mapWriter :: ((a, w) -> (b, w')) -> Writer w a -> Writer w' b
mapWriter f = mapWriterT (Identity . f . runIdentity)

-- ---------------------------------------------------------------------------
-- | A writer monad parameterized by:
--
--   * @w@ - the output to accumulate.
--
--   * @m@ - The inner monad.
--
-- The 'return' function produces the output 'mempty', while @>>=@
-- combines the outputs of the subcomputations using 'mappend'.
newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }

-- | Extract the output from a writer computation.
--
-- * @'execWriterT' m = 'liftM' 'snd' ('runWriterT' m)@
execWriterT :: Monad m => WriterT w m a -> m w
execWriterT m = do
    ~(_, w) <- runWriterT m
    return w

-- | Map both the return value and output of a computation using
-- the given function.
--
-- * @'runWriterT' ('mapWriterT' f m) = f ('runWriterT' m@)
mapWriterT :: (m (a, w) -> n (b, w')) -> WriterT w m a -> WriterT w' n b
mapWriterT f m = WriterT $ f (runWriterT m)

instance (Functor m) => Functor (WriterT w m) where
    fmap f = mapWriterT $ fmap $ \ ~(a, w) -> (f a, w)

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    m >>= k  = WriterT $ do
        ~(a, w)  <- runWriterT m
        ~(b, w') <- runWriterT (k a)
        return (b, w `mappend` w')
    fail msg = WriterT $ fail msg

instance (Monoid w, MonadPlus m) => MonadPlus (WriterT w m) where
    mzero       = WriterT mzero
    m `mplus` n = WriterT $ runWriterT m `mplus` runWriterT n

instance (Monoid w, MonadFix m) => MonadFix (WriterT w m) where
    mfix m = WriterT $ mfix $ \ ~(a, _) -> runWriterT (m a)

liftWriterT m = WriterT $ do
    a <- m
    return (a, mempty)

instance (Monoid w, MonadIO m) => MonadIO (WriterT w m) where
    liftIO = liftWriterT . liftIO

-- | @'tell' w@ is an action that produces the output @w@.
tell :: (Monoid w, Monad m) => w -> WriterT w m ()
tell w = WriterT $ return ((), w)
