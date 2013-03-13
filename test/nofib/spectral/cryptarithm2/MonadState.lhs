> {-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

<h1>MonadState</h1>
<haskell:module>
  <Name>MonadState</>
  <Version>0.1 alpha</>
  <Copywrite>The contents of this module are
	understood to be a straightforward implementation of
	part of the fokelore of the functional programming community,
	and therefore the contents are in the public domain.</>
  <Author>
	Rendered by <A HREF="http://www.cse.ogi.edu/~andy/">Andy Gill</a>,
	based on the paper
	<em>Functional Programming with Overloading and
	    Higher-Order Polymorphism</em>, 
	  <A HREF="http://www.cse.ogi.edu/~mpj/">Mark P Jones</a>,
		Advanced School of Functional Programming, 1995.</>
  <Restrictions>	
	This requires multi parameter classes
	and functional dependencies.
	</>
  <Tested>Hugs98</>
</haskell:module>

<hr/>

> module MonadState (
> 	MonadState(..),
> 	modify,
> 	State,		-- abstract
> 	runState,
> 	mapState,
> 	evalState,
> 	execState,
> 	StateT,		-- abstract
> 	runStateT,
> 	mapStateT,
> 	evalStateT,
> 	execStateT,
> 	module MonadTrans
> 	) where

> import Control.Monad
> import MonadTrans

<haskell:class>
  <TName>MonadState</>
</haskell:class>
{-
 - This class has two functions.
 - get : returns the state from the internals of the monad,
 - put : changes the state inside the monad.
 -}

> class (Monad m) => MonadState s m where
> 	get :: m s
> 	put :: s -> m ()


<haskell:function>
  <Purpose>Monadic state transformer.</>
  <Description>
      Maps an old state to a new state inside a state monad.
      The old state is thrown away.</>
  <Example>
	<haskell:code bgcolor="#ff88ff">
	  Main> :t modify ((+1) :: Int -> Int)
	  modify (...) :: (MonadState Int a) => a ()
	</haskell:code>
	<p>This says that modify (+1) acts over any
	Monad that is a member of the MonadState class,
	with an <haskell:expr>Int</haskell:expr> state.</p>
  </Example>
</haskell:function>

> modify :: (MonadState s m) => (s -> s) -> m ()
> modify f = do s <- get
> 	        put (f s)

------------------------------------------------------------------------------
{- Our parameterizable state monad
 -}

> newtype State s a = State { runState :: s -> (a,s) }

{-
 - The State Monad structure is paramterized over just the state:
 -
 -}

> instance Functor (State s) where
> 	fmap f p = State (\ s ->
> 		let (x,s') = runState p s
> 		in  (f x,s'))

> instance Monad (State s) where
>    return v  = State (\ s -> (v,s))
>    p  >>= f  = State (\ s -> let (r,s') = runState p s
> 			     in runState (f r) s')
>    fail str  = State (\ s -> error str)

> instance MonadState s (State s) where
> 	get   = State (\ s -> (s,s))
> 	put v = State (\ _ -> ((),v))


> mapState :: ((a,s) -> (b,s)) -> State s a -> State s b
> mapState f m = State (f . runState m)

> evalState :: State s a -> s -> a
> evalState m s = fst (runState m s)

> execState :: State s a -> s -> s
> execState m s = snd (runState m s)  

------------------------------------------------------------------------------
{- Our parameterizable state monad, with an inner monad
 -}

> newtype StateT s m a = StateT { runStateT :: s -> m (a,s) }

{-
 - The StateT Monad structure is paramterized over two things:
 -  s: The State itself.
 -  m: The inner monad. 
 -
 - Here are some examples of use:
 -
 - (Parser from ParseLib with Hugs)
 - type Parser a = StateT String [] a
 -    ==> StateT (String -> [(a,String)])
 - For example, item can be written as:
 - 	item = do (x:xs) <- get
 -		  put xs
 -		  return x
 -
 - type BoringState s a = StateT s Indentity a
 -    ==> StateT (s -> Identity (a,s))
 -
 - type StateWithIO s a = StateT s IO a
 -    ==> StateT (s -> IO (a,s))
 -
 - type StateWithErr s a = StateT s Maybe a
 -    ==> StateT (s -> Maybe (a,s))
 -}

> instance (Monad m) => Functor (StateT s m) where
> 	-- fmap :: (a -> b) -> StateT s m a -> StateT s m b
> 	fmap f p = StateT (\ s ->
> 		do (x,s') <- runStateT p s
> 		   return (f x,s'))
> 
> instance (Monad m) => Monad (StateT s m) where
>    return v  = StateT (\ s -> return (v,s))
>    p  >>= f  = StateT (\ s -> do (r,s') <- runStateT p s
> 				   runStateT (f r) s')
>    fail str  = StateT (\ s -> fail str)
> 
> instance (MonadPlus m) => MonadPlus (StateT s m) where
> 	mzero       = StateT (\ s -> mzero)
> 	p `mplus` q = StateT (\ s -> runStateT p s `mplus` runStateT q s)
> 
> instance (Monad m) => MonadState s (StateT s m) where
> 	get   = StateT (\ s -> return (s,s))
> 	put v = StateT (\ _ -> return ((),v))
> 
> instance MonadTrans (StateT s) where
>    lift f = StateT ( \ s -> do { r <- f ; runStateT (return r) s })

> mapStateT :: (m (a,s) -> n (b,s)) -> StateT s m a -> StateT s n b
> mapStateT f m = StateT (f . runStateT m)
> 
> evalStateT :: (Monad m) => StateT s m a -> s -> m a
> evalStateT m s =  
> 	do (r,_) <- runStateT m s
> 	   return r
> 
> execStateT :: (Monad m) => StateT s m a -> s -> m s
> execStateT m s =  
> 	do (_,s) <- runStateT m s
> 	   return s

------------------------------------------------------------------------------


