{-# LANGUAGE CPP, TypeFamilies #-}
module Javascript.Formatted.Base
  ( Identation
  , FormattedWriter
  , Formatted (P, unP)
  , indent
  , newLine
  , Precedence
  , mkOperation
  , tellWithPrecedenceConstraint
  , tellUnconstraint
  , tell
  ) where

import Data.Monoid

import Control.Monad (when)
#ifdef MIN_VERSION_monads_tf
import Control.Monad.Writer (Writer, tell, execWriter)
import Control.Monad.Reader (ReaderT, ask, local, runReaderT)
#else
import Javascript.Formatted.Monads (Writer, execWriter)
import qualified Javascript.Formatted.Monads as Monads
import Javascript.Formatted.Monads (ReaderT, ask, local, runReaderT, liftReaderT)
#endif

import Javascript.Language

type Identation = Int
type FormattedWriter = ReaderT Identation (Writer String)
newtype Formatted = P { unP :: FormattedWriter () }

#ifndef MIN_VERSION_monads_tf
tell = liftReaderT . Monads.tell
#endif

indent :: FormattedWriter a -> FormattedWriter a
indent = local (+1)

newLine :: FormattedWriter ()
newLine =
  do tell "\n"
     n <- ask
     let (t, s) = quotRem n 8
     tell $ replicate t '\t'
     tell $ replicate s ' '

instance Show Formatted
  where show = execWriter . flip runReaderT 0 . unP

instance Monoid Formatted
  where mempty = P (Prelude.return ())
        mappend (P a) (P b) = P (a >> b)
        mconcat = P . sequence_ . map unP

type Precedence = Int
instance JavascriptBase Formatted
  where -- | Expression is a function from precedence of the enclosing context
        -- to actual code writer.
        newtype Expression Formatted = E (Precedence -> FormattedWriter ())

tellWithPrecedenceConstraint :: Expression Formatted -> Precedence -> FormattedWriter ()
tellWithPrecedenceConstraint (E e) = e

-- | Helper function to define operation with given precedence
-- operations in expression are to be executed in order of precedence
mkOperation :: Precedence -> FormattedWriter () -> Expression Formatted
mkOperation ep b = E $ \p ->
  do when (p < ep) $ tell "("
     b
     when (p < ep) $ tell ")"

tellUnconstraint :: Expression Formatted -> FormattedWriter ()
tellUnconstraint e = tellWithPrecedenceConstraint e 20

