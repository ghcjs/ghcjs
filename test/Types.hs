{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Aeson

import           Data.Text (Text)
import qualified Data.Text as T

import           System.Exit (ExitCode(..))

{-
  a stdio test tests two things:
  stdout/stderr/exit output must be either:
     - the same as filename.out/filename.err/filename.exit (if any exists)
     - the same as runhaskell output (otherwise)
  the javascript is run with `js' (SpiderMonkey) and `node` (v8)
  if they're in $PATH.
-}

data StdioResult = StdioResult { stdioExit :: ExitCode
                               , stdioOut :: Text
                               , stdioErr :: Text
                               }
instance Eq StdioResult where
  (StdioResult e1 ou1 er1) == (StdioResult e2 ou2 er2) =
    e1 == e2 && (T.strip ou1 == T.strip ou2) && (T.strip er1 == T.strip er2)

instance FromJSON StdioResult where
  parseJSON (Object o) = StdioResult <$> fmap convertExit (o .: "exit")
                                     <*> o .: "out"
                                     <*> o .: "err"
    where
      convertExit 0 = ExitSuccess
      convertExit n = ExitFailure n
  parseJSON _ = mempty
