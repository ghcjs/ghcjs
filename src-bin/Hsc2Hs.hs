{-
  hsc2hs program

  as a temporary solution, this is a wrapper around an existing hsc2hs,
  this should be replaced by a custom preprocessor to generate the
  code for the JS architecture.
-}

import           Data.Char
import           Data.List

import           System.Exit
import           System.Process
import qualified System.Info as Info

import           Compiler.Info (getFullArguments)

main :: IO ()
main = do
  args <- getFullArguments
  exitWith =<< rawSystem "hsc2hs" (filter (\x -> isELF Info.os || not (isELFArg x)) args)

{-
  when Setup.hs is compiled to JS, we have a regular, non-dynamic-too GHCJS.
  Cabal assumes that the ghcjs OS is ELF. this confuses the wrapped hsc2hs on non-ELF
  platforms
-}
isELFArg :: String -> Bool
isELFArg xs = any (`isPrefixOf` xs) ["--lflags=-Wl,-R,", "--lflag=-Wl,-R,"]

isELF :: String -> Bool
isELF xs = map toLower  xs `notElem`
  ["mingw32", "win32", "cygwin32", "darwin", "osx", "ghcjs"]
