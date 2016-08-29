{-
   imported from haddock/driver/ResponseFile.hs,
   since it's not part of the Haddock library
-}
{-

Copyright 2002-2010, Simon Marlow.  All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

- Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.

- Redistributions in binary form must reproduce the above copyright notice,
this list of conditions and the following disclaimer in the documentation
and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS "AS IS" AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 -}
{-# LANGUAGE ScopedTypeVariables #-}
module ResponseFile (
    unescapeArgs,
    escapeArgs,
    expandResponse
  ) where

import Control.Exception
import Data.Char (isSpace)
import Data.Foldable (foldl')
import System.Exit (exitFailure)
import System.IO


-- | Given a string of concatenated strings, separate each by removing
-- a layer of /quoting/ and\/or /escaping/ of certain characters.
--
-- These characters are: any whitespace, single quote, double quote,
-- and the backslash character.  The backslash character always
-- escapes (i.e., passes through without further consideration) the
-- character which follows.  Characters can also be escaped in blocks
-- by quoting (i.e., surrounding the blocks with matching pairs of
-- either single- or double-quotes which are not themselves escaped).
--
-- Any whitespace which appears outside of either of the quoting and
-- escaping mechanisms, is interpreted as having been added by this
-- special concatenation process to designate where the boundaries
-- are between the original, un-concatenated list of strings.  These
-- added whitespace characters are removed from the output.
--
-- > unescapeArgs "hello\\ \\\"world\\\"\n" == escapeArgs "hello \"world\""
unescapeArgs :: String -> [String]
unescapeArgs = filter (not . null) . unescape

-- | Given a list of strings, concatenate them into a single string
-- with escaping of certain characters, and the addition of a newline
-- between each string.  The escaping is done by adding a single
-- backslash character before any whitespace, single quote, double
-- quote, or backslash character, so this escaping character must be
-- removed.  Unescaped whitespace (in this case, newline) is part
-- of this "transport" format to indicate the end of the previous
-- string and the start of a new string.
--
-- While 'unescapeArgs' allows using quoting (i.e., convenient
-- escaping of many characters) by having matching sets of single- or
-- double-quotes,'escapeArgs' does not use the quoting mechasnism,
-- and thus will always escape any whitespace, quotes, and
-- backslashes.
--
-- > unescapeArgs "hello\\ \\\"world\\\"\\n" == escapeArgs "hello \"world\""
escapeArgs :: [String] -> String
escapeArgs = unlines . map escapeArg

-- | Arguments which look like '@foo' will be replaced with the
-- contents of file @foo@. A gcc-like syntax for response files arguments
-- is expected.  This must re-constitute the argument list by doing an
-- inverse of the escaping mechanism done by the calling-program side.
--
-- We quit if the file is not found or reading somehow fails.
-- (A convenience routine for haddock or possibly other clients)
expandResponse :: [String] -> IO [String]
expandResponse = fmap concat . mapM expand
  where
    expand :: String -> IO [String]
    expand ('@':f) = readFileExc f >>= return . unescapeArgs
    expand x = return [x]

    readFileExc f =
      readFile f `catch` \(e :: IOException) -> do
        hPutStrLn stderr $ "Error while expanding response file: " ++ show e
        exitFailure

data Quoting = NoneQ | SngQ | DblQ

unescape :: String -> [String]
unescape args = reverse . map reverse $ go args NoneQ False [] []
    where
      -- n.b., the order of these cases matters; these are cribbed from gcc
      -- case 1: end of input
      go []     _q    _bs   a as = a:as
      -- case 2: back-slash escape in progress
      go (c:cs) q     True  a as = go cs q     False (c:a) as
      -- case 3: no back-slash escape in progress, but got a back-slash
      go (c:cs) q     False a as
        | '\\' == c              = go cs q     True  a     as
      -- case 4: single-quote escaping in progress
      go (c:cs) SngQ  False a as
        | '\'' == c              = go cs NoneQ False a     as
        | otherwise              = go cs SngQ  False (c:a) as
      -- case 5: double-quote escaping in progress
      go (c:cs) DblQ  False a as
        | '"' == c               = go cs NoneQ False a     as
        | otherwise              = go cs DblQ  False (c:a) as
      -- case 6: no escaping is in progress
      go (c:cs) NoneQ False a as
        | isSpace c              = go cs NoneQ False []    (a:as)
        | '\'' == c              = go cs SngQ  False a     as
        | '"'  == c              = go cs DblQ  False a     as
        | otherwise              = go cs NoneQ False (c:a) as

escapeArg :: String -> String
escapeArg = reverse . foldl' escape []

escape :: String -> Char -> String
escape cs c
  |    isSpace c
    || '\\' == c
    || '\'' == c
    || '"'  == c = c:'\\':cs -- n.b., our caller must reverse the result
  | otherwise    = c:cs
