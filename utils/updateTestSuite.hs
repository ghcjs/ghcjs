-- update the stand-alone testsuite cabal file from the
-- main ghcjs.cabal file

import Data.Char
import Data.List (isInfixOf, isPrefixOf)

main :: IO ()
main = do
  cbl  <- lines <$> readFile "ghcjs.cabal"
  tmpl <- readFile "test/ghcjs-testsuite.cabal.tmpl"
  let testSection =
        unlines .
        filterField "type" .
        filterField "hs-source-dirs" .
        takeSection .
        drop 1 .
        dropWhile (not . ("test-suite "`isPrefixIC`)) $
        cbl
  writeFile "test/ghcjs-testsuite.cabal"
    (tmpl ++ "\n" ++ testSection)

-- only works for one-line options
filterField :: String -> [String] -> [String]
filterField x = filter (not . ((' ':x++":")`isInfixIC`))

takeSection :: [String] -> [String]
takeSection = takeWhile inSection
  where 
    inSection xs = all isSpace xs ||
                   " " `isPrefixOf` xs ||
                   "--" `isPrefixOf` xs
  
isPrefixIC :: String -> String -> Bool
a `isPrefixIC` b = map toLower a `isPrefixOf` map toLower b

isInfixIC :: String -> String -> Bool
a `isInfixIC` b = map toLower a `isInfixOf` map toLower b
