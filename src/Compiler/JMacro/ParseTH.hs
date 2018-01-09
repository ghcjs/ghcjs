{-# LANGUAGE PackageImports #-}
module Compiler.JMacro.ParseTH (parseHSExp) where

import Language.Haskell.Meta.Parse
import qualified "template-haskell" Language.Haskell.TH as TH
-- import Language.Haskell.Exts.Translate
-- import Language.Haskell.Exts.Parser
-- import Language.Haskell.Exts.Extension
-- import Language.Haskell.Exts.Annotated.Fixity
-- import qualified Language.Haskell.Exts.Syntax as Hs

parseHSExp :: String -> Either String TH.Exp
--for haskell-src-exts-qq
--parseHSExp = fmap toExp . parseResultToEither . parseExpWithMode myDefaultParseMode

--for Language.Haskell.Meta
parseHSExp = parseExp

{-
parseResultToEither :: ParseResult a -> Either String a
parseResultToEither (ParseOk a) = Right a
parseResultToEither (ParseFailed loc e)
  = let line = Hs.srcLine loc - 1
    in Left (unlines [show line,show loc,e])

myDefaultParseMode :: ParseMode
myDefaultParseMode = ParseMode
  {parseFilename = []
  ,extensions = myDefaultExtensions
  ,ignoreLanguagePragmas = False
  ,fixities = baseFixities
  ,ignoreLinePragmas = False}

myDefaultExtensions :: [Extension]
myDefaultExtensions = [PostfixOperators
                      ,QuasiQuotes
                      ,UnicodeSyntax
                      ,PatternSignatures
                      ,MagicHash
                      ,ForeignFunctionInterface
                      ,TemplateHaskell
                      ,RankNTypes
                      ,MultiParamTypeClasses
                      ,RecursiveDo]
-}
