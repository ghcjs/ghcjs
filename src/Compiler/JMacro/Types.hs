{-# Language StandaloneDeriving, DeriveDataTypeable, FlexibleContexts, UndecidableInstances, FlexibleInstances #-}
module Compiler.JMacro.Types (
  JType(..), Constraint(..), JLocalType, VarRef, anyType, parseType, runTypeParser
  ) where

import Control.Applicative hiding ((<|>))
import Data.Char

import Data.Maybe(fromMaybe)

import Text.ParserCombinators.Parsec
import Text.Parsec.Prim hiding (runParser, try)
import Text.ParserCombinators.Parsec.Language(emptyDef)
import qualified Text.ParserCombinators.Parsec.Token as P

import qualified Data.Map as M
import Data.Map (Map)
import Data.Set (Set)
import Data.Generics

type VarRef = (Maybe String, Int)

-- sum types for list/record, map/record

data JType = JTNum
           | JTString
           | JTBool
           | JTStat
           | JTFunc [JType] (JType)
           | JTList JType
           | JTMap  JType
           | JTRecord JType (Map String JType)
           | JTRigid VarRef (Set Constraint)
           | JTImpossible
           | JTFree VarRef
           | JTForall [VarRef] JType
             deriving (Eq, Ord, Read, Show, Typeable, Data)

data Constraint = Sub JType
                | Super JType
                  deriving (Eq, Ord, Read, Show, Typeable, Data)
{-
                 | Choice Constraint Constraint
                 | GLB (Set JType)
                 | LUB (Set JType)
-}
type JLocalType = ([(VarRef,Constraint)], JType)

type TypeParserState = (Int, Map String Int)

type TypeParser a = CharParser TypeParserState a

typLang :: P.LanguageDef TypeParserState
typLang = emptyDef {
           P.reservedNames = ["()","->"],
           P.reservedOpNames = ["()","->","::"],
           P.identLetter = alphaNum <|> oneOf "_$",
           P.identStart  = letter <|> oneOf "_$"
          }

lexer :: P.TokenParser TypeParserState
lexer = P.makeTokenParser typLang

reservedOp :: String -> TypeParser ()
parens, braces, brackets, lexeme :: TypeParser a -> TypeParser a
identifier :: TypeParser String
commaSep, commaSep1 :: TypeParser a -> TypeParser [a]
parens    = P.parens lexer
braces    = P.braces lexer
brackets  = P.brackets lexer
identifier= P.identifier lexer
reservedOp= P.reservedOp lexer
commaSep1 = P.commaSep1 lexer
commaSep  = P.commaSep  lexer

lexeme    = P.lexeme lexer

parseType :: String -> Either ParseError JType
parseType s = runParser anyType (0,M.empty) "" s

parseConstrainedType :: String -> Either ParseError JLocalType
parseConstrainedType s = runParser constrainedType (0,M.empty) "" s

runTypeParser :: CharParser a JLocalType
runTypeParser = withLocalState (0,M.empty) (try (parens constrainedType) <|> constrainedType) -- anyType

withLocalState :: (Functor m, Monad m) => st -> ParsecT s st m a -> ParsecT s st' m a
withLocalState initState subParser = mkPT $
    \(State input pos otherState) -> fixState otherState <$> runParsecT subParser (State input pos initState)
      where
        fixState s res = (fmap . fmap) go res
            where go (Ok a (State input pos _localState) pe) = Ok a (State input pos s) pe
                  go (Error e) = (Error e)



constrainedType :: TypeParser JLocalType
constrainedType = do
  c <- try (Just <$> (constraintHead <* reservedOp "=>")) <|> return Nothing
  t <- anyType
  return (fromMaybe [] c, t)

--do we need to read supertype constraints, i.e. subtype constraints which have the freevar on the right??
constraintHead :: TypeParser [(VarRef,Constraint)]
constraintHead = parens go <|> go
    where go = commaSep1 constraint
          constraint = do
            r <- freeVarRef =<< identifier
            c <- (reservedOp "<:" >> (return Sub)) <|>
                 (reservedOp ":>" >> (return Super))
            t <- anyType
            return $ (r, c t)

anyType :: TypeParser JType
anyType = try (parens anyType) <|> funOrAtomType <|> listType <|> recordType

funOrAtomType :: TypeParser JType
funOrAtomType = do
  r <- anyNestedType `sepBy1` (lexeme (string "->"))
  return $ case reverse r of
    [x] -> x
    (x:xs) -> JTFunc (reverse xs) x
    _ -> error "funOrAtomType"

listType :: TypeParser JType
listType = JTList <$> brackets anyType

anyNestedType :: TypeParser JType
anyNestedType = nullType <|> parens anyType <|> atomicType <|> listType <|> recordType

nullType :: TypeParser JType
nullType = reservedOp "()" >> return JTStat

atomicType :: TypeParser JType
atomicType = do
  a <- identifier
  case a of
    "Num" -> return JTNum
    "String" -> return JTString
    "Bool" -> return JTBool
    (x:_) | isUpper x -> fail $ "Unknown type: " ++ a
          | otherwise -> JTFree <$> freeVarRef a
    _ -> error "typeAtom"

recordType :: TypeParser JType
recordType = braces $ JTRecord JTImpossible . M.fromList <$> commaSep namePair
    where namePair = do
            n <- identifier
            reservedOp "::"
            t <- anyType
            return (n, t)

freeVarRef :: String -> TypeParser VarRef
freeVarRef v = do
  (i,m) <- getState
  (\x -> (Just v, x)) <$> maybe (setState (i+1,M.insert v i m) >> return i)
                                 return
                                 (M.lookup v m)
