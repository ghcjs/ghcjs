 -- | Parser for ECMAScript 3.
{-# LANGUAGE FlexibleContexts #-}

{-
  Fixed parser, temporarily taken from the language-ecmascript package

  https://github.com/jswebtools/language-ecmascript

  language-ecmascript LICENSE:

Copyright (c) 2007--2012, Brown University, 2008-2012 Claudiu Saftoiu,
2012-2014 Stevens Institute of Technology.
All Rights Reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright 
      notice, this list of conditions and the following disclaimer in the 
      documentation and/or other materials provided with the distribution.
    * Neither the name of Brown University, Stevens Institute of Technology
      nor the names of its contributors may be used to endorse or promote
      products derived from this software without specific prior written 
      permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.

 -}
module GHCJS.Tyr.LeParser
  (parse
  , Parser  
  , expression
  , statement
  , program
  , parseFromString
  , parseFromFile
  -- old and deprecated   
  , parseScriptFromString
  , parseJavaScriptFromFile
  , parseScript
  , parseExpression
  , parseString
  , ParsedStatement
  , ParsedExpression
  , parseSimpleExpr'
  , parseBlockStmt
  , parseStatement
  , StatementParser
  , ExpressionParser
  , assignExpr
  , parseObjectLit  
  ) where

import Language.ECMAScript3.Lexer hiding (identifier)
import qualified Language.ECMAScript3.Lexer as Lexer
-- import Language.ECMAScript3.Parser.State
-- import Language.ECMAScript3.Parser.Type
import Language.ECMAScript3.Syntax hiding (pushLabel)
import Language.ECMAScript3.Syntax.Annotations
-- import Data.Default.Class
import Text.Parsec hiding (parse)
import Text.Parsec.Expr
import Control.Monad(liftM,liftM2)
import Control.Monad.Trans (MonadIO,liftIO)
import Numeric(readDec,readOct,readHex, readFloat)
import Data.Char
import Control.Monad.Identity
import Data.Maybe (isJust, isNothing, fromMaybe)
import Control.Monad.Error.Class
import Control.Applicative ((<$>), (<*>))

--------------------------------------------------------------------------------
-- module Language.ECMAScript3.Parser.State where

-- the statement label stack
type ParserState = [String]
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- module Language.ECMAScript3.Parser.Type where

-- import Language.ECMAScript3.Parser.State
-- import Text.Parsec
-- import Control.Monad.Identity

-- | The parser type, parametrised by the stream type @s@ and the
-- return value @a@
type Parser s a = ParsecT s ParserState Identity a
--------------------------------------------------------------------------------


{-# DEPRECATED ParsedStatement, ParsedExpression, StatementParser,
               ExpressionParser
    "These type aliases will be hidden in the next version" #-}

{-# DEPRECATED parseSimpleExpr', parseBlockStmt, parseObjectLit
   "These parsers will be hidden in the next version" #-}

{-# DEPRECATED assignExpr, parseExpression "Use 'expression' instead" #-}

{-# DEPRECATED parseStatement "Use 'statement' instead" #-}

{-# DEPRECATED parseScript "Use 'program' instead" #-}

{-# DEPRECATED parseScriptFromString, parseString "Use 'parseFromString' instead" #-}

{-# DEPRECATED parseJavaScriptFromFile "Use 'parseFromFile' instead" #-}

-- We parameterize the parse tree over source-locations.
type ParsedStatement = Statement SourcePos
type ParsedExpression = Expression SourcePos

-- These parsers can store some arbitrary state
type StatementParser s = Parser s ParsedStatement
type ExpressionParser s = Parser s ParsedExpression

initialParserState :: ParserState
initialParserState = []

-- | checks if the label is not yet on the stack, if it is -- throws
-- an error; otherwise it pushes it onto the stack
pushLabel :: String -> Parser s ()
pushLabel lab = do labs <- getState
                   pos <- getPosition
                   if lab `elem` labs 
                     then fail $ "Duplicate label at " ++ show pos
                     else putState (lab:labs)

popLabel :: Parser s ()
popLabel = modifyState safeTail
  where safeTail [] = []
        safeTail (_:xs) = xs

clearLabels :: ParserState -> ParserState
clearLabels _ = []

withFreshLabelStack :: Parser s a -> Parser s a
withFreshLabelStack p = do oldState <- getState
                           putState $ clearLabels oldState
                           a <- p
                           putState oldState
                           return a

identifier :: Stream s Identity Char => Parser s (Id SourcePos)
identifier =
  liftM2 Id getPosition Lexer.identifier

--{{{ Statements

-- Keep in mind that Token.reserved parsers (exported from the lexer) do not
-- consume any input on failure.  Note that all statements (expect for labelled
-- and expression statements) begin with a reserved-word.  If we fail to parse
-- this reserved-word, no input is consumed.  Hence, we can have the massive or
-- block that is parseExpression.  Note that if the reserved-word is parsed, it 
-- must be whatever statement the reserved-word indicates.  If we fail after the
-- reserved-word, we truly have a syntax error.  Since input has been consumed,
-- <|> will not try its alternate in parseExpression, and we will fail.

parseIfStmt:: Stream s Identity Char => StatementParser s 
parseIfStmt = do
  pos <- getPosition
  reserved "if"
  test <- parseParenExpr <?> "parenthesized test-expression in if statement"
  consequent <- parseStatement <?> "true-branch of if statement"
  optional semi -- TODO: in spec?
  ((do reserved "else"
       alternate <- parseStatement
       return $ IfStmt pos test consequent alternate)
   <|> return (IfSingleStmt pos test consequent))

parseSwitchStmt :: Stream s Identity Char => StatementParser s
parseSwitchStmt =
  let parseDefault = do
        pos <- getPosition
        reserved "default"
        colon
        statements <- many parseStatement
        return (CaseDefault pos statements)
      parseCase = do
         pos <- getPosition
         reserved "case"
         condition <- parseListExpr
         colon
         actions <- many parseStatement
         return (CaseClause pos condition actions)
      isCaseDefault (CaseDefault _ _) = True   
      isCaseDefault _                 = False
      checkClauses cs = case filter isCaseDefault cs of
        (_:c:_) -> fail $ "duplicate default clause in switch statement at " ++
                          show (getAnnotation c)
        _ -> return ()                  
    in do pos <- getPosition
          reserved "switch"
          test <- parseParenExpr
          clauses <- braces $ many $ parseDefault <|> parseCase
          checkClauses clauses
          return (SwitchStmt pos test clauses)

parseWhileStmt:: Stream s Identity Char => StatementParser s
parseWhileStmt = do
  pos <- getPosition
  reserved "while"
  test <- parseParenExpr <?> "parenthesized test-expression in while loop"
  body <- parseStatement
  return (WhileStmt pos test body)

parseDoWhileStmt:: Stream s Identity Char => StatementParser s
parseDoWhileStmt = do
  pos <- getPosition
  reserved "do"
  body <- parseStatement
  reserved "while" <?> "while at the end of a do block"
  test <- parseParenExpr <?> "parenthesized test-expression in do loop"
  optional semi
  return (DoWhileStmt pos body test)

parseContinueStmt:: Stream s Identity Char => StatementParser s
parseContinueStmt = do
  pos <- getPosition
  reserved "continue"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'continue.'
  id <- if sourceLine pos == sourceLine pos'
        then liftM Just identifier <|> return Nothing
        else return Nothing
  optional semi
  return $ ContinueStmt pos id

parseBreakStmt:: Stream s Identity Char => StatementParser s
parseBreakStmt = do
  pos <- getPosition
  reserved "break"
  pos' <- getPosition
  -- Ensure that the identifier is on the same line as 'break.'
  id <- if sourceLine pos == sourceLine pos'
        then liftM Just identifier <|> return Nothing
        else return Nothing
  optional semi           
  return $ BreakStmt pos id

parseBlockStmt:: Stream s Identity Char => StatementParser s
parseBlockStmt = do
  pos <- getPosition
  statements <- braces (many parseStatement)
  return (BlockStmt pos statements)

parseEmptyStmt:: Stream s Identity Char => StatementParser s
parseEmptyStmt = do
  pos <- getPosition
  semi
  return (EmptyStmt pos)

parseLabelledStmt:: Stream s Identity Char => StatementParser s
parseLabelledStmt = do
  pos <- getPosition
  -- Lookahead for the colon.  If we don't see it, we are parsing an identifier
  -- for an expression statement.
  label <- try (do label <- identifier
                   colon
                   return label)
  pushLabel $ unId label
  statement <- parseStatement
  popLabel
  return (LabelledStmt pos label statement)

parseExpressionStmt:: Stream s Identity Char => StatementParser s
parseExpressionStmt = do
  pos <- getPosition
  expr <- parseExpression -- TODO: spec 12.4?
  optional semi
  return $ ExprStmt pos expr


parseForInStmt:: Stream s Identity Char => StatementParser s
parseForInStmt =
  let parseInit = (reserved "var" >> liftM ForInVar identifier)
               <|> liftM ForInLVal lvalue
  in do pos <- getPosition
        -- Lookahead, so that we don't clash with parseForStmt
        (init,expr) <- try $ do reserved "for"
                                parens $ do init <- parseInit
                                            reserved "in"
                                            expr <- parseExpression
                                            return (init,expr)
        body <- parseStatement
        return $ ForInStmt pos init expr body

parseForStmt:: Stream s Identity Char => StatementParser s
parseForStmt =
  let parseInit = (reserved "var" >> liftM VarInit (parseVarDecl `sepBy` comma))
               <|> liftM ExprInit parseListExpr
               <|> return NoInit
    in do pos <- getPosition
          reserved "for"
          reservedOp "("
          init <- parseInit
          semi
          test <- optionMaybe parseExpression
          semi
          iter <- optionMaybe parseExpression
          reservedOp ")" <?> "closing paren"
          stmt <- parseStatement
          return $ ForStmt pos init test iter stmt

parseTryStmt:: Stream s Identity Char => StatementParser s
parseTryStmt =
  let parseCatchClause = do pos <- getPosition
                            reserved "catch"
                            id <- parens identifier
                            stmt <- parseStatement
                            return $ CatchClause pos id stmt
  in do reserved "try"
        pos <- getPosition
        guarded <- parseStatement
        mCatch <- optionMaybe parseCatchClause
        mFinally <- optionMaybe $ reserved "finally" >> parseStatement
        -- the spec requires at least a catch or a finally block to
        -- be present
        if isJust mCatch || isJust mFinally 
          then return $ TryStmt pos guarded mCatch mFinally
          else fail $ "A try statement should have at least a catch\ 
                      \ or a finally block, at " ++ show pos

parseThrowStmt:: Stream s Identity Char => StatementParser s
parseThrowStmt = do
  pos <- getPosition
  reserved "throw"
  expr <- parseExpression
  optional semi
  return (ThrowStmt pos expr)

parseReturnStmt:: Stream s Identity Char => StatementParser s
parseReturnStmt = do
  pos <- getPosition
  reserved "return"
  expr <- optionMaybe parseListExpr
  optional semi
  return (ReturnStmt pos expr)

parseWithStmt:: Stream s Identity Char => StatementParser s
parseWithStmt = do
  pos <- getPosition
  reserved "with"
  context <- parseParenExpr
  stmt <- parseStatement
  return (WithStmt pos context stmt)

parseVarDecl :: Stream s Identity Char => Parser s (VarDecl SourcePos) 
parseVarDecl = do
  pos <- getPosition
  id <- identifier
  init <- (reservedOp "=" >> liftM Just assignExpr) <|> return Nothing
  return (VarDecl pos id init)

parseVarDeclStmt:: Stream s Identity Char => StatementParser s
parseVarDeclStmt = do 
  pos <- getPosition
  reserved "var"
  decls <- parseVarDecl `sepBy` comma
  optional semi
  return (VarDeclStmt pos decls)

parseFunctionStmt:: Stream s Identity Char => StatementParser s
parseFunctionStmt = do
  pos <- getPosition
  name <- try (reserved "function" >> identifier) -- ambiguity with FuncExpr
  args <- parens (identifier `sepBy` comma)
  -- label sets don't cross function boundaries
  BlockStmt _ body <- withFreshLabelStack parseBlockStmt <?> 
                      "function body in { ... }"
  return (FunctionStmt pos name args body)

parseStatement :: Stream s Identity Char => StatementParser s
parseStatement = parseIfStmt <|> parseSwitchStmt <|> parseWhileStmt 
  <|> parseDoWhileStmt <|> parseContinueStmt <|> parseBreakStmt 
  <|> parseBlockStmt <|> parseEmptyStmt <|> parseForInStmt <|> parseForStmt
  <|> parseTryStmt <|> parseThrowStmt <|> parseReturnStmt <|> parseWithStmt 
  <|> parseVarDeclStmt  <|> parseFunctionStmt
  -- labelled, expression and the error message always go last, in this order
  <|> parseLabelledStmt <|> parseExpressionStmt <?> "statement"

-- | The parser that parses a single ECMAScript statement
statement :: Stream s Identity Char => Parser s (Statement SourcePos)
statement = parseStatement

--}}}

--{{{ Expressions

-- References used to construct this stuff:
-- + http://developer.mozilla.org/en/docs/
--     Core_JavaScript_1.5_Reference:Operators:Operator_Precedence
-- + http://www.mozilla.org/js/language/grammar14.html
--
-- Aren't expression tables nice?  Well, we can't quite use them, because of 
-- JavaScript's ternary (?:) operator.  We have to use two expression tables.
-- We use one expression table for the assignment operators that bind looser 
-- than ?: (assignTable).  The terms of assignTable are ternary expressions 
-- (parseTernaryExpr).  parseTernaryExpr left-factors the left-recursive
-- production for ?:, and is defined over the second expression table, 
-- exprTable, which consists of operators that bind tighter than ?:.  The terms
-- of exprTable are atomic expressions, parenthesized expressions, functions and
-- array references.

--{{{ Primary expressions

parseThisRef:: Stream s Identity Char => ExpressionParser s
parseThisRef = do
  pos <- getPosition
  reserved "this"
  return (ThisRef pos)

parseNullLit:: Stream s Identity Char => ExpressionParser s
parseNullLit = do
  pos <- getPosition
  reserved "null"
  return (NullLit pos)


parseBoolLit:: Stream s Identity Char => ExpressionParser s
parseBoolLit = do
    pos <- getPosition
    let parseTrueLit  = reserved "true"  >> return (BoolLit pos True)
        parseFalseLit = reserved "false" >> return (BoolLit pos False)
    parseTrueLit <|> parseFalseLit

parseVarRef:: Stream s Identity Char => ExpressionParser s
parseVarRef = liftM2 VarRef getPosition identifier

parseArrayLit:: Stream s Identity Char => ExpressionParser s
parseArrayLit = liftM2 ArrayLit getPosition (squares (assignExpr `sepEndBy` comma))

parseFuncExpr :: Stream s Identity Char => ExpressionParser s
parseFuncExpr = do
  pos <- getPosition
  reserved "function"
  name <- optionMaybe identifier
  args <- parens (identifier `sepBy` comma)
  -- labels don't cross function boundaries
  BlockStmt _ body <- withFreshLabelStack parseBlockStmt
  return $ FuncExpr pos name args body

--{{{ parsing strings

escapeChars =
 [('\'','\''),('\"','\"'),('\\','\\'),('b','\b'),('f','\f'),('n','\n'),
  ('r','\r'),('t','\t'),('v','\v'),('/','/'),(' ',' '),('0','\0')]

allEscapes:: String
allEscapes = map fst escapeChars

parseEscapeChar :: Stream s Identity Char => Parser s Char
parseEscapeChar = do
  c <- oneOf allEscapes
  let (Just c') = lookup c escapeChars -- will succeed due to line above
  return c' 

parseAsciiHexChar :: Stream s Identity Char => Parser s Char
parseAsciiHexChar = do
  char 'x'
  d1 <- hexDigit
  d2 <- hexDigit
  return ((chr.fst.head.readHex) (d1:d2:""))

parseUnicodeHexChar :: Stream s Identity Char => Parser s Char
parseUnicodeHexChar = do
  char 'u'
  liftM (chr.fst.head.readHex) 
        (sequence [hexDigit,hexDigit,hexDigit,hexDigit])
        
isWhitespace ch = ch `elem` " \t"


-- The endWith argument is either single-quote or double-quote, depending on how
-- we opened the string.
parseStringLit' endWith =
  (char endWith >> return "") <|>
  (do try (string "\\'")
      cs <- parseStringLit' endWith
      return $ "'" ++ cs) <|>
  (do char '\\'
      c0 <- fmap Left (parseEscapeChar <|> parseAsciiHexChar <|> parseUnicodeHexChar) <|> 
            fmap Right (char '\r' <|> char '\n')
      cs <- parseStringLit' endWith
      let c = either id id c0
      if c0 == Right '\r' || c0 == Right '\n' 
        then return (c:dropWhile isWhitespace cs) 
        else return (c:cs)) <|>
   liftM2 (:) anyChar (parseStringLit' endWith)

parseStringLit:: Stream s Identity Char => ExpressionParser s
parseStringLit = do
  pos <- getPosition
  -- parseStringLit' takes as an argument the quote-character that opened the
  -- string.
  str <- lexeme $ (char '\'' >>= parseStringLit') <|> (char '\"' >>= parseStringLit')
  -- CRUCIAL: Parsec.Token parsers expect to find their token on the first
  -- character, and read whitespaces beyond their tokens.  Without 'lexeme'
  -- above, expressions like:
  --   var s = "string"   ;
  -- do not parse.
  return $ StringLit pos str

--}}}

parseRegexpLit:: Stream s Identity Char => ExpressionParser s
parseRegexpLit = do
  let parseFlags = do
        flags <- many (oneOf "mgi")
        return $ \f -> f ('g' `elem` flags) ('i' `elem` flags) 
  let parseEscape :: Stream s Identity Char => Parser s Char
      parseEscape = char '\\' >> anyChar
  let parseChar :: Stream s Identity Char => Parser s Char
      parseChar = noneOf "/"
  let parseRe = (char '/' >> return "") <|> 
                (do char '\\'
                    ch <- anyChar -- TODO: too lenient
                    rest <- parseRe
                    return ('\\':ch:rest)) <|> 
                liftM2 (:) anyChar parseRe
  pos <- getPosition
  char '/'
  notFollowedBy $ char '/'
  pat <- parseRe --many1 parseChar
  flags <- parseFlags
  spaces -- crucial for Parsec.Token parsers
  return $ flags (RegexpLit pos pat)
          
parseObjectLit:: Stream s Identity Char => ExpressionParser s
parseObjectLit =
  let parseProp = do
        -- Parses a string, identifier or integer as the property name.  I
        -- apologize for the abstruse style, but it really does make the code
        -- much shorter.
        name <- liftM (\(StringLit p s) -> PropString p s) parseStringLit
            <|> liftM2 PropId getPosition identifier
            <|> liftM2 PropNum getPosition (parseNumber >>= toInt)
        colon
        val <- assignExpr
        return (name,val)
      toInt eid = case eid of
        Left i -> return $ fromIntegral i
        -- Note, the spec actually allows floats in property names.
        -- This is left for legacy reasons and will be fixed in 1.0
        Right d-> unexpected "Floating point number in property name"
    in do pos <- getPosition
          props <- braces (parseProp `sepEndBy` comma) <?> "object literal"
          return $ ObjectLit pos props

--{{{ Parsing numbers.  From pg. 17-18 of ECMA-262.
hex :: Stream s Identity Char => Parser s (Either Int Double)
hex = do s <- hexIntLit
         Left <$> wrapReadS Numeric.readHex s

decimal :: Stream s Identity Char => Parser s (Either Int Double)
decimal = do (s, i) <- decLit
             if i then Left <$> wrapReadS readDec s
                  else Right <$> wrapReadS readFloat s

wrapReadS :: ReadS a -> String -> Parser s a
wrapReadS r s = case r s of
  [(a, "")] -> return a
  _         -> fail "Bad parse: could not convert a string to a Haskell value"

parseNumber:: Stream s Identity Char => Parser s (Either Int Double) 
parseNumber = hex <|> decimal

parseNumLit:: Stream s Identity Char => ExpressionParser s
parseNumLit = do pos <- getPosition
                 eid <- lexeme $ parseNumber
                 notFollowedBy identifierStart <?> "whitespace"
                 return $ case eid of
                   Left i -> IntLit pos i
                   Right d-> NumLit pos d

------------------------------------------------------------------------------
-- Position Helper
------------------------------------------------------------------------------

withPos cstr p = do { pos <- getPosition; e <- p; return $ cstr pos e }

-------------------------------------------------------------------------------
-- Compound Expression Parsers
-------------------------------------------------------------------------------

dotRef e = (reservedOp "." >> withPos cstr identifier) <?> "property.ref"
    where cstr pos = DotRef pos e

funcApp e = parens (withPos cstr (assignExpr `sepBy` comma)) 
         <?>"(function application)"
    where cstr pos = CallExpr pos e

bracketRef e = brackets (withPos cstr parseExpression) <?> "[property-ref]"
    where cstr pos = BracketRef pos e

-------------------------------------------------------------------------------
-- Expression Parsers
-------------------------------------------------------------------------------

parseParenExpr:: Stream s Identity Char => ExpressionParser s
parseParenExpr = parens parseListExpr

-- everything above expect functions
parseExprForNew :: Stream s Identity Char => ExpressionParser s
parseExprForNew = parseThisRef <|> parseNullLit <|> parseBoolLit <|> parseStringLit 
  <|> parseArrayLit <|> parseParenExpr <|> parseNewExpr <|> parseNumLit 
  <|> parseRegexpLit <|> parseObjectLit <|> parseVarRef

-- all the expression parsers defined above
parseSimpleExpr' :: Stream s Identity Char => ExpressionParser s
parseSimpleExpr' = parseThisRef <|> parseNullLit <|> parseBoolLit 
  <|> parseStringLit <|> parseArrayLit <|> parseParenExpr
  <|> parseFuncExpr <|> parseNumLit <|> parseRegexpLit <|> parseObjectLit
  <|> parseVarRef

parseNewExpr :: Stream s Identity Char => ExpressionParser s
parseNewExpr =
  (do pos <- getPosition
      reserved "new"
      constructor <- parseSimpleExprForNew Nothing -- right-associativity
      arguments <- try (parens (assignExpr `sepBy` comma)) <|> return []
      return (NewExpr pos constructor arguments)) <|>
  parseSimpleExpr'

parseSimpleExpr (Just e) = ((dotRef e <|> funcApp e <|> bracketRef e) >>=
                            parseSimpleExpr . Just)  
                        <|> return e
parseSimpleExpr Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExpr (Just e)

parseSimpleExprForNew :: Stream s Identity Char
                      =>(Maybe ParsedExpression) -> ExpressionParser s
parseSimpleExprForNew (Just e) = ((dotRef e <|> bracketRef e) >>=
                                  parseSimpleExprForNew . Just)
                              <|> return e
parseSimpleExprForNew Nothing = do
  e <- parseNewExpr <?> "expression (3)"
  parseSimpleExprForNew (Just e)
    
--}}}

makeInfixExpr str constr = Infix parser AssocLeft where
  parser:: Stream s Identity Char
        => Parser s (Expression SourcePos -> Expression SourcePos -> Expression SourcePos)
  parser = do
    pos <- getPosition
    reservedOp str
    return (InfixExpr pos constr)  -- points-free, returns a function


-- apparently, expression tables can't handle immediately-nested prefixes
parsePrefixedExpr :: Stream s Identity Char => ExpressionParser s
parsePrefixedExpr = do
  pos <- getPosition
  op <- optionMaybe $ (reservedOp "!" >> return PrefixLNot) <|> 
                      (reservedOp "~" >> return PrefixBNot) <|>
                      (try (lexeme $ char '-' >> notFollowedBy (char '-')) >>
                       return PrefixMinus) <|>
                      (try (lexeme $ char '+' >> notFollowedBy (char '+')) >>
                       return PrefixPlus) <|>
                      (reserved "typeof" >> return PrefixTypeof) <|>
                      (reserved "void" >> return PrefixVoid) <|>
                      (reserved "delete" >> return PrefixDelete)
  case op of
    Nothing -> unaryAssignExpr
    Just op -> do
      innerExpr <- parsePrefixedExpr
      return (PrefixExpr pos op innerExpr)

exprTable:: Stream s Identity Char => [[Operator s ParserState Identity ParsedExpression]]
exprTable = 
  [ [ makeInfixExpr "*" OpMul
    , makeInfixExpr "/" OpDiv
    , makeInfixExpr "%" OpMod
    ]
  , [ makeInfixExpr "+" OpAdd
    , makeInfixExpr "-" OpSub
    ]
  , [ makeInfixExpr "<<" OpLShift
    , makeInfixExpr ">>" OpSpRShift
    , makeInfixExpr ">>>" OpZfRShift
    ]
  , [ makeInfixExpr "<" OpLT
    , makeInfixExpr "<=" OpLEq
    , makeInfixExpr ">" OpGT
    , makeInfixExpr ">=" OpGEq
    , makeInfixExpr "instanceof" OpInstanceof
    , makeInfixExpr "in" OpIn
    ]
  , [ makeInfixExpr "==" OpEq
    , makeInfixExpr "!=" OpNEq
    , makeInfixExpr "===" OpStrictEq
    , makeInfixExpr "!==" OpStrictNEq
    ]
  , [ makeInfixExpr "&" OpBAnd ]
  , [ makeInfixExpr "^" OpBXor ]
  , [ makeInfixExpr "|" OpBOr ]
  , [ makeInfixExpr "&&" OpLAnd ]
  , [ makeInfixExpr "||" OpLOr ]
  ]

parseExpression' :: Stream s Identity Char => ExpressionParser s
parseExpression' = 
  buildExpressionParser exprTable parsePrefixedExpr <?> "simple expression"

asLValue :: Stream s Identity Char
         => SourcePos
         -> Expression SourcePos 
         -> Parser s (LValue SourcePos)
asLValue p' e = case e of
  VarRef p (Id _ x) -> return (LVar p x)
  DotRef p e (Id _ x) -> return (LDot p e x)
  BracketRef p e1 e2 -> return (LBracket p e1 e2)
  otherwise -> fail $ "expected a left-value at " ++ show p'

lvalue :: Stream s Identity Char => Parser s (LValue SourcePos)
lvalue = do
  p <- getPosition
  e <- parseSimpleExpr Nothing
  asLValue p e

unaryAssignExpr :: Stream s Identity Char => ExpressionParser s
unaryAssignExpr = do
  p <- getPosition
  let prefixInc = do
        reservedOp "++"
        liftM (UnaryAssignExpr p PrefixInc) lvalue
  let prefixDec = do
        reservedOp "--"
        liftM (UnaryAssignExpr p PrefixDec) lvalue
  let postfixInc e = do
        reservedOp "++"
        liftM (UnaryAssignExpr p PostfixInc) (asLValue p e)
  let postfixDec e = do
        reservedOp "--"
        liftM (UnaryAssignExpr p PostfixDec) (asLValue p e)
  let other = do
        e <- parseSimpleExpr Nothing
        postfixInc e <|> postfixDec e <|> return e
  prefixInc <|> prefixDec <|> other

parseTernaryExpr':: Stream s Identity Char
                 => Parser s (ParsedExpression,ParsedExpression)
parseTernaryExpr' = do
    reservedOp "?"
    l <- assignExpr
    colon
    r <- assignExpr
    return (l,r)

parseTernaryExpr:: Stream s Identity Char => ExpressionParser s
parseTernaryExpr = do
  e <- parseExpression'
  e' <- optionMaybe parseTernaryExpr'
  case e' of
    Nothing -> return e
    Just (l,r) -> do p <- getPosition
                     return $ CondExpr p e l r

assignOp :: Stream s Identity Char => Parser s AssignOp
assignOp = (reservedOp "=" >> return OpAssign)
        <|>(reservedOp "+=" >> return OpAssignAdd)
        <|>(reservedOp "-=" >> return OpAssignSub)
        <|>(reservedOp "*=" >> return OpAssignMul)
        <|>(reservedOp "/=" >> return OpAssignDiv)
        <|>(reservedOp "%=" >> return OpAssignMod)
        <|>(reservedOp "<<=" >> return OpAssignLShift)
        <|>(reservedOp ">>=" >> return OpAssignSpRShift)
        <|>(reservedOp ">>>=" >> return OpAssignZfRShift)
        <|>(reservedOp "&=" >> return OpAssignBAnd)
        <|>(reservedOp "^=" >> return OpAssignBXor)
        <|>(reservedOp "|=" >> return OpAssignBOr)

assignExpr :: Stream s Identity Char => ExpressionParser s
assignExpr = do
  p <- getPosition
  lhs <- parseTernaryExpr
  let assign = do
        op <- assignOp
        lhs <- asLValue p lhs
        rhs <- assignExpr
        return (AssignExpr p op lhs rhs)
  assign <|> return lhs

parseExpression:: Stream s Identity Char => ExpressionParser s
parseExpression = parseListExpr

-- | A parser that parses ECMAScript expressions
expression :: Stream s Identity Char => Parser s (Expression SourcePos)
expression = parseExpression

parseListExpr :: Stream s Identity Char => ExpressionParser s
parseListExpr = assignExpr `sepBy1` comma >>= \exprs ->
  case exprs of
    [expr] -> return expr
    es     -> liftM2 ListExpr getPosition (return es)

parseScript:: Stream s Identity Char => Parser s (JavaScript SourcePos)
parseScript = do
  whiteSpace
  liftM2 Script getPosition (parseStatement `sepBy` whiteSpace)

-- | A parser that parses an ECMAScript program.
program :: Stream s Identity Char => Parser s (JavaScript SourcePos)
program = parseScript
  
-- | Parse from a stream given a parser, same as 'Text.Parsec.parse'
-- in Parsec. We can use this to parse expressions or statements alone,
-- not just whole programs.
parse :: Stream s Identity Char
      => Parser s a -- ^ The parser to use
      -> SourceName -- ^ Name of the source file
      -> s -- ^ the stream to parse, usually a 'String'
      -> Either ParseError a
parse p = runParser p initialParserState

-- | A convenience function that takes a 'String' and tries to parse
-- it as an ECMAScript program:
--
-- > parseFromString = parse program ""
parseFromString :: String -- ^ JavaScript source to parse
                  -> Either ParseError (JavaScript SourcePos)
parseFromString = parse program ""

-- | A convenience function that takes a filename and tries to parse
-- the file contents an ECMAScript program, it fails with an error
-- message if it can't.
parseFromFile :: (Error e, MonadIO m, MonadError e m) => String -- ^ file name
                -> m (JavaScript SourcePos)
parseFromFile fname =
  liftIO (readFile fname) >>= \source ->
  case parse program fname source of
    Left err -> throwError $ strMsg $ show err
    Right js -> return js

-- | Read a JavaScript program from file an parse it into a list of
-- statements
parseJavaScriptFromFile :: MonadIO m => String -- ^ file name
                        -> m [Statement SourcePos]
parseJavaScriptFromFile filename = do
  chars <- liftIO $ readFile filename
  case parse parseScript filename chars of
    Left err               -> fail (show err)
    Right (Script _ stmts) -> return stmts

-- | Parse a JavaScript program from a string
parseScriptFromString :: String -- ^ source file name
                      -> String -- ^ JavaScript source to parse
                      -> Either ParseError (JavaScript SourcePos)
parseScriptFromString = parse parseScript

-- | Parse a JavaScript source string into a list of statements
parseString :: String -- ^ JavaScript source
            -> [Statement SourcePos]
parseString str = case parse parseScript "" str of
  Left err -> error (show err)
  Right (Script _ stmts) -> stmts
