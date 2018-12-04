{-# LANGUAGE GeneralizedNewtypeDeriving,
             TemplateHaskell,
             QuasiQuotes,
             OverloadedStrings #-}

module Gen2.Utils where

import           Control.Monad.State.Strict

import           Data.Array
import qualified Data.ByteString        as B
import qualified Data.ByteString.Lazy   as BL
import qualified Data.ByteString.Search as S
import           Data.Char        (isSpace)
import           Data.List        (isPrefixOf)
import           Data.Text (Text)
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as TE
import           Compiler.JMacro

import           DynFlags
import           SrcLoc
import           Outputable (defaultUserStyle, text)
import           ErrUtils (Severity(..))
import           FastString

import qualified Debug.Trace
import qualified GHC.Stack
import           Util

trace' :: HasDebugCallStack => String -> a -> a
trace' xs a
  | otherwise = a
  | otherwise = Debug.Trace.trace (xs ++ "\n" ++ GHC.Stack.prettyCallStack GHC.Stack.callStack) a

trace'' :: HasDebugCallStack => String -> a -> a
trace'' xs a
    | otherwise = a
    | otherwise = Debug.Trace.trace (xs ++ "\n" ++ GHC.Stack.prettyCallStack GHC.Stack.callStack) a


insertAt :: Int -> a -> [a] -> [a]
insertAt 0 y xs             = y:xs
insertAt n y (x:xs) | n > 0 = x : insertAt (n-1) y xs
insertAt _ _ _ = error "insertAt"

-- hack for missing unary negation in jmacro
jneg :: JExpr -> JExpr
jneg = UOpExpr NegOp

jnull :: JExpr
jnull = ValExpr (JVar $ TxtI "null")

jvar :: Text -> JExpr
jvar xs = ValExpr (JVar $ TxtI xs)

jstr :: Text -> JExpr
jstr xs = toJExpr xs

jint :: Integer -> JExpr
jint n = ValExpr (JInt n)

jzero :: JExpr
jzero = jint 0

decl :: Ident -> JStat
decl i = DeclStat i

-- until supported in jmacro
decl' :: Ident -> JExpr -> JStat
decl' i e = decl i `mappend` AssignStat (ValExpr (JVar i)) e

decls :: Text -> JStat
decls s = DeclStat (TxtI s)

assignj :: (ToJExpr a, ToJExpr b) => a -> b -> JStat
assignj x y = [j| `x` = `y` |]

jfun :: [Ident] -> JStat -> JExpr
jfun args body = ValExpr $ JFunc args body


-- generate an identifier, use it in both statements
identBoth :: (Ident -> JStat) -> (Ident -> JStat) -> JStat
identBoth s1 s2 = UnsatBlock . IS $ do
  i <- newIdent
  return $ s1 i <> s2 i

withIdent :: (Ident -> JStat) -> JStat
withIdent s = UnsatBlock . IS $ newIdent >>= return . s

-- declare a new var and use it in statement
withVar :: (JExpr -> JStat) -> JStat
withVar s = withIdent (\i -> decl i <> s (ValExpr . JVar $ i))

newIdent :: State [Ident] Ident
newIdent = do
  xxs <- get
  case xxs of
    (x:xs) -> do
      put xs
      return x
    _ -> error "newIdent: empty list"

iex :: Ident -> JExpr
iex i = (ValExpr . JVar) i

itxt :: Ident -> T.Text
itxt (TxtI s) = s

ji :: Int -> JExpr
ji = toJExpr

showIndent :: Show s => s -> String
showIndent x = unlines . runIndent 0 . map trim . lines . replaceParens . show $ x
    where
      replaceParens ('(':xs) = "\n( " ++ replaceParens xs
      replaceParens (')':xs) = "\n)\n" ++ replaceParens xs
      replaceParens (x:xs)   = x : replaceParens xs
      replaceParens []        = []
      indent n xs = replicate n ' ' ++ xs
      runIndent n (x:xs) | "(" `isPrefixOf` x  = indent n     x : runIndent (n+2) xs
                         | ")" `isPrefixOf` x  = indent (n-2) x : runIndent (n-2) xs
                         | all isSpace x    = runIndent n xs
                         | otherwise = indent n x : runIndent n xs
      runIndent _ [] = []

trim :: String -> String
trim = let f = dropWhile isSpace . reverse in f . f

ve :: Text -> JExpr
ve = ValExpr . JVar . TxtI

concatMapM :: (Monad m, Monoid b) => (a -> m b) -> [a] -> m b
concatMapM f xs = mapM f xs >>= return . mconcat

-- fixme these should be proper keywords in jmacro
jTrue :: JExpr
jTrue = ve "true"

jFalse :: JExpr
jFalse = ve "false"

jBool :: Bool -> JExpr
jBool True = jTrue
jBool False = jFalse

-- Encode integers (for example used as Unique keys) to a relatively short String
-- that's valid as part of a JS indentifier (but it might start with a number)
encodeUnique :: Int -> String
encodeUnique = reverse . go  -- reversed is more compressible
  where
    chars = listArray (0,61) (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])
    go n | n < 0  = '_' : encodeUnique (negate n)
         | n > 61 = let (q,r) = n `quotRem` 62
                    in  chars ! r : encodeUnique q
         | otherwise = [chars ! n]


-- GHC produces modified UTF8 that the Text package doesn't particularly like
-- unmodify it before decoding
decodeModifiedUTF8 :: B.ByteString -> Maybe Text
decodeModifiedUTF8 bs
  | B.any (==0) bs = Nothing
  | otherwise      =
    either (const Nothing) Just . TE.decodeUtf8' . unmodify $ bs
    where
      unmodify = BL.toStrict . S.replace (B.pack [192,128]) (B.singleton 0)

buildingDebug :: DynFlags -> Bool
buildingDebug dflags = WayDebug `elem` ways dflags

buildingProf :: DynFlags -> Bool
buildingProf dflags = WayProf `elem` ways dflags

-- use instead of ErrUtils variant to prevent being suppressed
compilationProgressMsg :: DynFlags -> String -> IO ()
compilationProgressMsg dflags msg
  = ifVerbose dflags 1 (log_action dflags dflags NoReason SevOutput ghcjsSrcSpan (defaultUserStyle dflags) (text msg))

ifVerbose :: DynFlags -> Int -> IO () -> IO ()
ifVerbose dflags val act
  | verbosity dflags >= val = act
  | otherwise               = return ()

ghcjsSrcSpan :: SrcSpan
ghcjsSrcSpan = UnhelpfulSpan (mkFastString "<GHCJS>")
