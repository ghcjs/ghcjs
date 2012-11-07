{-# LANGUAGE OverloadedStrings #-}
{-
  Custom prettyprinter for JMacro AST
  - uses the jmacro prettyprinter for most of the work
  - fixme: need a better way to make a customized prettyprinter, without duplicating 5 cases
-}
module Gen2.Printer where

import qualified Data.Text.Lazy               as TL
import           Language.Javascript.JMacro   (Ident, JExpr (..), JStat (..),
                                               JVal (..), jsToDoc)
import           Text.PrettyPrint.Leijen.Text (Doc, align, char, comma, empty,
                                               fillSep, hcat, nest, parens,
                                               punctuate, text, vcat, (<+>),
                                               (<>))
import qualified Text.PrettyPrint.Leijen.Text as PP


pretty :: JStat -> Doc
pretty (BlockStat xs) = prettyBlock (flattenBlocks xs)
-- these things contain other statements, rewire them to use our prettyprinter
--- (fixme remove this if possible)
pretty (IfStat cond x y) = text "if" <> parens (jsToDoc cond) <+> braceNest'' (pretty x) <+> mbElse
        where mbElse | y == BlockStat []  = PP.empty
                     | otherwise = text "else" <+> braceNest'' (pretty y)
pretty (WhileStat False p b)  = text "while" <> parens (jsToDoc p) <+> braceNest'' (pretty b)
pretty (WhileStat True  p b)  = (text "do" <+> braceNest'' (pretty b)) <+> text "while" <+> parens (jsToDoc p)
pretty (TryStat s i s1 s2) = text "try" <+> braceNest' (pretty s) $$ mbCatch $$ mbFinally
        where mbCatch | s1 == BlockStat [] = PP.empty
                      | otherwise = text "catch" <> parens (jsToDoc i) $$ braceNest' (pretty s1)
              mbFinally | s2 == BlockStat [] = PP.empty
                        | otherwise = text "finally" $$ braceNest' (pretty s2)
pretty (SwitchStat e l d) = text "switch" <+> parens (jsToDoc e) <+> braceNest'' cases
        where l' = map (\(c,s) -> (text "case" <+> parens (jsToDoc c) <> char ':') $$$ (pretty s)) l ++ [text "default:" $$$ (pretty d)]
              cases = vcat l'
pretty (ForInStat each i e b) = text txt <> parens (text "var" <+> jsToDoc i <+> text "in" <+> jsToDoc e) <+> braceNest'' (pretty b)
        where txt | each = "for each"
                  | otherwise = "for"
-- these are unsupported for ghcjs
pretty (ForeignStat{}) = error "unexpected foreign stat"
pretty (AntiStat {})   = error "unexpected antiquotation"
pretty (UnsatBlock {}) = error "unexpected unsaturated block"
pretty b               = jsToDoc b

prettyBlock :: [JStat] -> Doc
prettyBlock xs = vcat $ map addSemi (prettyBlock' xs)

-- recognize common patterns in a block and convert them to more idiomatic/concise javascript
prettyBlock' :: [JStat] -> [Doc]
-- resugar for loops with/without var declaration
prettyBlock' ( (DeclStat i _)
             : (AssignStat (ValExpr (JVar i')) v0)
             : (WhileStat False p (BlockStat bs))
             : xs
             )
    | i == i' && not (null flat) && isForUpdStat (last flat)
    = mkFor True i v0 p (last flat) (init flat) : prettyBlock' xs
       where
         flat = flattenBlocks bs
prettyBlock' ( (AssignStat (ValExpr (JVar i)) v0)
             : (WhileStat False p (BlockStat bs))
             : xs
             )
    | not (null flat) && isForUpdStat (last flat)
    = mkFor False i v0 p (last flat) (init flat) : prettyBlock' xs
       where
         flat = flattenBlocks bs

-- global function (does not preserve semantics but works for GHCJS)
prettyBlock' ( (DeclStat i _)
             : (AssignStat (ValExpr (JVar i')) (ValExpr (JFunc is b)))
             : xs
             )
    | i == i' = (text "function" <+> jsToDoc i
                 <> parens (fillSep . punctuate comma . map jsToDoc $ is)
                 $$ braceNest' (pretty b)
                ) : prettyBlock' xs
-- declare/assign
prettyBlock' ( (DeclStat i _)
             : (AssignStat (ValExpr (JVar i')) v)
             : xs
             )
    | i == i' = (text "var" <+> jsToDoc i <+> char '=' <+> jsToDoc v) : prettyBlock' xs

-- modify/assign operators (fixme this should be more general, but beware of side effects like PPostExpr)
prettyBlock' ( (AssignStat (ValExpr (JVar i)) (InfixExpr "+" (ValExpr (JVar i')) (ValExpr (JInt 1))))
             : xs
             )
    | i == i' = ("++" <> jsToDoc i) : prettyBlock' xs
prettyBlock' ( (AssignStat (ValExpr (JVar i)) (InfixExpr "-" (ValExpr (JVar i')) (ValExpr (JInt 1))))
             : xs
             )
    | i == i' = ("--" <> jsToDoc i) : prettyBlock' xs
prettyBlock' ( (AssignStat (ValExpr (JVar i)) (InfixExpr "+" (ValExpr (JVar i')) e))
             : xs
             )
    | i == i' = (jsToDoc i <+> text "+=" <+> jsToDoc e) : prettyBlock' xs
prettyBlock' ( (AssignStat (ValExpr (JVar i)) (InfixExpr "-" (ValExpr (JVar i')) e))
             : xs
             )
    | i == i' = (jsToDoc i <+> text "-=" <+> jsToDoc e) : prettyBlock' xs

-- nothing special: use regular pretty
prettyBlock' (x:xs) = pretty x : prettyBlock' xs
prettyBlock' [] = []

-- build the for block
mkFor :: Bool -> Ident -> JExpr -> JExpr -> JStat -> [JStat] -> Doc
mkFor decl i v0 p s1 sb = text "for" <> forCond <+> braceNest'' (pretty $ BlockStat sb)
    where
      c0 | decl      = text "var" <+> jsToDoc i <+> char '=' <+> jsToDoc v0
         | otherwise =                jsToDoc i <+> char '=' <+> jsToDoc v0
      forCond = parens $ hcat $ interSemi
                            [ c0
                            , jsToDoc p
                            , parens (pretty s1)
                            ]

-- check if a statement is suitable to be converted to something in the for(;;x) position
isForUpdStat :: JStat -> Bool
isForUpdStat (PPostStat {})  = True
isForUpdStat (AssignStat {}) = True
isForUpdStat (ApplStat {})   = True
isForUpdStat _               = False

interSemi :: [Doc] -> [Doc]
interSemi [] = [PP.empty]
interSemi [s] = [s]
interSemi (x:xs) = x <> text ";" : interSemi xs

addSemi :: Doc -> Doc
addSemi x = x <> text ";"

-- stuff below is from jmacro
infixl 5 $$, $+$
($+$), ($$), ($$$) :: Doc -> Doc -> Doc
x $+$ y = x PP.<$> y
x $$ y  = align (x $+$ y)
x $$$ y = align (nest 2 $ x $+$ y)

flattenBlocks :: [JStat] -> [JStat]
flattenBlocks (BlockStat y:ys) = flattenBlocks y ++ flattenBlocks ys
flattenBlocks (y:ys) = y : flattenBlocks ys
flattenBlocks [] = []

braceNest :: Doc -> Doc
braceNest x = char '{' <+> nest 2 x $$ char '}'

braceNest' :: Doc -> Doc
braceNest' x = nest 2 (char '{' $+$ x) $$ char '}'

-- somewhat more compact (egyptian style) braces
braceNest'' :: Doc -> Doc
braceNest'' x = nest 2 (char '{' PP.<$> x) PP.<$> char '}'

