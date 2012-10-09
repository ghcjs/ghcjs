{-

The first pass of the code generator generates nested functions like:

$hs_a = function() {
    toplevel: $hs_b = static_data()
    toplevel: jmId_1 = function() {
    }
}

- all statements with the `toplevel' label are floated out to top level
    ( AST needs to be saturated before floating )

-}
module Gen2.Floater where

import Language.Javascript.JMacro
import Data.Maybe (catMaybes)
import Data.Monoid
import Data.List (isSuffixOf, partition)
import Data.Generics.Aliases
import Data.Generics.Schemes

floatTop :: JStat -> JStat
floatTop ast = mconcat (collectTop ast) <> removeTop ast

collectTop :: JStat -> [JStat]
collectTop ast = map (everywhere (mkT removeTop))
               . map unlabel
               $ listify isToplevelStatement ast

removeTop :: JStat -> JStat
removeTop ast = everywhere (mkT removeTopStmt) ast

removeTopStmt :: JStat -> JStat
removeTopStmt s
    | isToplevelStatement s = mempty
    | otherwise           = s

unlabel :: JStat -> JStat
unlabel (LabelStat _ x) = x
unlabel x               = x

isToplevelStatement :: JStat -> Bool
isToplevelStatement (LabelStat "toplevel" _) = True
isToplevelStatement _                        = False

