module Javascript.Formatted.Statement where

import Control.Monad.Writer (tell)

import Javascript.Language
import Javascript.Formatted.Base

instance JavascriptStatement Formatted
  where expression expr = P $
          do newLine
             tellUnconstraint expr
             tell ";"
        declare id expr = P $
          do newLine
             tell $ concat ["var ", id, " = "]
             tellUnconstraint expr
             tell ";"
	ifthenelse test block1 maybeBlock2 = P $
	    do newLine
               tell "if ("
               tellUnconstraint test
               tell ") {"
	       indent $ unP block1
               newLine
	       tell "}"
               case maybeBlock2
                 of Nothing -> Prelude.return ()
                    Just block2 ->
                      do tell " else {"
                         indent $ unP block2
                         newLine
	                 tell "}"
	switch scrut def cases = P $
          do newLine
             tell "switch ("
             tellUnconstraint scrut
             tell ") {"
             unP casesP
             defP
             newLine
             tell "}"
	  where defP =
		  case def
		  of Nothing -> tell ""
		     Just (P prog) ->
                       do newLine
                          tell "default:"
                          indent prog
		casesP :: Formatted
		casesP = P . sequence_ . map (unP . uncurry caseP) $ cases
		caseP :: Expression Formatted -> Formatted -> Formatted
		caseP expr (P prog) = P $
                  do newLine
                     tell "case "
                     tellUnconstraint expr
                     tell ":"
                     indent prog
        throw e = P $
          do newLine
             tell "throw "
             tellUnconstraint e
             tell ";"

instance JavascriptReturnResult Formatted
  where return res = P $
          do newLine
             tell "return "
             tellUnconstraint res
             tell ";"

