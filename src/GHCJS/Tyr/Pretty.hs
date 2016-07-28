{-# LANGUAGE TupleSections, OverloadedStrings #-}
{-
  Prettyprinting for the Tyr AST

  The output resembles JavaScript, but includes type information
  and annotations for the special nodes.

  The result is less compact than the JS renderer result.
 -}

module GHCJS.Tyr.Pretty where

import Data.Monoid
import Data.Text (Text)

import GHCJS.Tyr.Base
import GHCJS.Tyr.Operators
import GHCJS.Tyr.Doc
import GHCJS.Tyr.Number
import GHCJS.Tyr.Render (renderBOp, renderAOp, renderUAOp, renderUOp
                        ,escapeStr, cchar)

import qualified Panic

panic :: String -> a
panic = Panic.panic . ("GHCJS.Tyr.Pretty."++)

data PrettySettings i ea sa
  = PrettySettings { prettyI   :: i  -> Doc
                   , prettyEA  :: ea -> Doc -> Doc
                   , prettySA  :: sa -> Doc -> Doc
                   , locS      :: L  -> Doc -> Doc
                   , locE      :: L  -> Doc -> Doc
                   , coE       :: ea -> ea  -> Doc -> Doc
                   , allParens :: Bool
                   }

prettyS :: PrettySettings i ea sa -> St i ea sa -> Doc
prettyS s (DeclS a v)             = prettySA s a (lit "var" <!+> prettyI s v <> semi)
prettyS _ EmptyS                  = semi
prettyS s (FunS a n (FunV as st)) = prettySA s a $
  lit "function" <!+> prettyI s n <> ctxDoc (argsListI s as) <!+> nestedS s st
prettyS s (IfS a c t f)           = prettySA s a $ 
  lit "if" <> parens (prettyE' s c) <> nestedS s t <!+>
  lit "else" <!+> nestedS s f
prettyS s (LoopS a c b)           = panic "prettyS LoopS" {- prettySA s a $
  lit "loop" <> cb <!+> nestedS s b <> ca -}
  where
{-    declToE (v,Nothing) = atom (prettyI s v)
    declToE (v,Just e)  = ECtx (prettyI s v <!+> equals <!+> prettyE' e) -}
 {-   (cb,ca) = case c of
      NoCond              -> (Empty, Empty)
      CondBefore e        -> (group (lparen <> prettyE' s e <> rparen), Empty)
      CondAfter  e        -> (Empty, group (lparen <> prettyE' s e <> rparen))
      CondFor mc init inc ->
          let c = maybe Empty (prettyE' s) mc
              i = maybe Empty (prettyE' s) inc
          in (,Empty) . group . parens . (\i' -> i' <> semi <> c <> i) $
               case init of
                 NoInit      -> Empty
                 DeclInit ds ->
                   lit "var" <!+>
                   group (ctxDoc $ prettyList s True (map prettyDecl ds))
                 ExpInit e   -> prettyE' s e -}
prettyS s (SwitchS a c cs d)      = panic "prettyS SwitchS" {- prettySA s a $
  lit "case" <> lparen <> prettyE' s c <> rparen <!+>
  nest 2 (lbrace <!$> pcs <!$> pd <!$> rbrace)
    where
      addBreak st = BlockS [st, BreakS Nothing]
      pcs = mconcat (map (\(e,st) -> pc (lit "case" <!+> prettyE' s e) (addBreak st)) cs)
      pd  = pd (lit "default") d
      pc doc st = hang 2 (doc <> colon <!$> prettyS st ) -}
prettyS s (CallT a lv cf ca)      = undefined
prettyS s (BreakS a lbl)          = panic "prettyS BreakS" {- prettySA s a $
  lit "continue" <> maybe Empty (\l -> space <> text l) <> semi -}
prettyS s (ContinueS a lbl)       = panic "prettyS ContinueS" {- prettySA s a $
  lit "continue" <> maybe Empty (\l -> space <> text l) <> semi -}
prettyS s (ExpS a e)              = panic "prettyS ExpS"  {- prettySA s a $
  prettyE s e <> semi -}
prettyS s (ReturnS a e)           = panic "prettyS ReturnS" {-prettySA s a $
  lit "return" <!+> prettyE s e <> semi-}
prettyS s (BlockS ss)             = vcat (map (prettyS s) ss)
prettyS s (LabelSA a lbl st)      = undefined
prettyS s (SwitchSA a c cc d)     = undefined
prettyS s (TrySA a t c f)         = undefined
prettyS s (ForInSA a fi c st)     = undefined
prettyS s (WithSA a e st)         = panic "prettyS WithSA" {- prettySA s a $
  lit "with" <> lparen <> group (prettyE s e <> rparen) <!+> nestedS st
-}
prettyS s (ThrowSA a e)           = panic "prettyS ThrowSA" {- prettySA s a $
  lit "throw" <!+> prettyE s e <> semi -}
prettyS s (LocS l st)             = locS s l (prettyS s st)

data ECtx = ECtx !Doc !Int !Assoc

ctxDoc :: ECtx -> Doc
ctxDoc (ECtx d _ _) = d

mapDoc :: (Doc -> Doc) -> ECtx -> ECtx
mapDoc f (ECtx d p a) = ECtx (f d) p a

prettyE' :: PrettySettings i ea sa -> Exp i ea sa -> Doc
prettyE' s e = let ECtx d _ _ = prettyE s e in d

prettyE :: PrettySettings i ea sa -> Exp i ea sa -> ECtx
prettyE s (VarE a i)      = panic "prettyE VarE" -- prettyEA' s a (prettyI s i)
prettyE s (DotE a e p)    = prettyEA' s a $ prettyDotE s e p
prettyE s (IdxE a e i)    = prettyEA' s a $ prettyIdxE s e i
prettyE s (BOpE a o l r)  = panic "prettyE BOpE"
{-  let p = precBOp o
      a = assocBOp o
      -- fixme parensR
  in prettyEA' s a $ ECtx (group $ ctxDoc (parensL p a l) <!+>
                                   renderBOp o <!+>
                                   ctxDoc (parensL p a r))
                          p
                          a -}
prettyE s (UOpE a o e)    =
  let ECtx d pi _ = prettyE s e
      kw          = isKeywordUOp o
      p           = precUOp o
      d'          = if p < pi then parens d else d
  in  prettyEA' s a $
        ECtx (renderUOp o <> (if kw then space else Empty) <> d')
             p
             (assocUOp o)
prettyE s (AOpE a o lv r) = panic "prettyE AOpE"
  -- prettyEA' s a $ prettyAOpE o lv r
prettyE s (UAOpE a o lv)
  | isPrefixUAOp o = pea (ro <> d')
  | otherwise      = pea (d' <> ro)
  where
    ro = renderUAOp o
    d' = if pi <= 2 then d else parens d
    ECtx d pi _ = prettyLV s lv
    pea x = prettyEA' s a (ECtx x op oa)
    op  = precUAOp o
    oa  = assocUAOp o
prettyE s (CondE a c t f) = panic "prettyE CondE"
  {- prettyEA' s a (ECtx d precCond assocCond)
  where
    d = group . nest 2 $
          ctxDoc (parensL s precCond assocCond c) <!$> question <!+>
          ctxDoc (parensL s precCond AssocNone t) <!$> colon <!+> -- fixme is this correct?
          ctxDoc (parensR s precCond assocCond f) -}
prettyE s (AppE a f as)   = panic "prettyE AppE"
   -- prettyEA s a (parensL s precApp assocApp f <> argsListE as)
prettyE s (FunE a mi (FunV as body)) = panic "prettyE FunE"
{-  lit "function" <> i <> argsListI s as <!$> nestedS s body
    where i = maybe Empty ((space <>) . prettyI s) mi -}
prettyE s (NewE a f as)   = panic "prettyE NewE"
  -- lit "new" <!+> parensR s precNewWithArgs assocNewWithArgs <> argsListI s as
prettyE s (CommaE a exps) = panic "prettyE CommaE" {-
  let ECtx d lp la = prettyList (map (prettyE s) exps)
  in  prettyEA' s a (ECtx (group (nest 2 d)) lp la) -}
prettyE s (NullL a)       = prettyEA' s a (litAtom "null")
prettyE s (ThisL a)       = prettyEA' s a (litAtom "this")
prettyE s (NumL a n)
  | Just xs <- formatNumLit n = atom (string xs)
  | hasMinusLit n             = mapDoc (char '-'<>) (prettyE s $ NumL a (negateNum n))
  | otherwise                 =
      panic ("prettyE: cannot format numeric literal: " ++ show n)
prettyE s (StrL a str) = prettyEA' s a $
  atom (dquote <> escapeStr False (Just '"') str <> dquote)
prettyE s (RegExpL a pat g i) = prettyEA' s a $
  atom (slash <> escapeStr True Nothing pat <> slash <> cchar i 'i' <> cchar g 'g')
prettyE s (BoolL a b) = prettyEA' s a (litAtom $ if b then "true" else "false")
prettyE s (ArrL a xs) = prettyEA' s a . atom . group $
  lbracket <> ctxDoc (prettyList True (map (prettyE s) xs)) <> rbracket
prettyE s (ObjL a o) = panic "prettyE ObjL" {-prettyEA' s a . atom $
  group (hang 0 $ lbrace <> o' <> rbrace)
  where
    o' = mconcat $
         map (\(p,e) -> text p <> colon <!+> parensR s precComma assocComma e) o
-}
prettyE s (LocE l e) = mapDoc (locE s l) (prettyE s e)
prettyE s (CoE a fa e) = mapDoc (coE s fa a) (prettyE s e)

prettyDotE :: PrettySettings i ea sa
           -> Exp i ea sa
           -> Prop
           -> ECtx
prettyDotE s e p =
  ECtx (parensL s precDot assocDot e <> dot <> text p) precDot assocDot

prettyIdxE :: PrettySettings i ea sa
           -> Exp i ea sa
           -> Exp i ea sa
           -> ECtx
prettyIdxE s e i =
  ECtx (parensL s precIdx assocIdx e <>
          lbracket <> ctxDoc (prettyE s i) <> rbracket)
       precIdx
       assocIdx

prettyAOpE :: PrettySettings i ea sa
           -> AOp
           -> LVal i ea sa
           -> Exp  i ea sa
           -> ECtx
prettyAOpE s o l e = error "prettyAOpE"

prettyLV :: PrettySettings i ea sa -> LVal i ea sa -> ECtx
prettyLV s (LVar i)    = atom (prettyI s i)
prettyLV s (LProp o p) = prettyDotE s o p
prettyLV s (LIdx e i)  = prettyIdxE s e i -- lbracket <> ctxDoc (prettyE s i) <> rbracket

prettyList -- :: PrettySettings i ea sa
           :: Bool -- ^ parenthesize comma expressions?
           -> [ECtx] -- Exp i ea sa]
           -> ECtx
prettyList _     []  = panic "prettyList: empty expression list"
prettyList False [e] = e
prettyList True  [ec@(ECtx d p _)] =
  if p >= precComma then atom (parens d) else ec
prettyList parenComma (ECtx d p _:es) =
   let t          = ctxDoc (prettyList parenComma es)
       d'         = if p >= precComma then parens d else d
   in ECtx (d' <> comma <!$> t) precComma assocComma

prettyEA' :: PrettySettings i ea sa -> ea -> ECtx -> ECtx
prettyEA' s ea (ECtx d i a) = ECtx (prettyEA s ea d) i a

argsListI :: PrettySettings i ea sa
          -> [i]
          -> ECtx
argsListI s args = mapDoc group (prettyList True (map (atom . prettyI s) args))

argsListE :: PrettySettings i ea sa
          -> [Exp i ea sa]
          -> ECtx
argsListE s args = mapDoc group (prettyList True (map (prettyE s) args))

atom :: Doc -> ECtx
atom d = ECtx d maxPrec AssocNone

nestedS :: PrettySettings i ea sa -> St i ea sa -> Doc
nestedS s st = nest 2 (lbrace <!$> prettyS s st) <!$> rbrace

litAtom :: Text -> ECtx
litAtom = atom . lit

parensL :: PrettySettings i ea sa -> Int -> Assoc -> Exp i ea sa -> Doc
parensL s = panic "parensL"

parensR :: PrettySettings i ea sa -> Int -> Assoc -> Exp i ea sa -> Doc
parensR s = panic "parensR"
