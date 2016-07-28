{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-
  Render Tyr to JS
 -}

module GHCJS.Tyr.Render where

import           Prelude hiding ((<$>))

import           Data.Array
import           Data.Bits
import           Data.Char
import           Data.List (intersperse)
import           Data.Maybe
import qualified Data.Map as M
import           Data.Monoid
import           Data.Ratio
import           Data.Text (Text)
import qualified Data.Text as T

import           GHC.Generics

import           GHCJS.Tyr.Arthur
import qualified GHCJS.Tyr.Base as B
import           GHCJS.Tyr.Base (L)
import           GHCJS.Tyr.Operators
import           GHCJS.Tyr.Types
import           GHCJS.Tyr.Doc
import           GHCJS.Tyr.Number

import qualified Panic

panic :: String -> a
panic = Panic.panic . ("GHCJS.Tyr.Render."++)

-- fixme this is not enough
data RenderSettings
  = RenderSettings { convertHId :: HId -> RId
                   }

isEmptyJStat :: JStat -> Bool
isEmptyJStat (B.LocS _ s)      = isEmptyJStat s
isEmptyJStat B.EmptyS          = True
isEmptyJStat (B.BlockS [])     = True
isEmptyJStat (B.BlockS xs)     = all isEmptyJStat xs
isEmptyJStat (B.LabelSA _ _ b) = isEmptyJStat b
isEmptyJStat _                 = False

toRenderableS :: RenderSettings -> JStat -> RSt
toRenderableS s (B.DeclS _ i) = DeclS [RDecl (convertHId s i) Nothing]
toRenderableS s (B.FunS _ n (B.FunV args st)) =
  FunS (convertHId s n) (map (convertHId s) args) (toRenderableS s st)
toRenderableS s (B.IfS _ cond bt bf) =
  IfS (toRenderableE s cond) (toRenderableS s bt) bfr
    where bfr = if isEmptyJStat bf
                  then Nothing
                  else (Just $ toRenderableS s bf)
toRenderableS s (B.LoopS _ B.NoCond b) =
  ForS NoI Nothing Nothing (toRenderableS s b)
toRenderableS s (B.LoopS _ (B.CondBefore c) b) =
  WhileS (toRenderableE s c) (toRenderableS s b)
toRenderableS s (B.LoopS _ (B.CondAfter c) b) =
  DoWhileS (toRenderableS s b) (toRenderableE s c)
toRenderableS s (B.LoopS _ (B.CondFor c fi incr) b) =
  ForS (toRenderableForInit s fi)
       (fmap (toRenderableE s) c)
       (fmap (toRenderableE s) incr) (toRenderableS s b)
-- fixme add implicit break
toRenderableS s (B.SwitchS _ c cases def) =
  SwitchS (toRenderableE s c)
          (map (\(n,st) -> Case (IntL $ fromIntegral n)
                                (addBreak $ toRenderableS s st)) cases ++
             [Default (toRenderableS s def)])
  where
    addBreak s = BlockS [s, BreakS Nothing]
toRenderableS _ (B.CallT {}) = panic "toRenderableS: CallT"
toRenderableS s (B.BreakS _ lbl) = BreakS (fmap (toRenderableLbl s) lbl)
toRenderableS s (B.ContinueS _ lbl) = ContinueS (fmap (toRenderableLbl s) lbl)
toRenderableS s (B.ExpS _ e) = ExpS (toRenderableE s e)
toRenderableS s (B.ReturnS _ e) = ReturnS (Just $ toRenderableE s e) -- fixme remove undefined
toRenderableS _ B.EmptyS = EmptyS
toRenderableS s (B.BlockS stats) = BlockS (map (toRenderableS s) stats)
toRenderableS s (B.LocS loc st) = LocS loc (toRenderableS s st)
toRenderableS s (B.LabelSA _ lbl st) =
  LabelS (toRenderableLbl s lbl) (toRenderableS s st)
toRenderableS s (B.SwitchSA _ c cases def) =
  SwitchS (toRenderableE s c)
          (map (\(e,st) -> Case (toRenderableE s e)
                                (toRenderableS s st))
               cases ++
             [Default $ toRenderableS s def])
-- fixme optional finally?
toRenderableS s (B.TrySA _ ts cs fs) =
  TryS (toRenderableS s ts)
       (fmap (\(i,st) -> (convertHId s i, toRenderableS s st)) cs)
       (if isEmptyJStat fs then Nothing else Just $ toRenderableS s fs)
-- fixme no support for VarI?
toRenderableS s (B.ForInSA _ lv e st) =
  ForInS (LValI $ toRenderableLVal s lv)
         (toRenderableE s e)
         (toRenderableS s st) 
toRenderableS s (B.WithSA _ e st) =
  WithS (toRenderableE s e) (toRenderableS s st)
toRenderableS s (B.ThrowSA _ e) = ThrowS (toRenderableE s e)

toRenderableE :: RenderSettings -> JExp -> RExp
toRenderableE s (B.VarE _ i) = VarE (convertHId s i)
toRenderableE s (B.DotE _ e p) =
  DotE (toRenderableE s e) (toRenderableProp s p)
toRenderableE s (B.IdxE _ e1 e2) =
  IdxE (toRenderableE s e1) (toRenderableE s e2)
toRenderableE s (B.BOpE _ op e1 e2) =
  BOpE op (toRenderableE s e1) (toRenderableE s e2)
toRenderableE s (B.UOpE _ op e) = UOpE op (toRenderableE s e)
toRenderableE s (B.AOpE _ op l e) =
  AOpE op (toRenderableLVal s l) (toRenderableE s e)
toRenderableE s (B.UAOpE _ op l) = UAOpE op (toRenderableLVal s l)
toRenderableE s (B.CondE _ c t f) =
  CondE (toRenderableE s c) (toRenderableE s t) (toRenderableE s f)
toRenderableE s (B.AppE _ f args) =
  AppE (toRenderableE s f) (map (toRenderableE s) args)
toRenderableE s (B.FunE _ mi (B.FunV args st)) =
  FunE (fmap (convertHId s) mi)
       (map (convertHId s) args)
       (toRenderableS s st)
toRenderableE s (B.NewE _ f args) =
  NewE (toRenderableE s f) (map (toRenderableE s) args)
toRenderableE s (B.CommaE _ exps) =
  CommaE (map (toRenderableE s) exps)
toRenderableE _ (B.NullL _) = NullL
toRenderableE _ (B.ThisL _) = ThisL
toRenderableE _ (B.NumL _ nl) = convertNumLit nl
toRenderableE _ (B.StrL _ str) = StrL str
toRenderableE _ (B.RegExpL _ re glbl caseIns) = RegExpL re glbl caseIns
toRenderableE _ (B.BoolL _ b) = BoolL b
toRenderableE s (B.ArrL _ a) = ArrL (map (toRenderableE s) a) 
toRenderableE s (B.ObjL _ o) =
  ObjL $ map (\(p,e) -> (toRenderableObjLProp s p, toRenderableE s e)) o
toRenderableE s (B.LocE l e) = LocE l (toRenderableE s e)
toRenderableE s (B.CoE _ _ e) = toRenderableE s e

toRenderableLVal :: RenderSettings -> JLVal -> RLVal
toRenderableLVal s (B.LVar i) = LVar (convertHId s i)
toRenderableLVal s (B.LProp e p) =
  LProp (toRenderableE s e)
        (toRenderableProp s p)
toRenderableLVal s (B.LIdx e1 e2) =
  LIdx (toRenderableE s e1)
       (toRenderableE s e2)

toRenderableLbl :: RenderSettings -> B.Label -> RLabel
toRenderableLbl _ = RLabel

toRenderableProp :: RenderSettings -> B.Prop -> RProp
toRenderableProp _ = RProp

toRenderableForInit :: RenderSettings -> B.ForInit HId Ty () -> RForI
toRenderableForInit _ B.NoInit = NoI
toRenderableForInit s (B.DeclInit decls) =
  DeclI $ map (\(i,me) -> RDecl (convertHId s i)
                                (fmap (toRenderableE s) me))
              decls
toRenderableForInit s (B.ExpInit e) = ExpI (toRenderableE s e)


-- fixme support numprop type?
toRenderableObjLProp :: RenderSettings -> B.ObjLProp HId -> RObjLPropName
toRenderableObjLProp _ (B.StrProp t) = StrProp t
toRenderableObjLProp s (B.IdProp i)  = IdProp (convertHId s i)
toRenderableObjLProp _ (B.NumProp n) = case convertNumLit n of
  IntL i -> NumProp (RIntL i)
  RatL r -> NumProp (RRatL r)
  _      -> panic "toRenderableObjLProp: unsupported NumProp"

convertNumLit :: Number -> RExp
convertNumLit l = case l of
  IntN i           -> IntL i
  RatN r           -> RatL r
  NaN              -> BOpE DivOp (IntL 0) (IntL 0)
  Infinity         -> BOpE DivOp (IntL 1) (IntL 0)
  NegativeInfinity -> BOpE DivOp (IntL (-1)) (IntL 0)
  NegativeZero     -> UOpE NegOp (IntL 0)


-- todo rename this, clashes with binds
s :: Doc -> Doc
s d = d <> semi


-- nested statement, braces optional
(<>>>) :: Doc -> RSt -> Doc
r <>>> n = -- r <> renderS True n
  let rs = renderS True n
  in if isBraceS n
     then r <+> rs
     else -- group (r <$> indent 2 rs)
       group (nest 2 $ r <$> group rs)

-- nested statement, braces optional, but space required if no braces
(<|>>>) :: Doc -> RSt -> Doc
r <|>>> n =
  let rs = renderS True n
      sp = if isBraceS n then Optional space else space
      l  = if isBraceS n then Optional line  else line
  in if isBraceS n
     then r <> sp <> rs
     else group (nest 2 $ r <> l <> group rs) -- hmm this eats a space

-- nested block, force braces
(<!>>>) :: Doc -> RSt -> Doc
r <!>>> n
  | null (flattenBlocks [n]) = r <+> lbrace <> rbrace
  | otherwise = r <+> nest 2 (lbrace <$> renderS False n) <$$> rbrace

nestElse :: Doc -> RSt -> Doc
nestElse d s
  | isIfStat s = d <!+> renderS False s
  | otherwise  = d <|>>> s

cchar :: Bool -> Char -> Doc
cchar True c  = char c
cchar _    _  = empty

renderS :: Bool -> RSt -> Doc
renderS bb (BlockS xs)
  | null xs'   = semi
  | [x] <- xs' = renderS bb x
  | bb         = lbrace <$> indent 2 (vcat $ map (renderS False) xs) <$$> rbrace
  | otherwise  = vcat (map (renderS False) xs)
  where xs' = flattenBlocks xs
renderS _  EmptyS                 = semi
renderS _  (ExpS e)
  | maybeBrace tl {- || maybeBrace tr -} = s (parens d)
  | otherwise                      = s d
  where
    EDoc d _ _ tl tr = renderE e
renderS _  (IfS e s1 s2)          = group $
  maybe id (\se s -> s <> br <> lit "else" `nestElse` se) s2 $
  lit "if" <> if forceBraces then argE e <!>>> s1 else argE e <>>> s1
  where
    br          = if isBraceS s1 then Optional space else line
    forceBraces = isJust s2 && isIfNoElseStat s1
renderS _  (SwitchS e cs)         =
  lit "switch" <> argE e <+> nest 2
    (lbrace <$$> vcat (map renderCase cs) <$$> rbrace)
renderS _  (BreakS Nothing)       = s (lit "break")
renderS _  (BreakS (Just lbl))    = s (lit "break" <!+> renderLbl lbl)
renderS _  (ContinueS Nothing)    = s (lit "continue")
renderS _  (ContinueS (Just lbl)) = s (lit "continue" <!+> renderLbl lbl)
renderS _  (LabelS lbl s)
  | isBraceS s = renderLbl lbl <> colon <+>  renderS True s
  | otherwise  = renderLbl lbl <> colon <$$> renderS True s
renderS _  (ForInS i e s) =
  lit "for" <> parens (renderFii i <!+> lit "in" <!+> renderE' e) <>>> s
renderS _  (ForS i t incr s) =
  lit "for" <> parens (renderFi i <> semi <> maybe empty renderE' t
    <> semi <> maybe empty renderE' incr) <>>> s
renderS _  (TryS t c f) = lit "try" <!>>> t <+> c' <+> f'
  where
    c' = maybe empty (\(i,s) -> lit "catch" <> argsListI [i] <!>>> s) c
    f' = maybe empty (lit "finally" <!>>>) f
renderS _  (ThrowS e)             = s ({- nest 2 $ -} lit "throw" <> opSpace True tl <> d)
  where EDoc d _ _ tl _ = renderE e
renderS _  (ReturnS Nothing)      = s (lit "return")
renderS _  (ReturnS (Just e))     = s ({- nest 2 $ -} lit "return" <> opSpace True tl <> d)
  where EDoc d _ _ tl _ = renderE e
renderS _  (WithS e s)            = lit "with" <> parensE' e <>>> s
renderS _  (DeclS [])             = empty
renderS _  (DeclS ds)             = s (lit "var" <!+> hang 0 (group $ edDoc (renderListE True True (map declToE ds))))

--                                       Union (edDoc (renderListE False True (map declToE ds)))
--                                                            (nest 2 $ edDoc (renderListE True True (map declToE ds))))
renderS _  (FunS i as s)          = lit "function" <!+> renderId i <> argsListI as <!>>> s
renderS bb (LocS l s)             = loc l (renderS bb s)
renderS _  (WhileS e st)           = lit "while" <> argE e <>>> st
renderS _  (DoWhileS st e)         = s ((lit "do" <|>>> st) <+> lit "while" <> argE e)

data EDoc
  = EDoc { edDoc     :: !Doc
         , edMinPrec :: !Int
         , edAssoc   :: !Assoc
         , edLeft    :: !TokenTy
         , edRight   :: !TokenTy
         }

data TokenTy
  = TokenTy { maybeOp    :: !Bool -- operator char (not new/instanceof etc)
            , maybeIdent :: !Bool
            , maybeBrace :: !Bool
            }
  deriving (Eq, Ord, Show)

otherTok :: TokenTy
otherTok = TokenTy False False False

identTok :: TokenTy
identTok = TokenTy False True False

opTok :: TokenTy
opTok = TokenTy True False False

braceTok :: TokenTy
braceTok = TokenTy False False True

unknownTok :: TokenTy
unknownTok = TokenTy True True True

renderEH :: RExp -> EDoc
renderEH e =
  let EDoc d pe ae lt rt = renderE e
  in  EDoc (group $ hang 0 d) pe ae lt rt

renderEH' :: RExp -> Doc
renderEH' = group . hang 0 . renderE'

renderE' :: RExp -> Doc
renderE' = edDoc . renderE

renderE :: RExp -> EDoc
renderE (StrL xs) = atom (dquote <> escapeStr False (Just '"') xs <> dquote) otherTok otherTok
renderE (RegExpL xs glbl ci) =
  atom (slash <> escapeStr True Nothing xs <> slash <>
        cchar ci 'i' <> cchar glbl 'g') opTok (if ci || glbl then identTok else opTok)
renderE (IntL i)
  | i < 0     = renderE (UOpE NegOp (IntL $ negate i))
  | otherwise = atom (string $ show i) identTok identTok
renderE (RatL r)
  | r < 0     = renderE (UOpE NegOp (RatL $ negate r))
  | denominator r == 1 = renderE (IntL $ numerator r)
  | otherwise = let d :: Double
                    d = realToFrac r
                in atom (string (show d)) identTok identTok
renderE (BoolL True)  = litAtom "true"
renderE (BoolL False) = litAtom "false"
renderE (ArrL [])     = atom (lbracket <> rbracket) otherTok otherTok
renderE (ArrL xs)     = atom (group $ lbracket <> edDoc (renderListE False True xs) <> rbracket) otherTok otherTok
{-
(Union (lbracket <> edDoc (renderListE False True xs) <> rbracket)
                                    (hang 0 $ lbracket <> edDoc (renderListE True True xs) <> rbracket))-}
--                              otherTok otherTok
renderE NullL         = litAtom "null"
renderE (ObjL [])     = atom (lbrace <> rbrace) braceTok braceTok
-- fixme horiz if fits, otherwise vertical object layout
renderE (ObjL xs)     = atom ({- Union (lbrace <+> ls Empty <+> rbrace) -}
                                    (group . hang 0 $ lbrace <> ls lbr <> rbrace)) braceTok braceTok
  where
    lbr   = if null (drop 1 xs) then Empty else Optional linebreak
    ls br = mconcat (intersperse (comma) $ map (rp br) xs)
    rp br (p,e) =
          renderObjPropName p <>
          colon <+> nest 2 ((\(x,_,_)->x) (parensR precComma assocComma e)) <>
          br -- Optional linebreak
renderE ThisL         = litAtom "this"
renderE (VarE v)      = atom (renderId v) identTok identTok
renderE (DotE e p)    =
  EDoc (lp <> dot <> renderProp p) precDot assocDot ltl identTok
    where
      (lp, ltl, ltr) = parensL precDot assocDot e
renderE (IdxE e i)    =
  EDoc (lp <> lbracket <> renderE' i <> rbracket) precIdx assocIdx ltl otherTok
    where
      (lp, ltl, _) = parensL precIdx assocIdx e
renderE (NewE e [])   =
  EDoc (lit "new" <> opSpace True rtl <> rp) precNewNoArgs assocNewNoArgs identTok rtr
    where
      (rp, rtl, rtr) = parensR precNewNoArgs assocNewNoArgs e
renderE (NewE e as)   =
  EDoc (lit "new" <> opSpace True rtl <> rp <> argsListE as) precNewWithArgs assocNewWithArgs identTok otherTok
    where (rp, rtl, _) = parensR precNewWithArgs assocNewWithArgs e -- fixme is parensR ok?
renderE (BOpE op e1 e2) =
  EDoc (lp <> opSpace kw ltr <> renderBOp op <> group (opSpaceR kw rtl <> rp)) p a ltl rtr
  where
    kw = isKeywordBOp op
    (lp,ltl,ltr) = parensL p a e1
    (rp,rtl,rtr) = parensR p a e2
    p = precBOp  op
    a = assocBOp op
renderE (UOpE op e)
  | pi <= p && not kw && (pi < p || op == NotOp || op == BNotOp) = EDoc (ro <> d) p a opTok tr
  | pi <= p && not kw = EDoc (ro <> opSpace False tl <> d) p a opTok tr
  | pi <= p && kw     = EDoc (ro <> opSpace True tl <> d) p a identTok tr
  | otherwise         = EDoc (ro <> parens d) p a (if kw then identTok else opTok) otherTok
    where
      kw                = isKeywordUOp op
      p                 = precUOp op
      a                 = assocUOp op
      ro                = renderUOp op
      EDoc d pi _ tl tr = renderE e
renderE (AOpE op l e) =
  -- assignment ops do not clash on the right side
  EDoc (pl <> opSpace False ltr <> renderAOp op <+> pr) p a ltl rtr
  where
    (pl,  ltl, ltr) = parensL p a (lvalToE l)
    (pr, _rtl, rtr) = parensR p a e
    p = precAOp  op
    a = assocAOp op
renderE (UAOpE op l)
  | isPrefixUAOp op = EDoc (ro <> d') p a opTok tr'
  | otherwise       = EDoc (d' <> ro) p a tl' opTok
  where
    (d',tl',tr')       = if pi <= 2 then (d,tl,tr)
                         else (parens d, otherTok, otherTok)
    ro                = renderUAOp op
    p                 = precUAOp op
    a                 = assocUAOp op
    EDoc d pi _ tl tr = renderE (lvalToE l)
renderE (CondE c e1 e2) =
  let p = precCond
      a = assocCond
      (plc, ctl, ctr) = parensL p a c
      (p1, tl1, tr1)  = parensL p AssocNone e1 -- is this correct?
      (p2, tl2, tr2)  = parensR p a e2
      d = group . nest 2 $ plc <$> question <+> p1 <$> colon <+> p2
  in EDoc d p a ctl tr2
renderE (CommaE []) = panic "renderE: empty comma expression"
renderE (CommaE es) = EDoc (group (nest 2 d)) p a lt rt
  where
    EDoc d p a lt rt = renderListE True False es
renderE (AppE e as) =
  EDoc (pl <> argsListE as) precApp assocApp ltl otherTok
  where
    (pl, ltl, _) = parensL precApp assocApp e
renderE (FunE mi as s) = EDoc (fl {- Union (hang 0 fl) fl-}) 3 AssocNone identTok otherTok
  where i  = maybe empty (\d -> space <> renderId d) mi
        fl = lit "function" <> i <> argsListI as <!>>> s
renderE (LocE l e) =
  let EDoc d p a lt rt = renderE e
  in  EDoc (loc l d) p a lt rt

renderListE :: Bool   -- ^ insert line breaks?
            -> Bool   -- ^ parenthesize comma expressions? (when list length has to stay the same)
            -> [RExp]
            -> EDoc
renderListE _ _     []  = panic "renderListE: empty expression list"
renderListE l False [e] =
  let EDoc d pi ai tl tr = renderE e
  in  EDoc (d {- <> if l then Optional linebreak else Empty) -}) pi ai tl tr
renderListE l True  [e] =
  let ed@(EDoc d pi _ _ _) = let EDoc d pi ai tl tr = renderE e
                             in EDoc (d{- <> if l then Optional linebreak else Empty)-}) pi ai tl tr
  in  if pi >= precComma then parensAtom d else ed
renderListE l parenComma (e:es) =
  let EDoc d pi _ ld _ = renderE e
      t                = renderListE l parenComma es
      (d',ld')         = if pi >= precComma then (parens d, otherTok) else (d, ld)
      lb               = if l then Optional line else Empty
  in  EDoc (group d' <> comma <> lb <> edDoc t) precComma assocComma ld' (edRight t)
-- hang this list

-- | only use where one expression is expected, comma exp is not parenthesized
argE :: RExp -> Doc
argE e = lparen <> renderEH' e <> rparen

argsListE :: [RExp] -> Doc
argsListE [] = lparen <> rparen
argsListE [e] = parens $ edDoc (renderE e)
argsListE es = parens (Union flat (nest 2 withLines)) -- group withLines -- Union (flatten flat) withLines -- (Union flat withLines) -- group . hang 0 . parens . edDoc $ renderListE True es
  where
    withLines = (Optional linebreak <> {-Optional space <> -}(edDoc $ renderListE True True es))
    flat      = flatten (edDoc $ renderListE False True es)

argsListI :: [RId] -> Doc
argsListI is = parens (Union flat withLines) -- flatten withLines -- group withLines -- Union flat withLines
  where
    withLines =
      hang 0 $ -- Optional space <>
      mconcat (intersperse (comma <> Optional line {- <> Optional space-}) (map renderId is))
    flat = flatten $ mconcat (intersperse (comma <> Optional space) (map renderId is))


litAtom :: Text -> EDoc
litAtom xs = atom (text xs) identTok identTok

parensAtom :: Doc -> EDoc
parensAtom d = atom (parens d) otherTok otherTok

opSpaceR :: Bool
         -> TokenTy
         -> Doc
opSpaceR b t = opSpaceT b t (if b then space else line)

opSpace b t = opSpaceT b t space

opSpaceT :: Bool -- ^ is it a keyword op?
         -> TokenTy
         -> Doc
         -> Doc
opSpaceT keywordOp tt sp
  | keywordOp     && not (maybeIdent tt)  = Optional sp
  | not keywordOp && not (maybeOp tt)     = Optional sp
  | otherwise                             = sp

parensE' :: RExp -> Doc
parensE' = edDoc . parensE

parensE :: RExp -> EDoc
parensE e = EDoc (lparen <> renderE' e <> rparen) maxPrec AssocNone otherTok otherTok

parensL :: Int -> Assoc -> RExp -> (Doc, TokenTy, TokenTy)
parensL = parensCond needParensL

parensR :: Int -> Assoc -> RExp -> (Doc, TokenTy, TokenTy)
parensR = parensCond needParensR

parensCond :: (Int -> Assoc -> Int -> Assoc -> Bool)
           -> Int -> Assoc -> RExp -> (Doc, TokenTy, TokenTy)
parensCond p po ao e
  | p pi ao po ao = (group (lparen <> d <> rparen), otherTok, otherTok)
  | otherwise     = (d, ts, te)
  where
    (EDoc d pi _ ts te) = renderE e

atom :: Doc -> TokenTy -> TokenTy -> EDoc
atom d start end = EDoc d maxPrec AssocNone start end

needParensL :: Int -> Assoc -> Int -> Assoc -> Bool
needParensL pi ai po ao
  | pi <  po                                        = False
  | pi == po  && ai == AssocLeft && ao == AssocLeft = False
  | otherwise                                       = True

needParensR :: Int -> Assoc -> Int -> Assoc -> Bool
needParensR pi ai po ao
  | pi <  po                                          = False
  | pi == po  && ai == AssocRight && ao == AssocRight = False
  | otherwise                                         = True

renderCase :: RCase -> Doc
renderCase (Case e s)
  | null (flattenBlocks [s]) = hang 2 c
  | otherwise                = hang 2 (c <$$> renderS False s)
  where
    c = nest 4 $ lit "case" <!+> renderE' e <> colon
renderCase (Default s) = hang 2 $ lit "default:" <$$> renderS False s

renderNum :: RNumL -> Doc
renderNum (RIntL i) = string (show i)
renderNum (RRatL r) = let d :: Double
                          d = realToFrac r
                      in string (show d) -- fixme use scientific or similar?

renderLbl :: RLabel -> Doc
renderLbl (RLabel l) = text l

renderId :: RId -> Doc
renderId (RId i) = text i

renderProp :: RProp -> Doc
renderProp (RProp p) = text p

renderFi :: RForI -> Doc
renderFi NoI        = empty
renderFi (DeclI []) = empty
renderFi (DeclI xs) = lit "var" <!+> group (hang 0 . edDoc $ renderListE True True (map declToE xs))
renderFi (ExpI e)   = renderE' e

renderFii :: RForInI -> Doc
renderFii (VarI i)  = lit "var" <!+> renderId i
renderFii (LValI l) = renderE' (lvalToE l)

lvalToE :: RLVal -> RExp
lvalToE (LVar i)     = VarE i
lvalToE (LProp e p)  = DotE e p
lvalToE (LIdx e1 e2) = IdxE e1 e2

declToE :: RDecl -> RExp
declToE (RDecl i Nothing)  = VarE i
declToE (RDecl i (Just e)) = AOpE AOp (LVar i) e

renderBOp :: BOp -> Doc
renderBOp EqOp         = lit  "=="
renderBOp StrictEqOp   = lit  "==="
renderBOp NeqOp        = lit  "!="
renderBOp StrictNeqOp  = lit  "!=="
renderBOp GtOp         = rangle
renderBOp GeOp         = lit  ">="
renderBOp LtOp         = langle
renderBOp LeOp         = lit  "<="
renderBOp AddOp        = char '+'
renderBOp SubOp        = char '-'
renderBOp MulOp        = char '*'
renderBOp DivOp        = char '/'
renderBOp ModOp        = char '%'
renderBOp LShiftOp     = lit  "<<"
renderBOp RShiftOp     = lit  ">>"
renderBOp ZRShiftOp    = lit  ">>>"
renderBOp BAndOp       = char '&'
renderBOp BOrOp        = char '|'
renderBOp BXorOp       = char '^'
renderBOp LAndOp       = lit  "&&"
renderBOp LOrOp        = lit  "||"
renderBOp InstanceofOp = lit "instanceof"
renderBOp InOp         = lit "in"

renderUOp :: UOp -> Doc
renderUOp NotOp    = char '!'
renderUOp BNotOp   = char '~'
renderUOp NegOp    = char '-'
renderUOp PlusOp   = char '+'
renderUOp TypeofOp = lit "typeof"
renderUOp DeleteOp = lit "delete"
renderUOp YieldOp  = lit "yield"
renderUOp VoidOp   = lit "void"

renderAOp :: AOp -> Doc
renderAOp AOp         = equals
renderAOp AAddOp     = lit "+="
renderAOp ASubOp     = lit "-="
renderAOp AMulOp     = lit "*="
renderAOp ADivOp     = lit "/="
renderAOp AModOp     = lit "%="
renderAOp ALShiftOp  = lit "<<="
renderAOp ARShiftOp  = lit ">>="
renderAOp AZRShiftOp = lit ">>>="
renderAOp ABAndOp    = lit "&="
renderAOp ABXorOp    = lit "^="
renderAOp ABOrOp     = lit "|="

renderUAOp :: UAOp -> Doc
renderUAOp PreInc  = lit "++"
renderUAOp PostInc = lit "++"
renderUAOp PreDec  = lit "--"
renderUAOp PostDec = lit "--"

renderObjPropName :: RObjLPropName -> Doc
renderObjPropName (StrProp xs) = dquote <> escapeStr False (Just '"') xs <> dquote
renderObjPropName (IdProp i)   = renderId i
renderObjPropName (NumProp n)  = renderNum n

escapeStr :: Bool       -- ^ already escaped, don't escape backslashes again 
          -> Maybe Char -- ^ quote character
          -> Text
          -> Doc
escapeStr preEscaped q t = string (escape $ T.unpack t)
  where
    escape []                         = []
    escape (x:xs) | Just x == q       = '\\':x:escape xs
    escape ('\\':xs) | not preEscaped = '\\':'\\':escape xs
    escape ('\r':xs)                  = '\\':'r':escape xs
    escape ('\n':xs)                  = '\\':'n':escape xs
    escape ('\t':xs)                  = '\\':'t':escape xs
    escape (x:xs)
      | ord x >= 32 && ord x <= 127 = x : escape xs
      | ord x < 255 =
          let i   = ord x
              h n = hexDigit i n
          in '\\':'x':h 4:h 0:escape xs
      | otherwise =
          let i   = ord x
              h n = hexDigit i n
          in  '\\':'u':h 12:h 8:h 4:h 0:escape xs

hexDigit :: Int -> Int -> Char
hexDigit i n = hexDigits!(si.&.15)
  where si | n > 0     = i `shiftR` n
           | otherwise = i

hexDigits :: Array Int Char
hexDigits = listArray (0,15) "0123456789abcdef"

