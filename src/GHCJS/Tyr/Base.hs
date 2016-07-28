{-# LANGUAGE DeriveDataTypeable,
             DeriveGeneric,
             DeriveAnyClass,
             Rank2Types,
             TemplateHaskell,
             GADTs
  #-}
{-
  Typed intermediate rep
 -}

module GHCJS.Tyr.Base where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding (coerce)
import Data.Bitraversable
import Data.Bifunctor
import Data.Bifoldable
import Data.Data
import Data.Data.Lens
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import Data.Vector (Vector)

import GHC.Generics

import Panic

import GHCJS.Tyr.Operators
import GHCJS.Tyr.Number

data Pos  = Pos !Int !Int
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NFData)

data Span = Span !Int !Int !Int !Int
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NFData)

data L = LocFile !Text !Int !Int !Int !Int
       | LocEmpty
       | LocInternal
       | LocUnknown
  deriving (Eq, Ord, Show, Data, Typeable, Generic, NFData)

instance Monoid L where
  mempty = LocEmpty
  LocEmpty   `mappend` x        = x
  x          `mappend` LocEmpty = x
  LocFile fa la1 ca1 la2 ca2 `mappend` LocFile fb lb1 cb1 lb2 cb2
    | fa == fb = let ((lc1,cc1),(lc2,cc2)) = combineSpans (la1,ca1) (la2,ca2)
                                                          (lb1,cb1) (lb2,cb2)
                 in  LocFile fa lc1 cc1 lc2 cc2
  x `mappend` y | x == y = x
  _ `mappend` _ = LocUnknown

combineSpans :: (Int,Int) -> (Int,Int) -> (Int,Int) -> (Int,Int)
             -> ((Int,Int),(Int,Int))
combineSpans start1 end1 start2 end2 =
  (min start1 start2, max end1 end2)

data St i ea sa
  = DeclS      { _sa :: !sa
                   , _declVar :: !i }
--                   , _body    :: !(St i ea sa) } -- scopes depend on phase -- fixme probably remove, might mess up source locs
  | FunS       { _sa :: !sa
                   , _name :: !i
                   , _funv :: !(FunV i ea sa) }
      -- Haskell function (special?
{-  | HFunS      { _sa :: !sa
                   , _name :: !i
                   , _body :: !(St i ea sa) } -}
  | IfS        { _sa :: !sa
                   , _cond    :: !(Exp i ea sa)
                   , _branchT :: !(St i ea sa)
                   , _branchF :: !(St i ea sa) }
  | LoopS      { _sa :: !sa
         --          , _linit   :: (St i ea sa) -- fixme, should be more limited than stats
                   , _lcond   :: !(LoopCond i ea sa)
                   , _body    :: !(St i ea sa)
         {---          , _incr    :: Maybe (Exp i ea sa) -} }
      -- restricted form, no fallthrough, implicit break in cases
  | SwitchS    { _sa :: !sa
                   , _cond    :: !(Exp i ea sa)
                   , _cases   :: [(Int,(St i ea sa))]
                   , _defCase :: (St i ea sa) }
      -- Tuple result
  | CallT      { _sa :: !sa
                   , _tlval    :: [LVal i ea sa]
                   , _callFun  :: !(Exp i ea sa)
                   , _callArgs :: [(Exp i ea sa)] }
  | BreakS     { _sa :: !sa
                   , _lbl     :: Maybe Label }
  | ContinueS  { _sa :: !sa
                   , _lbl     :: Maybe Label }
  | ExpS       { _sa :: !sa
                   , _exp     :: !(Exp i ea sa) }
  | ReturnS    { _sa :: !sa
                   , _exp     :: !(Exp i ea sa) }
    -- grouping only, no individual annotations
  | EmptyS
  | BlockS     { _stats :: [St i ea sa] }
  | LocS       { _loc :: !L
                   , _body :: !(St i ea sa) }
    -- do not use the nodes below if there's an alternative,
    -- the optimizer cannot do a good job on them
  | LabelSA    { _sa :: !sa
                   , _label   :: !Label
                   , _body    :: !(St i ea sa) }
  | SwitchSA   { _sa :: !sa
                   , _cond    :: !(Exp i ea sa)
                   , _casesA  :: [((Exp i ea sa),(St i ea sa))]
                   , _defCase :: (St i ea sa) }
{-  | ContinueSA { _sa :: !sa
                   , _label   :: !Label }
  | BreakSA    { _sa :: !sa
                   , _label   :: !Label } -}
  | TrySA      { _sa :: !sa
                   , _tryBody     :: !(St i ea sa)
                   , _catchBody   :: (Maybe (i, (St i ea sa)))
                   , _finallyBody :: !(St i ea sa) }
  | ForInSA    { _sa :: !sa
                   , _fiLv    :: !(LVal i ea sa)
                   , _exp     :: !(Exp i ea sa)
                   , _body    :: !(St i ea sa) }
  | WithSA     { _sa :: !sa
                   , _exp     :: !(Exp i ea sa)
                   , _body    :: !(St i ea sa) }
  | ThrowSA    { _sa :: !sa
                   , _exp     :: !(Exp i ea sa) }
  deriving (Show, Eq, Data, Typeable, Generic)

instance Monoid (St i ea sa) where
  mempty = EmptyS
  EmptyS    `mappend` x         = x
  x         `mappend` EmptyS    = x
  BlockS xs `mappend` BlockS ys = BlockS (xs++ys)
  x         `mappend` BlockS ys = BlockS (x:ys)
  BlockS ys `mappend` x         = BlockS (ys++[x])
  x         `mappend` y         = BlockS [x,y]

data Exp i ea sa
  = VarE       { _ea :: !ea
                   , _var     :: !i }
  | DotE       { _ea :: !ea
                   , _obj     :: !(Exp i ea sa)
                   , _prop    :: !Prop }
  | IdxE       { _ea :: !ea
                   , _obj     :: !(Exp i ea sa)
                   , _idx     :: !(Exp i ea sa) }
  | BOpE       { _ea :: !ea
                   , _bop     :: !BOp -- (AnOp ea BOp)
                   , _lhs     :: !(Exp i ea sa)
                   , _rhs     :: !(Exp i ea sa) }
  | UOpE       { _ea :: !ea
                   , _uop     :: !UOp -- (AnOp ea UOp)
                   , _expOp   :: !(Exp i ea sa) }
  | AOpE       { _ea :: !ea
                   , _aop     :: !AOp -- (AnOp ea AOp)
                   , _lval    :: !(LVal i ea sa)
                   , _rhs     :: !(Exp i ea sa) }
  | UAOpE      { _ea :: !ea
                   , _uaop    :: !UAOp -- (AnOp ea UAOp)
                   , _lval    :: !(LVal i ea sa) }
  | CondE      { _ea :: !ea
                   , _expCond :: !(Exp i ea sa)
                   , _expT    :: !(Exp i ea sa)
                   , _expF    :: !(Exp i ea sa) }
  | AppE       { _ea :: !ea
                   , _fun     :: !(Exp i ea sa)
                   , _args    :: [Exp i ea sa] }
  | FunE       { _ea :: !ea
                   , _funid   :: (Maybe i)
                   , _efunv   :: !(FunV i ea sa) }
  | NewE       { _ea :: !ea
                   , _fun     :: !(Exp i ea sa)
                   , _args    :: [Exp i ea sa] }
  | CommaE     { _ea :: !ea
                   , _exps    :: [Exp i ea sa] }
    -- built-in special identifiers
  | NullL      { _ea :: !ea }
  | ThisL      { _ea :: !ea }
    -- literals
  | NumL       { _ea :: !ea
                   , _numLit  :: !Number }
  | StrL       { _ea :: !ea
                   , _strVal  :: !Text }
  | RegExpL    { _ea :: !ea
                   , _strVal  :: !Text -- warning, pre-escaped!
                   , _global  :: !Bool
                   , _caseIns :: !Bool }
  | BoolL      { _ea :: !ea
                   , _boolVal :: !Bool }
  | ArrL       { _ea :: !ea
                   , _arrVals :: [(Exp i ea sa)] }
  | ObjL       { _ea :: !ea
                   , _objVal  :: [(ObjLProp i,Exp i ea sa)] }
    -- Haskell things
  | LocE       { _loce :: !L  -- source location, no annotation
                   , _lexp :: !(Exp i ea sa) }
  | CoE        { _ea :: !ea         -- conversion
                   , _eaFrom  :: !ea
                   , _expCo   :: !(Exp i ea sa) }
  deriving (Show, Eq, Data, Typeable, Generic)

data LoopCond i ea sa
  = CondFor    (Maybe (Exp i ea sa)) !(ForInit i ea sa) (Maybe (Exp i ea sa))
  | CondBefore !(Exp i ea sa)
  | CondAfter  !(Exp i ea sa)
  | NoCond
  deriving (Show, Eq, Data, Typeable, Generic)

data ForInit i ea sa
  = NoInit
  | DeclInit [(i, Maybe (Exp i ea sa))]
  | ExpInit  !(Exp i ea sa)
  deriving (Show, Eq, Data, Typeable, Generic)

type St'  i a = St  i a a
type Exp' i a = Exp i a a

data ObjLProp i
  = StrProp !Text
  | IdProp  !i
  | NumProp !Number
  deriving (Show, Eq, Data, Typeable, Generic)

type Prop  = Text
type Label = Text

data LVal i ea sa
  = LVar  { {- _lea :: !ea
              , -} _lvar  :: !i }             -- a
  | LProp { {- _lea :: !ea
              ,-} _lobj  :: !(Exp i ea sa)   -- a.b
              , _lprop :: !Prop }
  | LIdx  { {-_lea :: !ea
              ,-} _larr  :: !(Exp i ea sa)   -- a[b]
              , _lsub  :: !(Exp i ea sa) }
  deriving (Show, Eq, Data, Typeable, Generic)

-- fixme do we get type annotations for all components?
{-
data CallLVal i ea sa
  = CLVal   !(LVal i ea sa)
  | CLTuple [i]
  deriving (Show, Eq, Ord, Data, Typeable, Generic)
-}

-- data LTuple i = LTuple 

data FunV i ea sa
  = FunV [i] (St i ea sa)
  deriving (Show, Eq, Data, Typeable, Generic)

{-
data NumLit
  = NumI { _numlI :: Integer  }
  | NumR { _numlR :: Rational }
  | NaN
  | Infinity
  | NegInfinity
  | NegZero
  deriving (Show, Eq, Ord, Data, Typeable, Generic)
-}

data AnOp ea o
  = AO  !ea !o
  | NAO !o
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

----------------------------------------------------------------------
-- derived lenses and prisms
----------------------------------------------------------------------

makeLenses ''St
makePrisms ''St

makeLenses ''LVal
makePrisms ''LVal

makeLenses ''Exp
makePrisms ''Exp
{-
makeLenses ''NumLit
makePrisms ''NumLit
-}
makeLenses ''LoopCond
makePrisms ''LoopCond

makePrisms ''FunV

instance Bitraversable (St i) where bitraverse = annotsS
instance Bifunctor     (St i) where bimap      = bimapDefault
instance Bifoldable    (St i) where bifoldMap  = bifoldMapDefault

instance Bitraversable (Exp i) where bitraverse = annotsE
instance Bifunctor     (Exp i) where bimap      = bimapDefault
instance Bifoldable    (Exp i) where bifoldMap  = bifoldMapDefault

----------------------------------------------------------------------
-- traversing the structures
----------------------------------------------------------------------

-- identifiers

identsE :: Traversal (Exp i ea sa) (Exp j ea sa) i j
identsE f (VarE x1 x2)          = VarE x1         <$> f x2
identsE f (DotE x1 x2 x3)       = DotE x1        <$> identsE f x2
                                                        <*> pure x3
identsE f (IdxE x1 x2 x3)       = IdxE x1         <$> identsE f x2
                                                        <*> identsE f x3
identsE f (BOpE x1 x2 x3 x4)    = BOpE x1 x2      <$> identsE f x3
                                                        <*> identsE f x4
identsE f (UOpE x1 x2 x3)       = UOpE x1 x2      <$> identsE f x3
identsE f (AOpE x1 x2 x3 x4)    = AOpE x1 x2      <$> identsLV f x3
                                                        <*> identsE f x4
identsE f (UAOpE x1 x2 x3)      = UAOpE x1 x2    <$> identsLV f x3
identsE f (CondE x1 x2 x3 x4)   = CondE x1        <$> identsE f x2
                                                        <*> identsE f x3
                                                        <*> identsE f x4
identsE f (AppE x1 x2 x3)       = AppE x1         <$> identsE f x2
                                                        <*> (traverse.identsE) f x3
identsE f (FunE x1 x2 x3)          = FunE x1         <$> traverse f x2 <*> identsFV f x3
identsE f (NewE x1 x2 x3)       = NewE x1      <$> identsE f x2
                                                        <*> (traverse.identsE) f x3
identsE f (CommaE x1 x2)        = CommaE x1       <$> (traverse.identsE) f x2
identsE _ (NullL x)            = pure (NullL x)
identsE _ (ThisL x)            = pure (ThisL x)
identsE _ (NumL x1 x2)          = pure (NumL x1 x2)
identsE _ (StrL x1 x2)          = pure (StrL x1 x2)
identsE _ (RegExpL x1 x2 x3 x4) = pure (RegExpL x1 x2 x3 x4)
identsE _ (BoolL x1 x2)         = pure (BoolL x1 x2)
identsE f (ArrL x1 x2)          = ArrL x1         <$> (traverse.identsE) f x2
identsE f (ObjL x1 x2)          = ObjL x1         <$>
                                  traverse (\(p,e) -> (,) <$> identsOLP f p <*> identsE f e) x2
                                  -- (traverse.identsE) f x2
identsE f (LocE x1 x2)          = LocE x1 <$> identsE f x2
identsE f (CoE x1 x2 x3)        = CoE x1 x2 <$> identsE f x3

identsLV :: Traversal (LVal i ea sa) (LVal j ea sa) i j
identsLV f (LVar {- x1 -} x2)      = LVar {- x1 -} <$> f x2
identsLV f (LProp{- x1 -} x2 x3) = LProp{- x1 -} <$> identsE f x2 <*> pure x3
identsLV f (LIdx{- x1 -} x2 x3)  = LIdx {- x1 -} <$> identsE f x2 <*> identsE f x3

identsS :: Traversal (St i ea sa) (St j ea sa) i j
identsS f (DeclS x1 x2)       = DeclS x1      <$> f x2
identsS f (FunS x1 x2 x3)        = FunS x1       <$> f x2
                                                       <*> identsFV f x3
identsS f (IfS x1 x2 x3 x4)      = IfS x1        <$> identsE f x2
                                                       <*> identsS f x3
                                                       <*> identsS f x4
identsS f (LoopS x1 x2 x3)    = LoopS x1      <$> identsLC f x2
                                                       <*> identsS f x3

identsS f (SwitchS x1 x2 x3 x4)  = SwitchS x1    <$> identsE f x2
                                                       <*> (traverse._2.identsS) f x3
                                                       <*> identsS f x4
identsS _ (BreakS x1 x2)            = pure (BreakS x1 x2)
identsS _ (ContinueS x1 x2)         = pure (ContinueS x1 x2)
identsS f (ExpS x1 x2)           = ExpS x1       <$> identsE f x2
identsS f (ReturnS x1 x2)        = ReturnS x1    <$> identsE f x2
identsS _ EmptyS                    = pure EmptyS
identsS f (BlockS x)                = BlockS           <$> (traverse.identsS) f x
identsS f (LabelSA x1 x2 x3)     = LabelSA x1 x2 <$> identsS f x3
identsS f (SwitchSA x1 x2 x3 x4) = SwitchSA x1 <$> identsE f x2
                                                       <*> (traverse.beside identsE identsS) f x3
                                                       <*> identsS f x4
-- identsS _ (ContinueSA x1 x2)     = pure (ContinueSA x1 x2)
-- identsS _ (BreakSA x1 x2)        = pure (BreakSA x1 x2)
identsS f (TrySA x1 x2 x3 x4)    = TrySA x1      <$> identsS f x2
                                                       <*> (traverse.beside id identsS) f x3
                                                       <*> identsS f x4
identsS f (ForInSA x1 x2 x3 x4)  = ForInSA x1    <$> identsLV f x2
                                                       <*> identsE f x3
                                                       <*> identsS f x4
identsS f (WithSA x1 x2 x3)      = WithSA x1     <$> identsE f x2
                                                       <*> identsS f x3
identsS f (ThrowSA x1 x2)        = ThrowSA x1    <$> identsE f x2
identsS f (CallT x1 x2 x3 x4)    = CallT x1 <$> (traverse.identsLV) f x2
                                   <*> identsE f x3
                                   <*> (traverse.identsE) f x4
identsS f (LocS x1 x2) = LocS x1 <$> identsS f x2

identsFV :: Traversal (FunV i ea sa) (FunV j ea sa) i j
identsFV f (FunV is s) = FunV <$> traverse f is <*> identsS f s

identsLC :: Traversal (LoopCond i ea sa) (LoopCond j ea sa) i j
identsLC f (CondFor x1 x2 x3) = CondFor <$> (traverse . identsE) f x1
                                        <*> identsFI f x2
                                        <*> (traverse . identsE) f x3
identsLC f (CondBefore x) = CondBefore <$> identsE f x
identsLC f (CondAfter x)  = CondAfter  <$> identsE f x
identsLC _ NoCond         = pure NoCond

identsFI :: Traversal (ForInit i ea sa) (ForInit j ea sa) i j
identsFI f (ExpInit e)   = ExpInit  <$> identsE f e 
identsFI f (DeclInit xs) = DeclInit <$> traverse (\(i,e) -> (,) <$> f i <*> (traverse . identsE) f e) xs
identsFI _ NoInit        = pure NoInit

identsOLP :: Traversal (ObjLProp i) (ObjLProp j) i j
identsOLP _ (StrProp x) = pure (StrProp x)
identsOLP f (IdProp i)  = IdProp <$> f i
identsOLP _ (NumProp n) = pure (NumProp n)

-- locations
locationsE :: (Data i, Data ea, Data sa)
           => Traversal' (Exp i ea sa) L
locationsE = template

locationsS :: (Data i, Data ea, Data sa)
           => Traversal' (St i ea sa) L
locationsS = template

-- expression annotations
eannotsE :: Traversal (Exp i ea sa) (Exp i ea' sa) ea ea'
eannotsE f = annotsE f pure

eannotsS :: Traversal (St i ea sa) (St i ea' sa) ea ea'
eannotsS f = annotsS f pure

getAnnotE :: Exp i ea sa -> ea
getAnnotE e =
  fromMaybe (panic $ "Gen2.Tyr.Base: getAnnotE: no annotation")
            (preview eannotsE e)

-- statement annotations
sannotsE :: Traversal (Exp i ea sa) (Exp i ea sa') sa sa'
sannotsE f = annotsE pure f

sannotsS :: Traversal (St i ea sa) (St i ea sa') sa sa'
sannotsS f = annotsS pure f

-- caution, this will give the wrong result if other nodes
-- have no annotation field
-- getAnnotS :: St i ea sa -> Maybe sa
-- mkAccessor "getAnnotS" 2 ''St
{-
getAnnotS EmptyS     = Nothing
getAnnotS (BlockS{}) = Nothing
getAnnotS x          = preview sannotsS e
-}
-- sum annotations
-- figure out later
{-
data GLeft
data GRight
data GEither a b p where
  GLeft  :: a -> GEither a b GLeft
  GRight :: b -> GEither a b GRight

unGLeft :: GEither a b GLeft -> a
unGLeft (GLeft x) = x

unGRight :: GEither a b GRight -> b
unGRight (GRight x) = x

infixl 9 <++>
(<++>) :: (a -> b) -> (c -> d) -> (forall e. GEither a c e -> GEither b d e)
f <++> g = \x -> case x of
  GLeft  l -> GLeft  (f l)
  GRight r -> GRight (g r)

bothAnnotsE :: Applicative f
            => (forall c. GEither ea sa c -> f (GEither ea' sa' c))
            -> (Exp i ea sa) -> f (Exp i ea' sa')
bothAnnotsE f = annotsE (fmap unGLeft.f.GLeft) (fmap unGRight.f.GRight)

bothAnnotsS :: Applicative f
            => (forall c. GEither ea sa c -> f (GEither ea' sa' c))
            -> (St i ea sa) -> f (St i ea' sa')
bothAnnotsS f = annotsS (fmap unGLeft.f.GLeft) (fmap unGRight.f.GRight)
-}

-- annotations, the gory details
annotsE :: Applicative f
        => (ea -> f ea') -> (sa -> f sa') -> Exp i ea sa -> f (Exp i ea' sa')
annotsE f _ (VarE x1 x2)          = VarE     <$> f x1
                                                  <*> pure x2
annotsE f g (DotE x1 x2 x3)       = DotE     <$> f x1
                                                  <*> annotsE f g x2
                                                  <*> pure x3
annotsE f g (IdxE x1 x2 x3)       = IdxE     <$> f x1
                                                  <*> annotsE f g x2
                                                  <*> annotsE f g x3
annotsE f g (BOpE x1 x2 x3 x4)    = BOpE     <$> f x1
                                                  <*> pure x2 -- annotsAO f x2
                                                  <*> annotsE f g x3
                                                  <*> annotsE f g x4
annotsE f g (UOpE x1 x2 x3)       = UOpE     <$> f x1
                                                  <*> pure x2 -- annotsAO f x2
                                                  <*> annotsE f g x3
annotsE f g (AOpE x1 x2 x3 x4)    = AOpE    <$> f x1
                                                  <*> pure x2 -- annotsAO f x2
                                                  <*> annotsLV f g x3
                                                  <*> annotsE f g x4
annotsE f g (UAOpE x1 x2 x3)      = UAOpE    <$> f x1
                                                  <*> pure x2 -- annotsAO f x2
                                                  <*> annotsLV f g x3
annotsE f g (CondE x1 x2 x3 x4)   = CondE    <$> f x1
                                                  <*> annotsE f g x2
                                                  <*> annotsE f g x3
                                                  <*> annotsE f g x4
annotsE f g (AppE x1 x2 x3)       = AppE    <$> f x1
                                                  <*> annotsE f g x2
                                                  <*> traverse (annotsE f g) x3
annotsE f g (FunE x1 x2 x3)          = FunE    <$> f x1 <*> pure x2
                                                  <*> annotsFV f g x3
annotsE f g (NewE x1 x2 x3)       = NewE    <$> f x1
                                                  <*> annotsE f g x2
                                                  <*> traverse (annotsE f g) x3
annotsE f g (CommaE x1 x2)        = CommaE  <$> f x1
                                                  <*> traverse (annotsE f g) x2
annotsE f _ (NullL x)            = NullL   <$> f x
annotsE f _ (ThisL x)            = ThisL   <$> f x
annotsE f _ (NumL x1 x2)          = NumL    <$> f x1
                                                  <*> pure x2
annotsE f _ (StrL x1 x2)          = StrL    <$> f x1
                                                  <*> pure x2
annotsE f _ (RegExpL x1 x2 x3 x4) = RegExpL <$> f x1
                                                  <*> pure x2
                                                  <*> pure x3
                                                  <*> pure x4
annotsE f _ (BoolL x1 x2)         = BoolL   <$> f x1
                                                  <*> pure x2
annotsE f g (ArrL x1 x2)          = ArrL    <$> f x1
                                                  <*> traverse (annotsE f g) x2
annotsE f g (ObjL x1 x2)          = ObjL    <$> f x1
                                                  <*> (traverse . _2) (annotsE f g) x2
annotsE f g (LocE x1 x2)          = LocE x1 <$> annotsE f g x2
annotsE f g (CoE  x1 x2 x3)       = CoE <$> f x1 <*> f x2 <*> annotsE f g x3

annotsS :: Applicative f
        => (ea -> f ea') -> (sa -> f sa') -> St i ea sa -> f (St i ea' sa')
annotsS _ g (DeclS x1 x2)       = DeclS       <$> g x1
                                                      <*> pure x2
annotsS f g (FunS x1 x2 x3)        = FunS        <$> g x1
                                                      <*> pure x2
                                                      <*> annotsFV f g x3
{- annotsS f g (HFunS x1 x2 x3 x4)       = HFunS x1      <$> g x2
                                                      <*> pure x3
                                                      <*> annotsS f g x4 -}
annotsS f g (IfS x1 x2 x3 x4)      = IfS         <$> g x1
                                                      <*> annotsE f g x2
                                                      <*> annotsS f g x3
                                                      <*> annotsS f g x4
annotsS f g (LoopS x1 x2 x3)    = LoopS       <$> g x1 
                                                      <*> annotsLC f g x2
                                                      <*> annotsS f g x3
annotsS f g (SwitchS x1 x2 x3 x4)  = SwitchS     <$> g x1
                                                      <*> annotsE f g x2
                                                      <*> traverse (_2 (annotsS f g)) x3
                                                      <*> annotsS f g x4
{- annotsS f g (HCallS x1 x2 x3 x4)      = HCallS x1     <$> g x2
                                                      <*> annotsE f g x3
                                                      <*> traverse (annotsE f g) x4 -}
annotsS _ g (BreakS x1 x2)            = BreakS      <$> g x1 <*> pure x2
annotsS _ g (ContinueS x1 x2)         = ContinueS   <$> g x1 <*> pure x2
annotsS f g (ExpS x1 x2)           = ExpS        <$> g x1
                                                      <*> annotsE f g x2
annotsS f g (ReturnS x1 x2)        = ReturnS     <$> g x1
                                                      <*> annotsE f g x2
annotsS _ _  EmptyS                   = pure EmptyS
annotsS f g (BlockS x)                = BlockS        <$> traverse (annotsS f g) x
annotsS f g (LabelSA x1 x2 x3)     = LabelSA     <$> g x1
                                                      <*> pure x2
                                                      <*> annotsS f g x3
annotsS f g (SwitchSA x1 x2 x3 x4) = SwitchSA    <$> g x1
                                                      <*> annotsE f g x2
                                                      <*> traverse (\(e,s) -> (,) <$> annotsE f g e <*> annotsS f g s) x3
                                                      <*> annotsS f g x4
--annotsS _ g (ContinueSA x1 x2)     = ContinueSA  <$> g x1
--                                                      <*> pure x2
-- annotsS _ g (BreakSA x1 x2)        = BreakSA    <$> g x1
--                                                      <*> pure x2
annotsS f g (TrySA x1 x2 x3 x4)    = TrySA       <$> g x1
                                                      <*> annotsS f g x2
                                                      <*> traverse (_2 (annotsS f g)) x3
                                                      <*> annotsS f g x4
annotsS f g (ForInSA x1 x2 x3 x4)  = ForInSA     <$> g x1
                                                      <*> annotsLV f g x2
                                                      <*>  annotsE f g x3
                                                      <*> annotsS f g x4
annotsS f g (WithSA x1 x2 x3)      = WithSA    <$> g x1
                                               <*> annotsE f g x2
                                               <*> annotsS f g x3
annotsS f g (ThrowSA x1 x2)        = ThrowSA   <$> g x1
                                               <*> annotsE f g x2
annotsS f g (LocS x1 x2)           = LocS   x1
                                     <$> annotsS f g x2
annotsS f g (CallT x1 x2 x3 x4) = CallT <$> g x1 <*> traverse (annotsLV f g) x2 <*> annotsE f g x3 <*> traverse (annotsE f g) x4

annotsLV :: Applicative f
         => (ea -> f ea') -> (sa -> f sa')
         -> LVal i ea sa -> f (LVal i ea' sa')
annotsLV _ _ (LVar {- x1 -} x2)      = LVar <$> {- f x1 <*> -} pure x2
annotsLV f g (LProp {- x1 -} x2 x3) = LProp <$> {- f x1 <*> -} annotsE f g x2 <*> pure x3
annotsLV f g (LIdx {- x1 -} x2 x3)  = LIdx  <$> {- f x1 <*> -} annotsE f g x2 <*> annotsE f g x3

annotsFV :: Applicative f
         => (ea -> f ea') -> (sa -> f sa')
         -> FunV i ea sa -> f (FunV i ea' sa')
annotsFV f g (FunV x1 x2) = FunV x1 <$> annotsS f g x2

annotsLC :: Applicative f
        => (ea -> f ea') -> (sa -> f sa')
        -> LoopCond i ea sa -> f (LoopCond i ea' sa')
annotsLC f g (CondFor x1 x2 x3) = CondFor <$> traverse (annotsE f g) x1
                                          <*> annotsFI f g x2
                                          <*> traverse (annotsE f g) x3
annotsLC f g (CondBefore x) = CondBefore <$> annotsE f g x
annotsLC f g (CondAfter x)  = CondAfter  <$> annotsE f g x
annotsLC _ _  NoCond        = pure NoCond

annotsFI :: Applicative f
         => (ea -> f ea') -> (sa -> f sa')
         -> ForInit i ea sa -> f (ForInit i ea' sa')
annotsFI _ _ NoInit        = pure NoInit
annotsFI f g (DeclInit xs) = DeclInit <$> traverse (\(i,e) -> (,) i <$> traverse (annotsE f g) e) xs
annotsFI f g (ExpInit x)   = ExpInit <$> annotsE f g x

annotsAO :: Applicative f
         => (ea -> f ea') -> (AnOp ea o) -> f (AnOp ea' o)
annotsAO _ (NAO o)  = pure (NAO o)
annotsAO f (AO a o) = AO <$> f a <*> pure o
