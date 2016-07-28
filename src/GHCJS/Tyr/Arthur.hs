{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, ViewPatterns #-}
{-
   the Tyr AST, defined in Tyr.Base AST is kept as simple as possible
   to make optimization easier, and supports GHCJS-specific extensions,
   which allow the choice of the JavaScript representation for some operations
   to be deferred until after the optimization passes.

   To render JavaScript, Tyr is first converted to the renderable Arthur
   AST defined here, by choosing a concrete JavaScript representation for
   all GHCJS-specific extensions.
 -}

module GHCJS.Tyr.Arthur where

import GHC.Generics

import Control.Lens.Plated

import Data.Data
import Data.Data.Lens
import Data.Text (Text)
import Data.Typeable

import qualified GHCJS.Tyr.Base as B
import GHCJS.Tyr.Base (L(..))
import GHCJS.Tyr.Operators


data RSt
  = BlockS    [RSt]
  | EmptyS
  | ExpS      !RExp
  | IfS       !RExp !RSt (Maybe RSt)
  | SwitchS   !RExp [RCase]
  | WhileS    !RExp !RSt
  | DoWhileS  !RSt !RExp
  | BreakS    (Maybe RLabel)
  | ContinueS (Maybe RLabel)
  | LabelS    !RLabel !RSt
  | ForInS    !RForInI !RExp !RSt
  | ForS      !RForI (Maybe RExp) (Maybe RExp) !RSt
  | TryS      !RSt (Maybe (RId,RSt)) (Maybe RSt)
  | ThrowS    !RExp
  | ReturnS   (Maybe RExp)
  | WithS     !RExp !RSt
  | DeclS     [RDecl]
  | FunS      !RId [RId] !RSt -- do we need a list? a (Id a) [Id a] [Statement a]
  | LocS      !L !RSt
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data RExp
  = StrL    !Text
  | RegExpL !Text !Bool !Bool
  | IntL    !Integer
  | RatL    !Rational
  | BoolL   !Bool
  | ArrL    [RExp]
  | NullL
  | ObjL    [(RObjLPropName,RExp)]
  | ThisL
  | VarE    !RId
  | DotE    !RExp !RProp
  | IdxE    !RExp !RExp
  | NewE    !RExp [RExp]
  | BOpE    !BOp !RExp !RExp
  | UOpE    !UOp !RExp
  | AOpE    !AOp !RLVal !RExp
  | UAOpE   !UAOp !RLVal
  | CondE   !RExp !RExp !RExp
  | CommaE  [RExp]
  | AppE    !RExp [RExp]
  | FunE    (Maybe RId) [RId] !RSt -- do we need multiple st?
  | LocE    !L !RExp
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype RId    = RId Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype RLabel = RLabel Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

newtype RProp  = RProp Text
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data RObjLPropName
  = StrProp !Text
  | IdProp  !RId
  | NumProp !RNumL
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data RLVal
  = LVar !RId
  | LProp !RExp !RProp
  | LIdx  !RExp !RExp
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

-- fixme, change to number?
data RNumL
  = RIntL !Integer
  | RRatL !Rational
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data RForInI
  = VarI  !RId
  | LValI !RLVal
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data RForI
  = NoI
  | DeclI [RDecl]
  | ExpI  !RExp
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data RDecl = RDecl !RId (Maybe RExp)
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

data RCase
  = Case !RExp !RSt -- fixme multiple stats?
  | Default !RSt -- fixme multiple stats?
  deriving (Eq, Ord, Show, Data, Typeable, Generic)

normalize :: RSt -> RSt
normalize = transformOf template normalizeRst

normalizeRst :: RSt -> RSt
normalizeRst (BlockS xs) =
  case normalizeBlock (flattenBlocks xs) of
    []  -> EmptyS
    [x] -> x
    xs' -> BlockS xs'
normalizeRst x = x

unlocS :: RSt -> RSt
unlocS (LocS _ s)   = unlocS s
unlocS (BlockS [x]) = unlocS x
unlocS x            = x

unlocE :: RExp -> RExp
unlocE (LocE _ e) = unlocE e
unlocE x          = x

normalizeBlock :: [RSt] -> [RSt]
normalizeBlock [] = []
normalizeBlock xs@(x:_) | DeclS{} <- unlocS x = normalizeDecls [] xs
normalizeBlock (x:xs)                         = x : normalizeBlock xs

isAssignExp :: RId -> RExp -> Maybe RExp
isAssignExp i (unlocE -> AOpE AOp (LVar j) e)
  | i == j = Just e
isAssignExp _ _ = Nothing

normalizeDecls :: [RDecl] -> [RSt] -> [RSt]
normalizeDecls ds (s1:s2:xs) -- (unlocS -> DeclS [RDecl x Nothing] Nothing:unlocS -> ExpS e:xs)
  | DeclS [RDecl x Nothing] <- unlocS s1,
    ExpS e <- unlocS s2,
    Just rhs <- isAssignExp x e = normalizeDecls (RDecl x (Just rhs):ds) xs
normalizeDecls ds (s:xs)
  | DeclS ds' <- unlocS s = normalizeDecls (reverse ds' ++ ds) xs
normalizeDecls ds xs
  | null ds   = normalizeBlock xs
  | otherwise = DeclS (reverse ds) : normalizeBlock xs

flattenBlocks :: [RSt] -> [RSt]
flattenBlocks (BlockS xs:ys) = flattenBlocks xs ++ flattenBlocks ys
flattenBlocks (EmptyS:xs)    = flattenBlocks xs
flattenBlocks (LocS l s:xs)  = flattenLocs l s ++ flattenBlocks xs
flattenBlocks (x:xs)         = x : flattenBlocks xs
flattenBlocks []             = []

flattenLocs :: L -> RSt -> [RSt]
flattenLocs _ (LocS l s)  = flattenLocs l s
flattenLocs l (BlockS xs) = flattenBlocks (map (LocS l) xs)
flattenLocs _ EmptyS      = []
flattenLocs l s           = [LocS l s]

-- fixme this is a hack
isBraceS :: RSt -> Bool
isBraceS (BlockS xs)
  | null xs'   = False
  | [x] <- xs' = isBraceS x
  | otherwise  = True
  where xs' = flattenBlocks xs
isBraceS (LocS _ s) = isBraceS s
isBraceS _          = False

isIfNoElseStat :: RSt -> Bool
isIfNoElseStat (LocS _ s)         = isIfNoElseStat s
isIfNoElseStat (LabelS _ s)       = isIfNoElseStat s
isIfNoElseStat (BlockS xs)
  | [x] <- flattenBlocks xs       = isIfNoElseStat x
isIfNoElseStat (IfS _ _ Nothing)  = True
isIfNoElseStat (IfS _ _ (Just e)) = isIfNoElseStat e
isIfNoElseStat (WhileS _ s)       = isIfNoElseStat s
isIfNoElseStat (ForS _ _ _ s)     = isIfNoElseStat s
isIfNoElseStat (ForInS _ _ s)     = isIfNoElseStat s
isIfNoElseStat (WithS _ s)        = isIfNoElseStat s
isIfNoElseStat _                  = False

isIfStat :: RSt -> Bool
isIfStat (LocS _ s)         = isIfStat s
isIfStat (BlockS xs)
  | [x] <- flattenBlocks xs = isIfStat x
isIfStat (IfS{})            = True
isIfStat _                  = False

