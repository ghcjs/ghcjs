{-# LANGUAGE DeriveGeneric, DeriveDataTypeable #-}
{-
  Operators in the JavaScript and Tyr languages and their
  properties. Lower numbers for higher precedence.
 -}
module GHCJS.Tyr.Operators where

import Data.Data
import Data.Typeable
import GHC.Generics

data Assoc
  = AssocLeft
  | AssocRight
  | AssocNone
  deriving (Show, Eq, Ord, Data, Typeable, Generic, Enum)

data BOp
  = EqOp         -- ==
  | StrictEqOp   -- ===
  | NeqOp        -- !=
  | StrictNeqOp  -- !==
  | GtOp         -- >
  | GeOp         -- >=
  | LtOp         -- <
  | LeOp         -- <=
  | AddOp        -- +
  | SubOp        -- -
  | MulOp        -- "*"
  | DivOp        -- /
  | ModOp        -- %
  | LShiftOp     -- <<
  | RShiftOp     -- >>
  | ZRShiftOp    -- >>>
  | BAndOp       -- &
  | BOrOp        -- |
  | BXorOp       -- ^
  | LAndOp       -- &&
  | LOrOp        -- ||
  | InstanceofOp -- instanceof
  | InOp         -- in
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

assocBOp :: BOp -> Assoc
assocBOp _ = AssocLeft

precBOp :: BOp -> Int
precBOp MulOp        = 5
precBOp DivOp        = 5
precBOp ModOp        = 5
precBOp AddOp        = 6
precBOp SubOp        = 6
precBOp LShiftOp     = 7
precBOp RShiftOp     = 7
precBOp ZRShiftOp    = 7
precBOp LtOp         = 8
precBOp LeOp         = 8
precBOp GtOp         = 8
precBOp GeOp         = 8
precBOp InOp         = 8
precBOp InstanceofOp = 8
precBOp EqOp         = 9
precBOp StrictEqOp   = 9
precBOp NeqOp        = 9
precBOp StrictNeqOp  = 9
precBOp BAndOp       = 10
precBOp BXorOp       = 11
precBOp BOrOp        = 12
precBOp LAndOp       = 13
precBOp LOrOp        = 14

isKeywordBOp :: BOp -> Bool
isKeywordBOp InstanceofOp = True
isKeywordBOp InOp         = True
isKeywordBOp _            = False

isShiftOp :: BOp -> Bool
isShiftOp LShiftOp  = True
isShiftOp RShiftOp  = True
isShiftOp ZRShiftOp = True
isShiftOp _         = False

data UOp
  = NotOp           -- !
  | BNotOp          -- ~
  | NegOp           -- -
  | PlusOp          -- +x
  | TypeofOp        -- typeof x
  | DeleteOp        -- delete x
  | YieldOp         -- yield x
  | VoidOp          -- void x
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

assocUOp :: UOp -> Assoc
assocUOp _ = AssocRight

precUOp :: UOp -> Int
precUOp NotOp    = 4
precUOp BNotOp   = 4
precUOp NegOp    = 4
precUOp PlusOp   = 4
precUOp TypeofOp = 4
precUOp DeleteOp = 4
precUOp YieldOp  = 17
precUOp VoidOp   = 4

isKeywordUOp :: UOp -> Bool
isKeywordUOp TypeofOp = True
isKeywordUOp DeleteOp = True
isKeywordUOp YieldOp  = True
isKeywordUOp VoidOp   = True
isKeywordUOp _        = False

data AOp
  = AOp        -- =
  | AAddOp     -- +=
  | ASubOp     -- -=
  | AMulOp     -- *=
  | ADivOp     -- /=
  | AModOp     -- %=
  | ALShiftOp  -- <<=
  | ARShiftOp  -- >>=
  | AZRShiftOp -- >>>=
  | ABAndOp    -- &=
  | ABXorOp    -- ^=
  | ABOrOp     -- |=
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

assocAOp :: AOp -> Assoc
assocAOp _ = AssocRight

precAOp :: AOp -> Int
precAOp _ = 16

data UAOp
  = PreInc          -- ++x
  | PostInc         -- x++
  | PreDec          -- --x
  | PostDec         -- x--
  deriving (Show, Eq, Ord, Enum, Data, Typeable, Generic)

assocUAOp :: UAOp -> Assoc
assocUAOp PreInc  = AssocRight
assocUAOp PostInc = AssocNone
assocUAOp PreDec  = AssocRight
assocUAOp PostDec = AssocNone

precUAOp :: UAOp -> Int
precUAOp PreInc  = 4
precUAOp PostInc = 3
precUAOp PreDec  = 4
precUAOp PostDec = 3

isPrefixUAOp :: UAOp -> Bool
isPrefixUAOp PreInc = True
isPrefixUAOp PreDec = True
isPrefixUAOp _      = False

----------------------------------------------------------------------
-- properties of other operators not in the enums
----------------------------------------------------------------------

maxPrec :: Int
maxPrec = 0

-- conditional expression:  .. ? .. : ..

precCond :: Int
precCond = 15

assocCond :: Assoc
assocCond = AssocRight

-- member access: a.b

precDot :: Int
precDot = 1

assocDot :: Assoc
assocDot = AssocLeft

-- computed member access: a[b]

precIdx :: Int
precIdx = 1

assocIdx :: Assoc
assocIdx = AssocLeft

-- new

precNewNoArgs :: Int
precNewNoArgs = 2

assocNewNoArgs :: Assoc
assocNewNoArgs = AssocRight

precNewWithArgs :: Int
precNewWithArgs = 1

assocNewWithArgs :: Assoc
assocNewWithArgs = AssocNone

-- function calls

precApp :: Int
precApp = 1

assocApp :: Assoc
assocApp = AssocLeft

-- comma

precComma :: Int
precComma = 19

assocComma :: Assoc
assocComma = AssocLeft

-- fixme, spread? ...

