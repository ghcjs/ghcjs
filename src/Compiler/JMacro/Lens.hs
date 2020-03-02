
module Compiler.JMacro.Lens where
import Prelude
import Control.Lens
import Compiler.JMacro.Base

import Data.Map (Map)
import Data.Text (Text)

_DeclStat :: Prism' JStat Ident
_DeclStat = prism DeclStat
                  (\x -> case x of DeclStat y -> Right y
                                   _          -> Left x)

_ReturnStat :: Prism' JStat JExpr
_ReturnStat = prism ReturnStat
                    (\x -> case x of ReturnStat y -> Right y
                                     _            -> Left x)

_IfStat :: Prism' JStat (JExpr, JStat, JStat)
_IfStat = prism (\(x1, x2, x3) -> IfStat x1 x2 x3)
                (\x -> case x of IfStat y1 y2 y3 -> Right (y1, y2, y3)
                                 _               -> Left x)

_WhileStat :: Prism' JStat (Bool, JExpr, JStat)
_WhileStat = prism (\(x1, x2, x3) -> WhileStat x1 x2 x3)
                   (\x -> case x of WhileStat y1 y2 y3 -> Right (y1, y2, y3)
                                    _                  -> Left x)

_ForInStat :: Prism' JStat (Bool, Ident, JExpr, JStat)
_ForInStat =
  prism (\(x1, x2, x3, x4) -> ForInStat x1 x2 x3 x4)
        (\x -> case x of ForInStat y1 y2 y3 y4 -> Right (y1, y2, y3, y4)
                         _                     -> Left x)

_SwitchStat :: Prism' JStat (JExpr, [(JExpr, JStat)], JStat)
_SwitchStat =
  prism (\(x1, x2, x3) -> SwitchStat x1 x2 x3)
        (\x -> case x of SwitchStat y1 y2 y3 -> Right (y1, y2, y3)
                         _                   -> Left x)

_TryStat :: Prism' JStat (JStat, Ident, JStat, JStat)
_TryStat =
  prism (\(x1, x2, x3, x4) -> TryStat x1 x2 x3 x4)
        (\x -> case x of TryStat y1 y2 y3 y4 -> Right (y1, y2, y3, y4)
                         _                   -> Left x)

_BlockStat :: Prism' JStat [JStat]
_BlockStat =
  prism BlockStat
        (\x -> case x of BlockStat y -> Right y
                         _           -> Left x)

_ApplStat :: Prism' JStat (JExpr, [JExpr])
_ApplStat =
  prism (uncurry ApplStat)
        (\x -> case x of ApplStat y1 y2 -> Right (y1, y2)
                         _              -> Left x)

_UOpStat :: Prism' JStat (JUOp, JExpr)
_UOpStat =
  prism (uncurry UOpStat)
        (\x  -> case x of UOpStat y1 y2 -> Right (y1, y2)
                          _             -> Left x)

_AssignStat :: Prism' JStat (JExpr, JExpr)
_AssignStat =
  prism (uncurry AssignStat)
        (\x -> case x of AssignStat y1 y2 -> Right (y1, y2)
                         _                -> Left x)

_UnsatBlock :: Prism' JStat (IdentSupply JStat)
_UnsatBlock =
  prism UnsatBlock
        (\x -> case x of UnsatBlock y -> Right y
                         _            -> Left x)

_LabelStat :: Prism' JStat (JsLabel, JStat)
_LabelStat =
  prism (uncurry LabelStat)
        (\x -> case x of LabelStat y1 y2 -> Right (y1, y2)
                         _               -> Left x)

_BreakStat :: Prism' JStat (Maybe JsLabel)
_BreakStat =
  prism BreakStat
        (\x -> case x of BreakStat y -> Right y
                         _           -> Left x)

_ContinueStat :: Prism' JStat (Maybe JsLabel)
_ContinueStat =
  prism ContinueStat
        (\x -> case x of ContinueStat y -> Right y
                         _              -> Left x)

-- JExpr

_ValExpr :: Prism' JExpr JVal
_ValExpr =
  prism ValExpr
        (\x -> case x of ValExpr y -> Right y
                         _         -> Left x)

_SelExpr :: Prism' JExpr (JExpr, Ident)
_SelExpr =
  prism (uncurry SelExpr)
        (\x -> case x of SelExpr y1 y2 -> Right (y1, y2)
                         _             -> Left x)

_IdxExpr :: Prism' JExpr (JExpr, JExpr)
_IdxExpr =
  prism (uncurry IdxExpr)
        (\x -> case x of IdxExpr y1 y2 -> Right (y1, y2)
                         _             -> Left x)

_InfixExpr :: Prism' JExpr (JOp, JExpr, JExpr)
_InfixExpr =
  prism (\(x1, x2, x3) -> InfixExpr x1 x2 x3)
        (\x -> case x of InfixExpr y1 y2 y3 -> Right (y1, y2, y3)
                         _                  -> Left x)

_UOpExpr :: Prism' JExpr (JUOp, JExpr)
_UOpExpr =
  prism (uncurry UOpExpr)
        (\x -> case x of UOpExpr y1 y2 -> Right (y1, y2)
                         _             -> Left x)

_IfExpr :: Prism' JExpr (JExpr, JExpr, JExpr)
_IfExpr =
  prism (\(x1, x2, x3) -> IfExpr x1 x2 x3)
        (\x -> case x of IfExpr y1 y2 y3 -> Right (y1, y2, y3)
                         _               -> Left x)

_ApplExpr :: Prism' JExpr (JExpr, [JExpr])
_ApplExpr =
  prism (uncurry ApplExpr)
        (\x -> case x of ApplExpr y1 y2 -> Right (y1, y2)
                         _              -> Left x)

_UnsatExpr :: Prism' JExpr (IdentSupply JExpr)
_UnsatExpr
  = prism UnsatExpr
          (\x -> case x of UnsatExpr y -> Right y
                           _           -> Left x)

-- JVal

_JVar :: Prism' JVal Ident
_JVar =
  prism JVar
        (\x -> case x of JVar y -> Right y
                         _      -> Left x)
_JList :: Prism' JVal [JExpr]
_JList =
  prism JList
        ( \x -> case x of JList y -> Right y
                          _       -> Left x)

_JDouble :: Prism' JVal SaneDouble
_JDouble =
  prism JDouble
        (\x -> case x of JDouble y -> Right y
                         _         -> Left x)

_JInt :: Prism' JVal Integer
_JInt =
  prism JInt
        (\x -> case x of JInt y -> Right y
                         _      -> Left x)

_JStr :: Prism' JVal Text
_JStr =
  prism JStr
        (\x -> case x of JStr y -> Right y
                         _      -> Left x)

_JRegEx :: Prism' JVal Text
_JRegEx
  = prism JRegEx
          (\x -> case x of JRegEx y -> Right y
                           _        -> Left x)

_JHash :: Prism' JVal (Map Text JExpr)
_JHash =
  prism JHash
        (\x -> case x of JHash y -> Right y
                         _       -> Left x)

_JFunc :: Prism' JVal ([Ident], JStat)
_JFunc =
  prism (uncurry JFunc)
        (\x -> case x of JFunc y1 y2 -> Right (y1, y2)
                         _           -> Left x)

_UnsatVal :: Prism' JVal (IdentSupply JVal)
_UnsatVal =
  prism UnsatVal
        (\x -> case x of UnsatVal y -> Right y
                         _          -> Left x)

-- Ident

_TxtI :: Iso' Ident Text
_TxtI = iso (\(TxtI x) -> x) TxtI
