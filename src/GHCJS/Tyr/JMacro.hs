{-
  transitional module for converting JMacro AST to Tyr
  eventually all of JMacro will be replaced
-}
module GHCJS.Tyr.JMacro where

import           Compiler.JMacro.Base

import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           GHCJS.Tyr.Arthur
import           GHCJS.Tyr.Doc
import           GHCJS.Tyr.Operators (BOp, UOp, UAOp)
import qualified GHCJS.Tyr.Operators as O

import qualified Panic

panic :: String -> a
panic = Panic.panic . ("GHCJS.Tyr.JMacro."++)

jstatToArthur :: JStat -> RSt
jstatToArthur (DeclStat i)             = DeclS [RDecl (identToArthur i) Nothing]
jstatToArthur (ReturnStat e)           = ReturnS (Just $ jexprToArthur e)
jstatToArthur (IfStat e s1 s2)         =
  IfS (jexprToArthur e) (jstatToArthur s1) (Just $ jstatToArthur s2)
jstatToArthur (WhileStat False e s)    =
  WhileS (jexprToArthur e) (jstatToArthur s)
jstatToArthur (WhileStat True e s)     =
  DoWhileS (jstatToArthur s) (jexprToArthur e)
jstatToArthur (ForInStat isEach i e s) =
  ForInS (VarI $ identToArthur i) (jexprToArthur e) (jstatToArthur s)
jstatToArthur (SwitchStat e cs def)    =
  SwitchS (jexprToArthur e)
          ((map (\(e,s) -> Case (jexprToArthur e) (jstatToArthur s)) cs) ++
           [Default $ jstatToArthur def])
jstatToArthur (TryStat st i sc sf)     =
  TryS (jstatToArthur st)
       (Just (identToArthur i, jstatToArthur sc))
       (Just (jstatToArthur sf))
jstatToArthur (BlockStat ss)           =
  normalizeRst (BlockS (map jstatToArthur ss))
jstatToArthur (ApplStat e args)        =
  ExpS (jexprToArthur $ ApplExpr e args)
jstatToArthur (UOpStat op e)           =
  ExpS (UOpE (uopToTyr op) (jexprToArthur e))
jstatToArthur (AssignStat e1 e2)       =
  ExpS (AOpE O.AOp (jexprToArthurLVal e1) (jexprToArthur e2))
jstatToArthur (UnsatBlock {})          = panic "jstatToArthur: UnsatBlock"
jstatToArthur (AntiStat {})            = panic "jstatToArthur: AntiStat"
jstatToArthur (LabelStat l s)          = LabelS (RLabel l) (jstatToArthur s)
jstatToArthur (BreakStat ml)           = BreakS (fmap RLabel ml)
jstatToArthur (ContinueStat ml)        = ContinueS (fmap RLabel ml)

jexprToArthur :: JExpr -> RExp
jexprToArthur (ValExpr v)    = jvalToArthur v
jexprToArthur (SelExpr e (TxtI i))    = DotE (jexprToArthur e) (RProp i)
jexprToArthur (IdxExpr e1 e2) = IdxE (jexprToArthur e1) (jexprToArthur e2)
jexprToArthur (InfixExpr op e1 e2)  =
  BOpE (opToTyr op) (jexprToArthur e1) (jexprToArthur e2)
jexprToArthur (UOpExpr NewOp e) =
  case e of
    (ApplExpr e' args) -> NewE (jexprToArthur e') (map jexprToArthur args)
    _                  -> panic "jexprToArthur: invalid NewOp expression"
jexprToArthur (UOpExpr o e)
  | isUAOp o = UAOpE (uaopToTyr o) (jexprToArthurLVal e)
jexprToArthur (UOpExpr op e)    = UOpE (uopToTyr op) (jexprToArthur e)
jexprToArthur (IfExpr e1 e2 e3)     =
  CondE (jexprToArthur e1) (jexprToArthur e2) (jexprToArthur e3)
jexprToArthur (ApplExpr e args) =
  AppE (jexprToArthur e) (map jexprToArthur args)
jexprToArthur (UnsatExpr {}) = panic "jexprToArthur: UnsatExpr"
jexprToArthur (AntiExpr {})  = panic "jexprToArthur: AntiExpr"

jvalToArthur :: JVal -> RExp
jvalToArthur (JVar (TxtI i))   = VarE (RId i)
jvalToArthur (JList [])        = ArrL []
jvalToArthur (JList es)        = ArrL (map jexprToArthur es)
jvalToArthur (JDouble (SaneDouble d))
  | isNaN d          = BOpE O.DivOp (IntL 0) (IntL 0)
  | isNegativeZero d = UOpE O.NegOp (IntL 0)
  | isInfinite d && d < 0 = UOpE O.NegOp (BOpE O.DivOp (IntL 1) (IntL 0))
  | isInfinite d          = BOpE O.DivOp (IntL 1) (IntL 0)
  | otherwise        = RatL (realToFrac d)
jvalToArthur (JInt i)          = IntL i
jvalToArthur (JStr t)          = StrL t
jvalToArthur (JRegEx t)        =
  RegExpL t False False -- is this correct?
jvalToArthur (JHash m)         =
  ObjL (map (\(i, e) -> (IdProp (RId i), jexprToArthur e)) (M.toList m))
jvalToArthur (JFunc args body) =
  FunE Nothing (map (\(TxtI i) -> RId i) args) (jstatToArthur body)
jvalToArthur (UnsatVal {})     = panic "jvalToArthur: UnsatVal"

jexprToArthurLVal :: JExpr -> RLVal
jexprToArthurLVal (ValExpr (JVar (TxtI i))) = LVar (RId i)
jexprToArthurLVal (SelExpr e (TxtI i))      = LProp (jexprToArthur e) (RProp i)
jexprToArthurLVal (IdxExpr e1 e2)           =
  LIdx (jexprToArthur e1) (jexprToArthur e2)
jexprToArthurLVal x                         =
  panic ("jexprToArthurLVal: not an lvalue: " ++ show x)

identToArthur :: Ident -> RId
identToArthur (TxtI t) = RId t

isUAOp :: JUOp -> Bool
isUAOp PreInc  = True
isUAOp PostInc = True
isUAOp PreDec  = True
isUAOp PostDec = True
isUAOp _       = False

uaopToTyr :: JUOp -> UAOp
uaopToTyr PreInc  = O.PreInc
uaopToTyr PreDec  = O.PreDec
uaopToTyr PostInc = O.PostInc
uaopToTyr PostDec = O.PostDec
uaopToTyr o       = panic ("uaopToTyr: " ++ show o)

uopToTyr :: JUOp -> UOp
uopToTyr NotOp    = O.NotOp
uopToTyr BNotOp   = O.BNotOp
uopToTyr NegOp    = O.NegOp
uopToTyr PlusOp   = O.PlusOp
uopToTyr NewOp    = panic "uopToTyr: NewOp"
uopToTyr TypeofOp = O.TypeofOp
uopToTyr DeleteOp = O.DeleteOp
uopToTyr YieldOp  = O.YieldOp
uopToTyr VoidOp   = O.VoidOp
uopToTyr PreInc   = panic "uopToTyr: PreInc"
uopToTyr PostInc  = panic "uopToTyr: PostInc"
uopToTyr PreDec   = panic "uopToTyr: PreDec"
uopToTyr PostDec  = panic "uopToTyr: PostDec"

opToTyr :: JOp -> BOp
opToTyr EqOp          = O.EqOp
opToTyr StrictEqOp    = O.StrictEqOp
opToTyr NeqOp         = O.NeqOp
opToTyr StrictNeqOp   = O.StrictNeqOp
opToTyr GtOp          = O.GtOp
opToTyr GeOp          = O.GeOp
opToTyr LtOp          = O.LtOp
opToTyr LeOp          = O.LeOp
opToTyr AddOp         = O.AddOp
opToTyr SubOp         = O.SubOp
opToTyr MulOp         = O.MulOp
opToTyr DivOp         = O.DivOp
opToTyr ModOp         = O.ModOp
opToTyr LeftShiftOp   = O.LShiftOp
opToTyr RightShiftOp  = O.RShiftOp
opToTyr ZRightShiftOp = O.ZRShiftOp
opToTyr BAndOp        = O.BAndOp
opToTyr BOrOp         = O.BOrOp
opToTyr BXorOp        = O.BXorOp
opToTyr LAndOp        = O.LAndOp
opToTyr LOrOp         = O.LOrOp
opToTyr InstanceofOp  = O.InstanceofOp
opToTyr InOp          = O.InOp
