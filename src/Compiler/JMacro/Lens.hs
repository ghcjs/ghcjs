
module Compiler.JMacro.Lens where
import Prelude
import Control.Lens
import Compiler.JMacro.Base

import Data.Map (Map)
import Data.Text (Text)
{-
makeLenses ''JStat
makePrisms ''JStat
-}

_DeclStat :: Prism' JStat Ident
_DeclStat
  = (prism (\ x1_a3DjT -> DeclStat x1_a3DjT))
      (\ x_a3DjV
         -> case x_a3DjV of
              DeclStat y1_a3DjW -> Right y1_a3DjW
              _ -> Left x_a3DjV)
_ReturnStat :: Prism' JStat JExpr
_ReturnStat
  = (prism (\ x1_a3DjX -> ReturnStat x1_a3DjX))
      (\ x_a3DjY
         -> case x_a3DjY of
              ReturnStat y1_a3DjZ -> Right y1_a3DjZ
              _ -> Left x_a3DjY)
_IfStat :: Prism' JStat (JExpr, JStat, JStat)
_IfStat
  = (prism
       (\ (x1_a3Dk0, x2_a3Dk1, x3_a3Dk2)
          -> ((IfStat x1_a3Dk0) x2_a3Dk1) x3_a3Dk2))
      (\ x_a3Dk3
         -> case x_a3Dk3 of
              IfStat y1_a3Dk4 y2_a3Dk5 y3_a3Dk6
                -> Right (y1_a3Dk4, y2_a3Dk5, y3_a3Dk6)
              _ -> Left x_a3Dk3)
_WhileStat :: Prism' JStat (Bool, JExpr, JStat)
_WhileStat
  = (prism
       (\ (x1_a3Dk7, x2_a3Dk8, x3_a3Dk9)
          -> ((WhileStat x1_a3Dk7) x2_a3Dk8) x3_a3Dk9))
      (\ x_a3Dka
         -> case x_a3Dka of
              WhileStat y1_a3Dkb y2_a3Dkc y3_a3Dkd
                -> Right (y1_a3Dkb, y2_a3Dkc, y3_a3Dkd)
              _ -> Left x_a3Dka)
_ForInStat :: Prism' JStat (Bool, Ident, JExpr, JStat)
_ForInStat
  = (prism
       (\ (x1_a3Dke, x2_a3Dkf, x3_a3Dkg, x4_a3Dkh)
          -> (((ForInStat x1_a3Dke) x2_a3Dkf) x3_a3Dkg) x4_a3Dkh))
      (\ x_a3Dki
         -> case x_a3Dki of
              ForInStat y1_a3Dkj y2_a3Dkk y3_a3Dkl y4_a3Dkm
                -> Right (y1_a3Dkj, y2_a3Dkk, y3_a3Dkl, y4_a3Dkm)
              _ -> Left x_a3Dki)
_SwitchStat :: Prism' JStat (JExpr, [(JExpr, JStat)], JStat)
_SwitchStat
  = (prism
       (\ (x1_a3Dkn, x2_a3Dko, x3_a3Dkp)
          -> ((SwitchStat x1_a3Dkn) x2_a3Dko) x3_a3Dkp))
      (\ x_a3Dkq
         -> case x_a3Dkq of
              SwitchStat y1_a3Dkr y2_a3Dks y3_a3Dkt
                -> Right (y1_a3Dkr, y2_a3Dks, y3_a3Dkt)
              _ -> Left x_a3Dkq)
_TryStat :: Prism' JStat (JStat, Ident, JStat, JStat)
_TryStat
  = (prism
       (\ (x1_a3Dku, x2_a3Dkv, x3_a3Dkw, x4_a3Dkx)
          -> (((TryStat x1_a3Dku) x2_a3Dkv) x3_a3Dkw) x4_a3Dkx))
      (\ x_a3Dky
         -> case x_a3Dky of
              TryStat y1_a3Dkz y2_a3DkA y3_a3DkB y4_a3DkC
                -> Right (y1_a3Dkz, y2_a3DkA, y3_a3DkB, y4_a3DkC)
              _ -> Left x_a3Dky)
_BlockStat :: Prism' JStat [JStat]
_BlockStat
  = (prism (\ x1_a3DkD -> BlockStat x1_a3DkD))
      (\ x_a3DkE
         -> case x_a3DkE of
              BlockStat y1_a3DkF -> Right y1_a3DkF
              _ -> Left x_a3DkE)
_ApplStat :: Prism' JStat (JExpr, [JExpr])
_ApplStat
  = (prism (\ (x1_a3DkG, x2_a3DkH) -> (ApplStat x1_a3DkG) x2_a3DkH))
      (\ x_a3DkI
         -> case x_a3DkI of
              ApplStat y1_a3DkJ y2_a3DkK -> Right (y1_a3DkJ, y2_a3DkK)
              _ -> Left x_a3DkI)
_UOpStat :: Prism' JStat (JUOp, JExpr)
_UOpStat
  = (prism (\ (x1_a3DkL, x2_a3DkM) -> (UOpStat x1_a3DkL) x2_a3DkM))
      (\ x_a3DkN
         -> case x_a3DkN of
              UOpStat y1_a3DkO y2_a3DkP -> Right (y1_a3DkO, y2_a3DkP)
              _ -> Left x_a3DkN)
_AssignStat :: Prism' JStat (JExpr, JExpr)
_AssignStat
  = (prism
       (\ (x1_a3DkQ, x2_a3DkR) -> (AssignStat x1_a3DkQ) x2_a3DkR))
      (\ x_a3DkS
         -> case x_a3DkS of
              AssignStat y1_a3DkT y2_a3DkU -> Right (y1_a3DkT, y2_a3DkU)
              _ -> Left x_a3DkS)
_UnsatBlock :: Prism' JStat (IdentSupply JStat)
_UnsatBlock
  = (prism (\ x1_a3DkV -> UnsatBlock x1_a3DkV))
      (\ x_a3DkW
         -> case x_a3DkW of
              UnsatBlock y1_a3DkX -> Right y1_a3DkX
              _ -> Left x_a3DkW)
_AntiStat :: Prism' JStat Text
_AntiStat
  = (prism (\ x1_a3DkY -> AntiStat x1_a3DkY))
      (\ x_a3DkZ
         -> case x_a3DkZ of
              AntiStat y1_a3Dl0 -> Right y1_a3Dl0
              _ -> Left x_a3DkZ)
_LabelStat :: Prism' JStat (JsLabel, JStat)
_LabelStat
  = (prism (\ (x1_a3Dl1, x2_a3Dl2) -> (LabelStat x1_a3Dl1) x2_a3Dl2))
      (\ x_a3Dl3
         -> case x_a3Dl3 of
              LabelStat y1_a3Dl4 y2_a3Dl5 -> Right (y1_a3Dl4, y2_a3Dl5)
              _ -> Left x_a3Dl3)
_BreakStat :: Prism' JStat (Maybe JsLabel)
_BreakStat
  = (prism (\ x1_a3Dl6 -> BreakStat x1_a3Dl6))
      (\ x_a3Dl7
         -> case x_a3Dl7 of
              BreakStat y1_a3Dl8 -> Right y1_a3Dl8
              _ -> Left x_a3Dl7)
_ContinueStat :: Prism' JStat (Maybe JsLabel)
_ContinueStat
  = (prism (\ x1_a3Dl9 -> ContinueStat x1_a3Dl9))
      (\ x_a3Dla
         -> case x_a3Dla of
              ContinueStat y1_a3Dlb -> Right y1_a3Dlb
              _ -> Left x_a3Dla)

{-
makeLenses ''JExpr
makePrisms ''JExpr
-}
_ValExpr :: Prism' JExpr JVal
_ValExpr
  = (prism (\ x1_a3E2p -> ValExpr x1_a3E2p))
      (\ x_a3E2q
         -> case x_a3E2q of
              ValExpr y1_a3E2r -> Right y1_a3E2r
              _ -> Left x_a3E2q)
_SelExpr :: Prism' JExpr (JExpr, Ident)
_SelExpr
  = (prism (\ (x1_a3E2s, x2_a3E2t) -> (SelExpr x1_a3E2s) x2_a3E2t))
      (\ x_a3E2u
         -> case x_a3E2u of
              SelExpr y1_a3E2v y2_a3E2w -> Right (y1_a3E2v, y2_a3E2w)
              _ -> Left x_a3E2u)
_IdxExpr :: Prism' JExpr (JExpr, JExpr)
_IdxExpr
  = (prism (\ (x1_a3E2x, x2_a3E2y) -> (IdxExpr x1_a3E2x) x2_a3E2y))
      (\ x_a3E2z
         -> case x_a3E2z of
              IdxExpr y1_a3E2A y2_a3E2B -> Right (y1_a3E2A, y2_a3E2B)
              _ -> Left x_a3E2z)
_InfixExpr :: Prism' JExpr (JOp, JExpr, JExpr)
_InfixExpr
  = (prism
       (\ (x1_a3E2C, x2_a3E2D, x3_a3E2E)
          -> ((InfixExpr x1_a3E2C) x2_a3E2D) x3_a3E2E))
      (\ x_a3E2F
         -> case x_a3E2F of
              InfixExpr y1_a3E2G y2_a3E2H y3_a3E2I
                -> Right (y1_a3E2G, y2_a3E2H, y3_a3E2I)
              _ -> Left x_a3E2F)
_UOpExpr :: Prism' JExpr (JUOp, JExpr)
_UOpExpr
  = (prism (\ (x1_a3E2J, x2_a3E2K) -> (UOpExpr x1_a3E2J) x2_a3E2K))
      (\ x_a3E2M
         -> case x_a3E2M of
              UOpExpr y1_a3E2N y2_a3E2O -> Right (y1_a3E2N, y2_a3E2O)
              _ -> Left x_a3E2M)
_IfExpr :: Prism' JExpr (JExpr, JExpr, JExpr)
_IfExpr
  = (prism
       (\ (x1_a3E2P, x2_a3E2Q, x3_a3E2R)
          -> ((IfExpr x1_a3E2P) x2_a3E2Q) x3_a3E2R))
      (\ x_a3E2S
         -> case x_a3E2S of
              IfExpr y1_a3E2T y2_a3E2U y3_a3E2V
                -> Right (y1_a3E2T, y2_a3E2U, y3_a3E2V)
              _ -> Left x_a3E2S)
_ApplExpr :: Prism' JExpr (JExpr, [JExpr])
_ApplExpr
  = (prism (\ (x1_a3E2W, x2_a3E2X) -> (ApplExpr x1_a3E2W) x2_a3E2X))
      (\ x_a3E2Y
         -> case x_a3E2Y of
              ApplExpr y1_a3E2Z y2_a3E30 -> Right (y1_a3E2Z, y2_a3E30)
              _ -> Left x_a3E2Y)
_UnsatExpr :: Prism' JExpr (IdentSupply JExpr)
_UnsatExpr
  = (prism (\ x1_a3E31 -> UnsatExpr x1_a3E31))
      (\ x_a3E33
         -> case x_a3E33 of
              UnsatExpr y1_a3E34 -> Right y1_a3E34
              _ -> Left x_a3E33)
_AntiExpr :: Prism' JExpr Text
_AntiExpr
  = (prism (\ x1_a3E35 -> AntiExpr x1_a3E35))
      (\ x_a3E36
         -> case x_a3E36 of
              AntiExpr y1_a3E37 -> Right y1_a3E37
              _ -> Left x_a3E36)

{-
makeLenses ''JVal
makePrisms ''JVal
-}
_JVar :: Prism' JVal Ident
_JVar
  = (prism (\ x1_a3E9F -> JVar x1_a3E9F))
      (\ x_a3E9G
         -> case x_a3E9G of
              JVar y1_a3E9H -> Right y1_a3E9H
              _ -> Left x_a3E9G)
_JList :: Prism' JVal [JExpr]
_JList
  = (prism (\ x1_a3E9I -> JList x1_a3E9I))
      (\ x_a3E9J
         -> case x_a3E9J of
              JList y1_a3E9K -> Right y1_a3E9K
              _ -> Left x_a3E9J)
_JDouble :: Prism' JVal SaneDouble
_JDouble
  = (prism (\ x1_a3E9L -> JDouble x1_a3E9L))
      (\ x_a3E9M
         -> case x_a3E9M of
              JDouble y1_a3E9N -> Right y1_a3E9N
              _ -> Left x_a3E9M)
_JInt :: Prism' JVal Integer
_JInt
  = (prism (\ x1_a3E9O -> JInt x1_a3E9O))
      (\ x_a3E9P
         -> case x_a3E9P of
              JInt y1_a3E9Q -> Right y1_a3E9Q
              _ -> Left x_a3E9P)
_JStr :: Prism' JVal Text
_JStr
  = (prism (\ x1_a3E9R -> JStr x1_a3E9R))
      (\ x_a3E9S
         -> case x_a3E9S of
              JStr y1_a3E9T -> Right y1_a3E9T
              _ -> Left x_a3E9S)
_JRegEx :: Prism' JVal Text
_JRegEx
  = (prism (\ x1_a3E9U -> JRegEx x1_a3E9U))
      (\ x_a3E9V
         -> case x_a3E9V of
              JRegEx y1_a3E9W -> Right y1_a3E9W
              _ -> Left x_a3E9V)
_JHash ::
  Prism' JVal (Map Text JExpr)
_JHash
  = (prism (\ x1_a3E9X -> JHash x1_a3E9X))
      (\ x_a3E9Y
         -> case x_a3E9Y of
              JHash y1_a3E9Z -> Right y1_a3E9Z
              _ -> Left x_a3E9Y)
_JFunc :: Prism' JVal ([Ident], JStat)
_JFunc
  = (prism (\ (x1_a3Ea0, x2_a3Ea1) -> (JFunc x1_a3Ea0) x2_a3Ea1))
      (\ x_a3Ea2
         -> case x_a3Ea2 of
              JFunc y1_a3Ea3 y2_a3Ea4 -> Right (y1_a3Ea3, y2_a3Ea4)
              _ -> Left x_a3Ea2)
_UnsatVal :: Prism' JVal (IdentSupply JVal)
_UnsatVal
  = (prism (\ x1_a3Ea5 -> UnsatVal x1_a3Ea5))
      (\ x_a3Ea6
         -> case x_a3Ea6 of
              UnsatVal y1_a3Ea7 -> Right y1_a3Ea7
              _ -> Left x_a3Ea6)

{-
makeLenses ''Ident
makePrisms ''Ident
-}

_TxtI :: Iso' Ident Text
_TxtI
  = (iso (\ (TxtI x1_a3EgV) -> x1_a3EgV))
      (\ x1_a3EgW -> TxtI x1_a3EgW)