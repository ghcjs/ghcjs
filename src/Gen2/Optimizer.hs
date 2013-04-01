{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable, QuasiQuotes, NoMonomorphismRestriction #-}
{-
  Optimizer:
  Basic optimization of the generated JavaScript to reduce file size and improve readability

  assumptions:
  - ast is fully saturated
  - no exceptions
-}
module Gen2.Optimizer where

import Control.Monad
import Language.Javascript.JMacro
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Data
import Data.Monoid
import Data.Data.Lens
import Control.Lens
import Control.Arrow
import qualified Data.List as L
import Data.Maybe
import Panic

import Gen2.Utils

import Debug.Trace

optimize :: JStat -> JStat
optimize = removeDeadVars . propagateConstants . renameLocalVars -- . combineStackUpdates

renameLocalVars :: JStat -> JStat
renameLocalVars = thisFunction . nestedFuns %~ renameLocalsFun

newLocals :: [Ident]
newLocals = map StrI $ (map (:[]) chars0) ++ concatMap mkIdents [1..]
  where
    mkIdents n = [c0:cs | c0 <- chars0, cs <- replicateM n chars]
    chars0 = ['a'..'z']++['A'..'Z']
    chars = chars0++['0'..'9'] 

-- traverse all expressions in the statement (no recursion into nested statements)
statExprs :: Traversal' JStat JExpr
statExprs f (ReturnStat e) = ReturnStat <$> f e
statExprs f (IfStat e s1 s2) = IfStat <$> f e <*> pure s1 <*> pure s2
statExprs f (ForInStat b i e s) = ForInStat b i <$> f e <*> pure s
statExprs f (SwitchStat e es s) = SwitchStat <$> f e <*> (traverse . _1) f es <*> pure s
statExprs f (ApplStat e1 es) = ApplStat <$> f e1 <*> traverse f es
statExprs f (PPostStat b o e) = PPostStat b o <$> f e
statExprs f (AssignStat e1 e2) = AssignStat <$> f e1 <*> f e2
statExprs _ s = pure s

subStats :: Traversal' JStat JStat
subStats f (IfStat e s1 s2) = IfStat e <$> f s1 <*> f s2
subStats f (WhileStat b e s) = WhileStat b e <$> f s
subStats f (ForInStat b i e s) = ForInStat b i e <$> f s
subStats f (SwitchStat e es s) = SwitchStat e <$> (traverse . _2) f es <*> f s
subStats f (TryStat s1 i s2 s3) = TryStat <$> f s1 <*> pure i <*> f s2 <*> f s3
subStats f (BlockStat ss) = BlockStat <$> traverse f ss
subStats f (LabelStat l s) = LabelStat l <$> f s
subStats _ s = pure s

-- traverse all 'leaf' statements in this function
thisFunction :: Traversal' JStat JStat
thisFunction f (IfStat e s1 s2) = IfStat e <$> thisFunction f s1
                                           <*> thisFunction f s2
thisFunction f (WhileStat b e s) = WhileStat b e <$> thisFunction f s
thisFunction f (ForInStat b i e s) = ForInStat b i e <$> thisFunction f s
thisFunction f (SwitchStat e es s) = SwitchStat e <$> (traverse . _2 . thisFunction) f es
                                                  <*> thisFunction f s
thisFunction f (TryStat s1 i s2 s3) = TryStat <$> thisFunction f s1
                                              <*> pure i
                                              <*> thisFunction f s2
                                              <*> thisFunction f s3
thisFunction f (BlockStat ss) = BlockStat <$> template (thisFunction f) ss
thisFunction f (LabelStat l s) = LabelStat l <$> thisFunction f s
thisFunction f s = f s

localVars = thisFunction . _DeclStat . _1
localIdents = template . localFunctionVals . _JVar
allIdents = template . functionVals . _JVar
nestedFuns = template . localFunctionVals . _JFunc

functionVals f (JList es)      = JList <$> template (functionVals f) es
functionVals f (JHash m)       = JHash <$> template (functionVals f) m
functionVals f v@(JFunc as es) = JFunc as <$> template (functionVals f) es
functionVals f v               = f v

localFunctionVals f (JList es)   = JList <$> template (localFunctionVals f) es
localFunctionVals f (JHash m)    = JHash <$> template (localFunctionVals f) m
localFunctionVals f v            = f v

renameLocalsFun :: ([Ident], JStat) -> ([Ident], JStat)
renameLocalsFun f = ( map renameVar args
                    , s' & localIdents %~ renameVar & localVars %~ renameVar
                    )
  where
    (_,   s')   = nestedFuns %~ renameGlobals rename $ (args, s)
    (args, s)   = nestedFuns %~ renameLocalsFun $ f
    renameVar i = fromMaybe i (M.lookup i rename)
    rename      = M.fromList $ zip locals (filter (`S.notMember` globals) newLocals)
    globals     = idents S.\\ (S.fromList locals)
    idents      = S.fromList (s ^.. allIdents ++ args)
    locals      = L.nub $ args ++ s ^.. localVars

-- progragate renaming of variables global relative to this function body
renameGlobals :: M.Map Ident Ident -> ([Ident],JStat) -> ([Ident],JStat)
renameGlobals m f@(args,s) = (args, localIdents %~ renameVar $ s')
  where
    (_, s')     = nestedFuns %~ renameGlobals m' $ f
    renameVar i = fromMaybe i (M.lookup i m')
    locals      = L.nub $ args ++ s ^.. localVars
    m'          = m M.\\ (M.fromList $ zip locals (repeat (StrI "_")))


------------------------------------------------------
-- simple forward propagation of constants and var assignments
------------------------------------------------------

propagateConstants :: JStat -> JStat
propagateConstants = s & thisFunction . nestedFuns . _2 %~ propagateConstants'

propagateConstants' :: JStat -> JStat
propagateConstants' s = id

-----------------------------------------------------
-- dead variable elimination:
--  a dead var is a local variable that is never read
-----------------------------------------------------

removeDeadVars s = s & thisFunction . nestedFuns . _2 %~ removeDeadVars'

removeDeadVars' :: JStat -> JStat
removeDeadVars' s = transformOf subStats (removeDead dead) s
  where
    dead    = locals S.\\ nonDead
    nonDead = S.fromList $ universeOf subStats s ^.. traverse . readVarsS
    locals  = S.fromList $ s ^.. localVars

removeDead :: S.Set Ident -> JStat -> JStat
removeDead dead (AssignStat (ValExpr (JVar i)) _)
   | i `S.member` dead = mempty
removeDead dead (DeclStat i _)
   | i `S.member` dead = mempty
removeDead _ s = s

readVarsS :: Traversal' JStat Ident
readVarsS f (AssignStat v@(ValExpr {}) e) = AssignStat v <$> readVarsE f e
readVarsS f s = (statExprs . readVarsE) f s

readVarsE :: Traversal' JExpr Ident
readVarsE f (ValExpr (JVar i))     = ValExpr . JVar <$> f i
readVarsE f v@(ValExpr (JFunc {})) = pure v
readVarsE f e                      = template (readVarsE f) e

