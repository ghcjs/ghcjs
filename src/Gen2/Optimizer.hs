{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable, QuasiQuotes, NoMonomorphismRestriction #-}
{-
  Optimizer: uses rewrite rules on a simpler intermediate representation
  of function bodies, only a subset of javascript can be represented
  by this

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
optimize = renameLocalVars -- . combineStackUpdates

renameLocalVars :: JStat -> JStat
renameLocalVars = thisFunction . nestedFuns %~ renameLocalsFun

newLocals :: [Ident]
newLocals = map StrI $ (map (:[]) chars0) ++ concatMap mkIdents [1..]
  where
    mkIdents n = [c0:cs | c0 <- chars0, cs <- replicateM n chars]
    chars0 = ['a'..'z']++['A'..'Z']
    chars = chars0++['0'..'9'] 

-- traverse all 'leaf' statements in this function
thisFunction :: Traversal JStat JStat JStat JStat
thisFunction f (IfStat e s1 s2) = IfStat e <$> f s1 <*> f s2
thisFunction f (WhileStat b e s) = WhileStat b e <$> f s
thisFunction f (ForInStat b i e s) = ForInStat b i e <$> f s
thisFunction f (SwitchStat e es s) = SwitchStat e <$> template (thisFunction f) es <*> f s
thisFunction f (TryStat s1 i s2 s3) = TryStat <$> f s1 <*> pure i <*> f s2 <*> f s3
thisFunction f (BlockStat ss) = BlockStat <$> template (thisFunction f) ss
thisFunction f (LabelStat l s) = LabelStat l <$> f s
thisFunction f s = f s

localVars = thisFunction . _DeclStat . _1
localIdents = template . functionVals . _JVar
nestedFuns = template . functionVals . _JFunc

functionVals f (JList es)   = JList <$> template (functionVals f) es
functionVals f (JHash m)    = JHash <$> template (functionVals f) m
functionVals f v            = f v

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
    idents      = S.fromList (s ^.. localIdents ++ args) -- fixme does this scan the whole tree? it should
    locals      = L.nub $ args ++ s ^.. localVars

-- progragate renaming of variables global relative to this function body
renameGlobals :: M.Map Ident Ident -> ([Ident],JStat) -> ([Ident],JStat)
renameGlobals m f@(args,s) = (args, localIdents %~ renameVar $ s')
  where
    (_, s')     = nestedFuns %~ renameGlobals m' $ f
    renameVar i = fromMaybe i (M.lookup i m')
    locals      = L.nub $ args ++ s ^.. localVars
    m'          = m M.\\ (M.fromList $ zip locals (repeat (StrI "_")))


