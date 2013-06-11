{-# LANGUAGE CPP, GADTs, StandaloneDeriving, DeriveDataTypeable, QuasiQuotes, NoMonomorphismRestriction, TupleSections #-}
{-
  Optimizer:
  Basic optimization of the generated JavaScript to reduce file size and improve readability

  assumptions:
  - ast is fully saturated
-}
module Gen2.Optimizer where

import Control.Monad
import Language.Javascript.JMacro
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import Data.Int
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Data
import Data.Monoid
import Data.Data.Lens
import Data.Tree
import Control.Lens
import Control.Arrow
import Data.List (foldl')
import qualified Data.List as L
import Data.Maybe
import Panic
import Control.Monad.State
import Data.Bits
import Data.Default

import           Gen2.Utils
import           Gen2.Dataflow

import Debug.Trace

optimize :: JStat -> JStat
#ifdef DISABLE_OPTIMIZER
optimize = id
#else
optimize = renameLocalVars . removeDeadVars . dataflow
#endif

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

subExprs :: Traversal' JExpr JExpr
subExprs f (SelExpr e i) = SelExpr <$> f e <*> pure i
subExprs f (IdxExpr e1 e2) = IdxExpr <$> f e1 <*> f e2
subExprs f (InfixExpr o e1 e2) = InfixExpr o <$> f e1 <*> f e2
subExprs f (PPostExpr b xs e) = PPostExpr b xs <$> f e
subExprs f (IfExpr e1 e2 e3) = IfExpr <$> f e1 <*> f e2 <*> f e3
subExprs f (NewExpr e) = NewExpr <$> f e
subExprs f (ApplExpr e es) = ApplExpr <$> f e <*> traverse f es
subExprs f (TypeExpr b e t) = TypeExpr b <$> f e <*> pure t
subExprs f e = f e

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

localVars :: JStat -> [Ident]
localVars = universeOf subStats >=> getLocals
  where
    getLocals (DeclStat i _) = [i]
    getLocals (ForInStat _ i _ _) = [i]
    getLocals (TryStat _ i _ _) = [i]
    getLocals _ = []

localIdents = template . localFunctionVals . _JVar
allIdents = template . functionVals . _JVar
nestedFuns = template . localFunctionVals . _JFunc

-- all idents not in expressions in this function, including in declarations
nonExprLocalIdents :: Traversal' JStat Ident
nonExprLocalIdents f (IfStat e s1 s2) = IfStat e <$> nonExprLocalIdents f s1
                                                 <*> nonExprLocalIdents f s2
nonExprLocalIdents f (DeclStat i e)   = DeclStat <$> f i <*> pure e
nonExprLocalIdents f (WhileStat b e s) = WhileStat b e <$> nonExprLocalIdents f s
nonExprLocalIdents f (ForInStat b i e s) = ForInStat b <$> f i
                                                       <*> pure e
                                                       <*> nonExprLocalIdents f s
nonExprLocalIdents f (SwitchStat e es s) = SwitchStat e <$> (traverse . _2 . nonExprLocalIdents) f es
                                                        <*> nonExprLocalIdents f s
nonExprLocalIdents f (TryStat s1 i s2 s3) = TryStat <$> nonExprLocalIdents f s1
                                                    <*> f i
                                                    <*> nonExprLocalIdents f s2
                                                    <*> nonExprLocalIdents f s3
nonExprLocalIdents f (BlockStat ss) = BlockStat <$> (traverse . nonExprLocalIdents) f ss
nonExprLocalIdents f (LabelStat l s) = LabelStat l <$> nonExprLocalIdents f s
nonExprLocalIdents f s = pure s


functionVals f (JList es)      = JList <$> template (functionVals f) es
functionVals f (JHash m)       = JHash <$> tinplate (functionVals f) m
functionVals f v@(JFunc as es) = JFunc as <$> template (functionVals f) es
functionVals f v               = f v

localFunctionVals f (JList es)   = JList <$> template (localFunctionVals f) es
localFunctionVals f (JHash m)    = JHash <$> tinplate (localFunctionVals f) m  -- lens bug?
localFunctionVals f v            = f v

renameLocalsFun :: ([Ident], JStat) -> ([Ident], JStat)
renameLocalsFun f = ( map renameVar args
                    , s' & localIdents %~ renameVar & nonExprLocalIdents %~ renameVar
                    )
  where
    (_,   s')   = nestedFuns %~ renameGlobals rename $ (args, s)
    (args, s)   = nestedFuns %~ renameLocalsFun $ f
    renameVar i = fromMaybe i (M.lookup i rename)
    rename      = M.fromList $ zip locals (filter (`S.notMember` globals) newLocals)
    globals     = idents S.\\ (S.fromList locals)
    idents      = S.fromList (s ^.. allIdents ++ args)
    locals      = L.nub $ localVars s -- args ++ s ^.. localVars

-- propagate renaming of variables global relative to this function body
renameGlobals :: M.Map Ident Ident -> ([Ident],JStat) -> ([Ident],JStat)
renameGlobals m f@(args,s) = (args, localIdents %~ renameVar $ s')
  where
    (_, s')     = nestedFuns %~ renameGlobals m' $ f
    renameVar i = fromMaybe i (M.lookup i m')
    locals      = L.nub $ args ++ localVars s
    m'          = m M.\\ (M.fromList $ zip locals (repeat (StrI "_")))

tup :: a -> (a,a)
tup x = (x,x)

-----------------------------------------------------
-- dead variable elimination:
--  a dead var is a local variable that is never read or assigned
-----------------------------------------------------

removeDeadVars s = s & thisFunction . nestedFuns . _2 %~ removeDeadVars'

removeDeadVars' :: JStat -> JStat
removeDeadVars' s = transformOf subStats (removeDead dead) s
  where
    dead    = locals S.\\ nonDead
    nonDead = S.fromList (universeOf subStats s ^.. traverse . liveVarsS)
    locals  = S.fromList (localVars s)

removeDead :: S.Set Ident -> JStat -> JStat
removeDead dead (DeclStat i _)
   | i `S.member` dead = mempty
removeDead _ s = s

liveVarsS :: Traversal' JStat Ident
liveVarsS f s = (statExprs . liveVarsE) f s

liveVarsE :: Traversal' JExpr Ident
liveVarsE f (ValExpr (JVar i))     = ValExpr . JVar <$> f i
liveVarsE f v@(ValExpr (JFunc {})) = pure v
liveVarsE f e                      = template (liveVarsE f) e

----------------------------------------------------------
dataflow :: JStat -> JStat
dataflow = thisFunction . nestedFuns %~ f
  where f (args,stat)     = g stat . liveness . constants . constants . constants . propagateStack $ (args, localVars stat, cfg stat)
        g _ (args,_,cfg) = (args, unCfg cfg)
--        g stat (args,_,cfg) = (args, unCfg cfg <> [j| return; |]  <> stat)  -- append original, debug!

-----------------------------------------------------------
-- liveness: remove unnecessary assignments
-----------------------------------------------------------

liveness :: ([Ident],[Ident],Graph) -> ([Ident],[Ident],Graph)
liveness (args,locals,g) = (args,locals,g')
  where
    locals' = S.fromList locals
    lf = livenessFacts g
    g' = (nodes %~ IM.mapWithKey updNode) g
    updNode :: NodeId -> Node -> Node
    updNode nid (SimpleNode (AssignStat (ValExpr (JVar i)) e))
      | Just live <- lookupFact nid 0 lf, i `M.notMember` live 
         && not (mightHaveSideEffects e) 
         && i `S.member` locals' = SequenceNode []
    updNode _  n = n

type Liveness = Map Ident Use

data Use = Once | Multiple deriving (Show, Eq, Bounded, Ord, Enum)

livenessFacts :: Graph -> Facts Liveness
livenessFacts = foldBackward (M.unionWith lMax) f M.empty
  where
    f _ (SimpleNode (AssignStat (ValExpr (JVar i)) e)) m
       = M.unionWith lPlus (M.delete i m) (exprL e)
    f _ n m = M.unionWith lPlus (exprL n) m
    exprL n = M.fromList (map (,Once) $ n ^.. template)


lPlus :: Use -> Use -> Use
lPlus _ _ = Multiple

lMax :: Use -> Use -> Use
lMax Once Once = Once
lMax _    _    = Multiple

-----------------------------------------------------------
-- constants and assignment propagation
--   removes unreachable branches and statements
-----------------------------------------------------------

type CVals = Map Ident (JExpr, Set Ident) -- ident = expr, depends on set of idents

constants :: ([Ident],[Ident],Graph) -> ([Ident],[Ident],Graph)
constants (args,locals,g) = -- trace (show g) $ trace (show fs) 
                            (args, locals, g')
  where
    fs  = constantsFacts lfs g
    lfs = livenessFacts g
    g'  = (nodes %~ IM.mapWithKey updNode) g

    -- fact index 2 is what flows into the expression
    nodeFact :: NodeId -> CVals
    nodeFact nid = fromMaybe M.empty (lookupFact nid 2 fs)

    updNode :: NodeId -> Node -> Node
    updNode nid = rewriteNode nid . rewriteExprs (nodeFact nid)

    rewriteExprs :: CVals -> Node -> Node
    rewriteExprs cv = template %~ (normalize . propagateExpr cv)

    updExpr :: CVals -> JExpr -> JExpr
    updExpr fact e = normalize . propagateExpr fact $ e

    rewriteNode :: NodeId -> Node -> Node
    rewriteNode nid _
      | isNothing (lookupFact nid 0 fs) = SequenceNode [] -- unreachable code
    rewriteNode nid (IfNode e s1 s2)
      | Just True  <- c, not s = SequenceNode [s1] -- else branch unreachable
      | Just False <- c, not s = SequenceNode [s2] -- if branch unreachable
      where
        s  = mightHaveSideEffects e'
        c  = exprCond e'
        e' = updExpr (nodeFact nid) e
    rewriteNode nid (WhileNode e s1)  -- loop body unreachable
      | Just False <- c, not (mightHaveSideEffects e') = SequenceNode []
      where
        c  = exprCond e'
        e' = updExpr (nodeFact nid) e
    rewriteNode nid (SimpleNode (ApplStat s args)) = 
      case propagateExpr (nodeFact nid) (ApplExpr s args) of
        (ApplExpr s' args') -> SimpleNode (ApplStat s' args')
        _ -> error "rewriteNode: not ApplyExpr"
    rewriteNode nid n = rewriteExprs (nodeFact nid) n

    -- apply our facts to an expression
    propagateExpr :: CVals -> JExpr -> JExpr
    propagateExpr cv e@(ApplExpr (ValExpr _) args) =
      propagateExpr' (maybe M.empty (`deleteConstants` cv) (S.unions <$> mapM mutated args)) e
    propagateExpr cv e = 
      propagateExpr' (maybe M.empty (`deleteConstants` cv) (mutated e)) e
    propagateExpr' :: CVals -> JExpr -> JExpr
    propagateExpr' cv e = transformOf template f e
      where
        f :: JExpr -> JExpr
        f (ValExpr (JVar i)) | Just (e,_) <- M.lookup i cv = e
        f x = x

-- constant and variable assignment propagation
constantsFacts :: Facts Liveness -> Graph -> Facts CVals
constantsFacts lfs g = foldForward combineConstants f0 M.empty g
  where
    f0 :: Forward CVals
    f0 = def { fIf      = removeMutated1t
             , fWhile   = removeMutated1t
             , fDoWhile = removeMutated1t
             , fSwitch  = switched
             , fReturn  = removeMutated1
             , fForIn   = removeMutated3
             , fSimple  = \nid s m -> simple nid s (removeMutated s m)
             , fTry     = \_ x -> x
             }
    usedOnce :: NodeId -> Ident -> Bool
    usedOnce nid ident = fromMaybe False $
      (== Once) <$> (M.lookup ident =<< lookupFact nid 0 lfs)

    switched :: NodeId -> JExpr -> [JExpr] -> CVals -> ([CVals], CVals)
    switched _ e es m = let m' = removeMutated e m in (replicate (length es) m', m')
    simple :: NodeId -> JStat -> CVals -> CVals
    -- fixme remove ugly ReturnStat hack
    simple nid (ApplStat fn es) m = removeMutated (ReturnStat (ApplExpr fn es)) m
    simple nid (PPostStat b op expr) m = removeMutated (ReturnStat (PPostExpr b op expr)) m
    simple nid (AssignStat (ValExpr (JVar i)) e@(ValExpr (JInt _))) m =
       M.insert i (e, S.empty) (dc i m)
    simple nid (AssignStat (ValExpr (JVar i)) e@(ValExpr (JDouble _))) m =
       M.insert i (e, S.empty) (dc i m)
    simple nid (AssignStat (ValExpr (JVar i)) e@(ValExpr (JVar j))) m =
       M.insert i (e, S.singleton j) (dc i m)
    simple nid (AssignStat (ValExpr (JVar i)) e@(ApplExpr (ValExpr (JVar fun)) exprs)) m
       | isKnownPureFun fun && usedOnce nid i && i `S.notMember` argDeps = M.insert i (e,argDeps) (dc i m)
          where argDeps = S.unions (map exprDeps exprs)
    simple nid (AssignStat (ValExpr (JVar i)) e) m
       | not (mightHaveSideEffects e) && usedOnce nid i && i `S.notMember` exprDeps e
           = M.insert i (e, exprDeps e) (dc i m)
    simple nid (AssignStat (ValExpr (JVar i)) e@(SelExpr (ValExpr (JVar j)) field)) m
       | usedOnce nid i = M.insert i (e, S.singleton j) m
    simple nid (AssignStat (ValExpr (JVar i)) _) m = dc i m
    simple nid (AssignStat (IdxExpr (ValExpr (JVar i)) _) _) m = dc i m
    simple nid (AssignStat (SelExpr (ValExpr (JVar i)) _) _) m = dc i m
    simple _ _ m = m
    tup x = (x,x)
    removeMutated1t _ s  = tup . removeMutated s
    removeMutated1 _     = removeMutated
    removeMutated3 _ _ _ = removeMutated
    removeMutated s m = maybe M.empty (`deleteConstants` m) mut
      where
        mut = S.unions <$> sequence (Just S.empty : map mutated (s ^.. template))

    dc :: Ident -> CVals -> CVals
    dc i m = deleteConstants (S.singleton i) m

    -- propagate constants if we have the same from both branches
    combineConstants :: CVals -> CVals -> CVals
    combineConstants c1 c2 = M.filterWithKey f c1
      where
        f i v = M.lookup i c2 == Just v

-- all variables referenced by expression
exprDeps :: JExpr -> Set Ident
exprDeps e = S.fromList $ (e ^.. tinplate) >>= ids
  where
    ids (JVar x) = [x]
    ids _        = []

deleteConstants :: Set Ident -> CVals -> CVals
deleteConstants s m = M.filterWithKey f m
  where
    f k _       | k `S.member` s                       = False
    f _ (_, ds) | not . S.null $ ds `S.intersection` s = False
    f _ _                                              = True

-- returns Nothing if it does things that we aren't sure of
mutated :: JExpr -> Maybe (Set Ident)
mutated expr = foldl' f (Just S.empty) (universeOf tinplate expr)
   where
    f Nothing _                            = Nothing
    f s (PPostExpr _ _ (ValExpr (JVar i))) = S.insert i <$> s
    f s (ApplExpr (ValExpr (JVar i)) _)
      | Just m <- knownMutated i           = S.union m <$> s
    f s (SelExpr {})                       = s -- fixme might not be true with properties
    f s (InfixExpr{})                      = s
    f s (IdxExpr{})                        = s
    f s (NewExpr{})                        = s
    f s (ValExpr{})                        = s
    f _ _                                  = Nothing
    knownMutated i
      | isKnownPureFun i               = Just S.empty
      | Just s <- M.lookup i knownFuns = Just s
    knownMutated _ = Nothing

mutated' :: Data a => a -> Maybe (Set Ident)
mutated' a = S.unions <$> (mapM mutated $ a ^.. tinplate)

-- common RTS functions that we know touch only a few global variables
knownFuns :: Map Ident (Set Ident)
knownFuns = M.fromList . map (StrI *** S.fromList . map StrI) $
               [ ("h$bh", [ "h$r1" ])
               ] ++ map (\n -> ("h$p"  ++ show n, ["h$stack", "h$sp"])) [(1::Int)..32]
                 ++ map (\n -> ("h$pp" ++ show n, ["h$stack", "h$sp"])) [(1::Int)..255]

-- pure RTS functions that we know we can safely move around or remove
isKnownPureFun :: Ident -> Bool
isKnownPureFun (StrI i) = S.member i knownPure
  where knownPure = S.fromList . map ("h$"++) $
          ("c" : map (('c':).show) [(1::Int)..32]) ++
          map (('d':).show) [(1::Int)..32]


mightHaveSideEffects :: JExpr -> Bool
mightHaveSideEffects e = any f (universeOf template e)
  where
    f (ValExpr{})          = False
    f (SelExpr{})          = False  -- might be wrong with defineProperty
    f (IdxExpr{})          = False
    f (InfixExpr{})        = False
    f (PPostExpr _ "++" _) = True
    f (PPostExpr _ "--" _) = True
    f (PPostExpr{})        = False
    f (IfExpr{})           = False
    f (NewExpr{})          = False
    f (ApplExpr (ValExpr (JVar i)) _) | isKnownPureFun i = False
    f _                    = True

-- constant folding and other bottom up rewrites
foldExpr :: JExpr -> JExpr
foldExpr = transformOf template f
  where
    f (InfixExpr "||" v1 v2) = lorOp v1 v2
    f (InfixExpr "&&" v1 v2) = landOp v1 v2
    f (InfixExpr op (ValExpr (JInt i1)) (ValExpr (JInt i2)))       = intInfixOp op i1 i2
    f (InfixExpr op (ValExpr (JDouble d1)) (ValExpr (JDouble d2))) = doubleInfixOp op d1 d2
    f (InfixExpr op (ValExpr (JInt x)) (ValExpr (JDouble d)))
      | abs x < 10000 = doubleInfixOp op (fromIntegral x) d -- conservative estimate to prevent loss of precision?
    f (InfixExpr op (ValExpr (JDouble d)) (ValExpr (JInt x)))
      | abs x < 10000 = doubleInfixOp op d (fromIntegral x)
    f (InfixExpr "+" (ValExpr (JStr s1)) (ValExpr (JStr s2))) = ValExpr (JStr (s1++s2))
    f (InfixExpr "+" e (ValExpr (JInt i))) | i < 0 = InfixExpr "-" e (ValExpr (JInt $ negate i))
    f (InfixExpr "-" e (ValExpr (JInt i))) | i < 0 = InfixExpr "+" e (ValExpr (JInt $ negate i))
    f (PPostExpr True "-" (ValExpr (JInt i))) = ValExpr (JInt $ negate i)
    f (PPostExpr True "-" (ValExpr (JDouble i))) = ValExpr (JDouble $ negate i)
    f (PPostExpr True "!" e) | Just b <- exprCond e = if b then jFalse else jTrue
    f e = e

-- is expression a numeric or string constant?
isConst :: JExpr -> Bool
isConst (ValExpr (JDouble _)) = True
isConst (ValExpr (JInt _)) = True
isConst (ValExpr (JStr _)) = True
isConst _ = False

isVar :: JExpr -> Bool
isVar (ValExpr (JVar _)) = True
isVar _ = False

isConstOrVar :: JExpr -> Bool
isConstOrVar x = isConst x || isVar x

normalize :: JExpr -> JExpr
normalize = rewriteOf template assoc . rewriteOf template comm . foldExpr
  where
    comm :: JExpr -> Maybe JExpr
    comm (InfixExpr op e1 e2)
      |  commutes op && isConst e1 && not (isConst e2)
      || commutes op && isVar e1 && not (isConstOrVar e2) = f (InfixExpr op e2 e1)
    comm (InfixExpr op1 e1 (InfixExpr op2 e2 e3))
      |   op1 == op2 && commutes op1 && isConst e1 && not (isConst e2) 
      ||  op1 == op2 && commutes op1 && isVar e1 && not (isConstOrVar e2)
      || commutes2a op1 op2 && isConst e1 && not (isConst e2)
      || commutes2a op1 op2 && isVar e1 && not (isConstOrVar e2)
          = f (InfixExpr op1 e2 (InfixExpr op2 e1 e3))
    comm _ = Nothing
    assoc :: JExpr -> Maybe JExpr
    assoc (InfixExpr op1 (InfixExpr op2 e1 e2) e3)
      | op1 == op2 && associates op1 = f (InfixExpr op1 e1 (cf $ InfixExpr op1 e2 e3))
      | associates2a op1 op2 = f (InfixExpr op1 e1 (cf $ InfixExpr op2 e2 e3))
      | associates2b op1 op2 = f (InfixExpr op2 e1 (cf $ InfixExpr op1 e2 e3))
    assoc _ = Nothing
    commutes   op  = op `elem` [] --["*", "+"]                            --  a * b       = b * a
    commutes2a op1 op2 = (op1,op2) `elem` [] -- [("+","-"),("*", "/")]     --  a + (b - c) = b + (a - c)
    associates op  = op `elem` [] -- ["*", "+"]                            -- (a * b) * cv = a * (b * c)
    associates2a op1 op2 = (op1,op2) `elem` [] -- [("+", "-"), ("*", "/")] -- (a + b) - c  = a + (b - c)  -- (a*b)/c = a*(b/c)
    associates2b op1 op2 = (op1,op2) `elem` [] -- [("-", "+"), ("/", "*")] -- (a - b) + c  = a + (c - b)
    cf = rewriteOf template comm . foldExpr
    f  = Just . foldExpr

lorOp :: JExpr -> JExpr -> JExpr
lorOp e1 e2 | Just b <- exprCond e1 = if b then jTrue else e2
lorOp e1 e2 = InfixExpr "||" e1 e2

landOp :: JExpr -> JExpr -> JExpr
landOp e1 e2 | Just b <- exprCond e1 = if b then e2 else jFalse
landOp e1 e2 = InfixExpr "&&" e1 e2

-- if expression is used in a condition, can we statically determine its result?
exprCond :: JExpr -> Maybe Bool
exprCond (ValExpr (JInt x)) = Just (x /= 0)
exprCond (ValExpr (JDouble x)) = Just (x /= 0)
exprCond (ValExpr (JStr xs)) = Just (not $ null xs)
exprCond (ValExpr (JVar (StrI "false"))) = Just False   -- fixme these need to be proper values in jmacro
exprCond (ValExpr (JVar (StrI "true"))) = Just True
exprCond (ValExpr (JVar (StrI "undefined"))) = Just False
exprCond (ValExpr (JVar (StrI "null"))) = Just False
exprCond _ = Nothing

-- constant folding and other bottom up rewrites
intInfixOp :: String -> Integer -> Integer -> JExpr
intInfixOp "+" i1 i2 = ValExpr (JInt (i1+i2))
intInfixOp "-" i1 i2 = ValExpr (JInt (i1-i2))
intInfixOp "*" i1 i2 = ValExpr (JInt (i1*i2))
intInfixOp "/" i1 i2
  | i2 /= 0 && d * i2 == i1 = ValExpr (JInt d)
  where
    d = i1 `div` i2
intInfixOp "%"   i1 i2 = ValExpr (JInt (i1 `mod` i2)) -- not rem?
intInfixOp "&"   i1 i2 = bitwiseInt (.&.) i1 i2
intInfixOp "|"   i1 i2 = bitwiseInt (.|.) i1 i2
intInfixOp "^"   i1 i2 = bitwiseInt xor   i1 i2
intInfixOp ">"   i1 i2 = jBool (i1 > i2)
intInfixOp "<"   i1 i2 = jBool (i1 < i2)
intInfixOp "<="  i1 i2 = jBool (i1 <= i2)
intInfixOp ">="  i1 i2 = jBool (i1 >= i2)
intInfixOp "===" i1 i2 = jBool (i1 == i2)
intInfixOp "!==" i1 i2 = jBool (i1 /= i2)
intInfixOp "=="  i1 i2 = jBool (i1 == i2)
intInfixOp "!="  i1 i2 = jBool (i1 /= i2)
intInfixOp op i1 i2 = InfixExpr op (ValExpr (JInt i1)) (ValExpr (JInt i2))

doubleInfixOp :: String -> SaneDouble -> SaneDouble -> JExpr
doubleInfixOp "+"   (SaneDouble d1) (SaneDouble d2) = ValExpr (JDouble $ SaneDouble (d1+d2))
doubleInfixOp "-"   (SaneDouble d1) (SaneDouble d2) = ValExpr (JDouble $ SaneDouble (d1-d2))
doubleInfixOp "*"   (SaneDouble d1) (SaneDouble d2) = ValExpr (JDouble $ SaneDouble (d1*d2))
doubleInfixOp "/"   (SaneDouble d1) (SaneDouble d2) = ValExpr (JDouble $ SaneDouble (d1/d2))
doubleInfixOp ">"   (SaneDouble d1) (SaneDouble d2) = jBool (d1 > d2)
doubleInfixOp "<"   (SaneDouble d1) (SaneDouble d2) = jBool (d1 < d2)
doubleInfixOp "<="  (SaneDouble d1) (SaneDouble d2) = jBool (d1 <= d2)
doubleInfixOp ">="  (SaneDouble d1) (SaneDouble d2) = jBool (d1 >= d2)
doubleInfixOp "===" (SaneDouble d1) (SaneDouble d2) = jBool (d1 == d2)
doubleInfixOp "!==" (SaneDouble d1) (SaneDouble d2) = jBool (d1 /= d2)
doubleInfixOp "=="  (SaneDouble d1) (SaneDouble d2) = jBool (d1 == d2)
doubleInfixOp "!="  (SaneDouble d1) (SaneDouble d2) = jBool (d1 /= d2)
doubleInfixOp op d1 d2 = InfixExpr op (ValExpr (JDouble d1)) (ValExpr (JDouble d2))

bitwiseInt :: (Int32 -> Int32 -> Int32) -> Integer -> Integer -> JExpr
bitwiseInt op i1 i2 =
  let i = fromIntegral i1 `op` fromIntegral i2
  in ValExpr (JInt $ fromIntegral i)

-----------------------------------------------------------
-- stack and stack pointer magic
-----------------------------------------------------------

type StackFacts = Maybe Integer  -- relative offset of h$sp if known

propagateStack :: ([Ident],[Ident],Graph) -> ([Ident],[Ident],Graph)
propagateStack (args,locals,g)
  | nrets > 4 || stackAccess < nrets - 1 || stackAccess == 0 = (args, locals, g)
  | otherwise = (args,locals,g')
  where
    allNodes = IM.elems (g ^. nodes)
    nrets = length [() | (ReturnNode{}) <- allNodes]
    stackAccess = length $ filter ((StrI "h$stack") `S.member`)
                     (map exprDeps (allNodes ^.. template))

    sf = stackFacts g
    g' :: Graph
    g' = foldl' (\g (k,v) -> updNode k v g) g (IM.toList $ g^.nodes)

    updNode :: NodeId -> Node -> Graph -> Graph
    updNode nid e@(IfNode _ n1 _) g
      | known nid && unknown n1 = replace nid e g
    updNode nid e@(WhileNode _ n1) g
      | known nid && unknown n1 = replace nid e g
    updNode nid e@(ForInNode _ _ _ n1) g
      | known nid && unknown n1 = replace nid e g
    updNode nid e@(SwitchNode _ _ n1) g
      | known nid && unknown n1 = replace nid e g
    updNode nid e@(TryNode _ _ _ _) g
      | known nid = replace nid e g
    updNode nid e@(ReturnNode{}) g
      | known nid = replace nid e g
    updNode nid e@(SimpleNode{}) g
      | known nid && isNothing (join (lookupFact nid 1 sf)) = replace nid e g
    updNode nid e@(SimpleNode{}) g
      | Just (Just x) <- lookupFact nid 0 sf,
        Just (Just y) <- lookupFact nid 1 sf,
         x /= y = nodes %~ IM.insert nid (SequenceNode []) $ g
    updNode nid n g = updExprs nid n g

    updExprs :: NodeId -> Node -> Graph -> Graph
    updExprs nid n g
      | Just (Just offset) <- lookupFact nid 2 sf =
         let n' =  (tinplate %~ updExpr offset) n
         in  nodes %~ IM.insert nid n' $ g
      | otherwise = g

    updExpr :: Integer -> JExpr -> JExpr
    updExpr 0 e = e
    updExpr n e = transformOf tinplate sp e
      where
        sp (ValExpr (JVar (StrI "h$sp"))) = [je|h$sp+`n`|]
        sp e = e

    known :: NodeId -> Bool
    known nid | Just (Just _) <- lookupFact nid 0 sf = True
              | otherwise = False

    unknown :: NodeId -> Bool
    unknown = not . known

    replace :: NodeId -> Node -> Graph -> Graph
    replace nid n g
      | Just (Just x) <- lookupFact nid 0 sf, x /= 0 =
          let newId = g^.nodeid
              sp = ValExpr (JVar (StrI "h$sp"))
              newNodes = IM.fromList
                [ (nid,     SequenceNode [newId, newId+1])
                , (newId,   SimpleNode (AssignStat sp (InfixExpr "+" sp (ValExpr (JInt x)))))
                , (newId+1, n)
                ]
          in  (nodeid %~ (+2)) . (nodes %~ IM.union newNodes) $ g
      | otherwise = g

stackFacts :: Graph -> Facts StackFacts
stackFacts g = foldForward combineStack f0 (Just 0) g
  where
    f0 = def { fIf      = updSfT
             , fWhile   = updSfT
             , fDoWhile = updSfT
             , fSwitch  = switched
             , fReturn  = updSf1
             , fForIn   = updSf3
             , fSimple  = simple
             , fTry     = \_ _ -> Nothing
             }
    updSfT _ e sf = tup (updSf e sf)
    updSf1 _ = updSf
    updSf3 _ _ _ = updSf
    updSf :: JExpr -> StackFacts -> StackFacts
    updSf e sf | Just m <- mutated e, (StrI "h$sp") `S.notMember` m = sf
               | otherwise                                          = Nothing
    updSf' :: JStat -> StackFacts -> StackFacts
    updSf' s sf | Just m <- mutated' s, (StrI "h$sp") `S.notMember` m = sf
                | otherwise                                           = Nothing

    switched :: NodeId -> JExpr -> [JExpr] -> StackFacts -> ([StackFacts], StackFacts)
    switched _ e es sf = let sf' = updSf e sf in (replicate (length es) sf', sf')

    adjSf :: Integer -> StackFacts -> StackFacts
    adjSf n = fmap (+n)

    adjSfE :: JExpr -> StackFacts -> StackFacts
    adjSfE e sf | (ValExpr (JVar (StrI "h$sp"))) <- e = sf
                | (InfixExpr op (ValExpr (JVar (StrI "h$sp"))) (ValExpr (JInt x))) <- e =
                   case op of
                     "+" -> adjSf x sf
                     "-" -> adjSf (-x) sf
                     _   -> Nothing
                | otherwise = Nothing
      where e' = normalize e

    simple :: NodeId -> JStat -> StackFacts -> StackFacts
    simple nid (ApplStat e es) = updSf (ApplExpr e es)
    simple nid (PPostStat _ "++" (ValExpr (JVar (StrI "h$sp")))) = adjSf 1
    simple nid (PPostStat _ "--" (ValExpr (JVar (StrI "h$sp")))) = adjSf (-1)
    simple nid (AssignStat (ValExpr (JVar (StrI "h$sp"))) e) = adjSfE e
    simple nid s = updSf' s

    combineStack x y | x == y    = x
                     | otherwise = Nothing
