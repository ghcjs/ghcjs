{-# LANGUAGE GADTs, StandaloneDeriving, DeriveDataTypeable, QuasiQuotes #-}
{-
  Optimizer: uses rewrite rules on a simpler intermediate representation
  of function bodies, only a subset of javascript can be represented
  by this

  assumptions made:
  - ast is fully saturated
  - no nested variables scopes, no nested functions
  - single assignment for everything except stack, heap, sp, hp
  - no exceptions
-}
module Gen2.Optimizer where

import Compiler.Hoopl
import Control.Monad
import Language.Javascript.JMacro
import Control.Applicative ((<$>))
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M
import Data.Generics.Aliases
import Data.Generics.Schemes
import Data.Data
import Data.Monoid
import Control.Arrow

import Panic

import Gen2.Utils

import Debug.Trace

data Expr = UnaryOpE   Op Expr       -- ~e
          | BinaryOpE  Op Expr Expr  -- e1 + e2
          | ValE       Val           -- v -- fixme
          | IdxE       Expr Expr     -- e1[e2]
          | PropE      Expr Id -- Var      -- e.v
          | CondE      Expr Expr Expr
  deriving (Show, Ord, Eq, Data, Typeable)

-- fixme need list values?
data Val = DoubleV Double
         | IntV Integer
         | StrV String
         | IdV Id
  deriving (Show, Ord, Eq, Data, Typeable)

newtype Op = Op String deriving (Show, Ord, Eq, Data, Typeable)

newtype Id  = Id { unId :: String } deriving (Show, Ord, Eq, Data, Typeable)
newtype Lbl = Lbl Unique

data LVal = LVar Id       -- v    = ...
          | LIdx Id Expr  -- v[e] = ...
   deriving (Show, Ord, Eq, Data, Typeable)

data Proc = Proc Id (Graph I C C) (Set Id)

showProc :: Proc -> String
showProc (Proc n g vs) = show n ++ "\n" ++ showGraph show g ++ "\n"

data I i o where
  Entry    :: Label                          -> I C O
  Assign   :: LVal -> Expr                   -> I O O
  Apply    :: Expr -> [Expr]                 -> I O O
  Switch   :: Expr -> [(Val,Label)] -> Label -> I O C
  Cond     :: Expr -> Label -> Label         -> I O C
  Jump     :: Label                          -> I O C
  TailCall :: Expr                           -> I O C

deriving instance Show (I e x)

instance NonLocal I where
  entryLabel (Entry x)      = x
  successors (Cond _ l1 l2) = [l1,l2]
  successors (Switch _ cs d) = map snd cs ++ [d]
  successors (Jump l)       = [l]
  successors (TailCall _)   = []

instance HooplNode I where
  mkBranchNode label = Jump label
  mkLabelNode label  = Entry label

showHoopls :: JStat -> String
showHoopls s =  unlines (map showIndent funs) ++ "\n---\n" ++
                concatMap showProc procs
    where
      funs  = listify isfun s
      procs = map (toProc.funstat) funs
      isfun (AssignStat _ (ValExpr(JFunc{}))) = True
      isfun _         = False
      funstat (AssignStat n (ValExpr(JFunc _ s))) = (n,s)
      funstat _           = error "showHoopls: funstat"

optimize :: JStat -> JStat
optimize s = everywhere (mkT optimizeBody) s

optimizeBody :: JStat -> JStat
optimizeBody (AssignStat n (ValExpr (JFunc [] s))) = AssignStat n (ValExpr (JFunc [] s'))
  where
    p = toProc (n,s)
    s' = fromProc $ optimizeProc p
optimizeBody x = x

optimizeProc :: Proc -> Proc
optimizeProc = id

toProc :: (JExpr, JStat) -> Proc
toProc (name,s) = Proc n ir vs
  where
   n   = var  name
   ir  = runSimpleUniqueMonad $ do
           en <- freshLabel
           ex <- freshLabel
           g <- toIR [] en ex (removeDecls s)
           return $ label en <*> g
   vs  = S.fromList $ localVars s

removeDecls :: JStat -> JStat
removeDecls = everywhere (mkT filterDecls)
  where
    filterDecls :: JStat -> JStat
    filterDecls (BlockStat ss) = BlockStat (filter (not.isDecl) ss)
    filterDecls (DeclStat {})   = mempty
    filterDecls s              = s

isDecl :: JStat -> Bool
isDecl (DeclStat {}) = True
isDecl _             = False

localVars :: JStat -> [Id]
localVars s = [Id i | DeclStat (StrI i) _ <- listify isDecl s]

toIR :: [(Label,Label)] -> Label -> Label -> JStat -> SimpleUniqueMonad (Graph I O C)
toIR ls en ex (BlockStat ss)   = go ss emptyGraph
  where
    go :: [JStat] -> Graph I O O -> SimpleUniqueMonad (Graph I O C)
    go [] gr = return $ gr <*> jump ex
    go (x:xs) gr
        | BlockStat ss'  <- x = go (ss'++xs) gr
        | null xs             = liftM (gr <*>) (toIR ls en ex x)
        | AssignStat v e <- x = go xs (gr <*> assign (lval v) (expr e))
        | ApplStat  e es <- x = go xs (gr <*> apply (expr e) (map expr es))
        | otherwise = do
           ex2 <- freshLabel
           xg  <- toIR ls en ex2 x
           xsg <- toIR ls ex2 ex (BlockStat xs)
           return $ gr <*> xg |*><*| label ex2 <*> xsg
toIR ls en ex (AssignStat v e) = return $ assign (lval v) (expr e) <*> jump ex
toIR ls en ex (ApplStat e es)  = return $ apply (expr e) (map expr es) <*> jump ex
toIR ls en ex (ReturnStat e)   =
  return $ tailcall (expr e)
toIR ls en ex (IfStat e s1 s2) = do
  l1 <- freshLabel
  l2 <- freshLabel
  s1i <- toIR ls l1 ex s1
  s2i <- toIR ls l2 ex s2
  return $ cond (expr e) l1 l2 |*><*| label l1 <*> s1i |*><*| label l2 <*> s2i
toIR ls en ex (SwitchStat e cs d) = do
  cs' <- forM cs $ \(v,s) -> do
           cl <- freshLabel
           cb <- toIR ((cl,ex):ls) cl ex s -- fixme cl label wrong for continue
           return (((val v),cl), label cl <*> cb)
  dl <- freshLabel
  db <- toIR ((dl,ex):ls) dl ex d -- fimx wrong label for continue
  return $ switch (expr e) (map fst cs') dl |*><*| catClosed (map snd cs') |*><*| label dl <*> db
toIR ls en ex (WhileStat False e s) = do -- while
  bl   <- freshLabel
  cl   <- freshLabel
  body <- toIR ((cl,ex):ls) bl ex s
  return $ jump cl |*><*| label cl <*> cond (expr e) bl ex |*><*| label bl <*> body
toIR ls en ex (WhileStat True e s) = do -- do .. while()
  bl   <- freshLabel
  cl   <- freshLabel
  body <- toIR ((cl,ex):ls) bl ex s
  return $ jump bl |*><*| label cl <*> cond (expr e) bl ex |*><*| label bl <*> body
toIR ls en ex (BreakStat Nothing)    =
  return $ jump (snd . head $ ls)
toIR ls en ex (ContinueStat Nothing) =
  return $ jump (fst . head $ ls)
toIR ls en ex (DeclStat {})    = panic "toIR: DeclStat" -- removed in first pass
toIR ls en ex (TryStat {})     = panic "toIR: TryStat" -- no exceptions used in generated code
toIR ls en ex (PPostStat {})   = panic "toIR: PPostStat"  -- desugared to assignment?
toIR ls en ex (UnsatBlock {})  = panic "toIR: UnsatBlock" -- not allowed here
toIR ls en ex (AntiStat {})    = panic "toIR: AntiStat"
toIR ls en ex (ForeignStat {}) = panic "toIR: ForeignStat"
toIR ls en ex (ForInStat {})   = panic "toIR: ForInStat"
toIR _  _  _  s                = panic $ "toIR: unhandled statement: " ++ show s

catClosed :: [Graph I C C] -> Graph I C C
catClosed [] = emptyClosedGraph
catClosed [x] = x
catClosed (x:xs) = x |*><*| catClosed xs

newLabel :: SimpleUniqueMonad Lbl
newLabel = liftM Lbl freshUnique

label l       = mkFirst $ Entry l

assign v e    = mkMiddle $ Assign v e
apply e es    = mkMiddle $ Apply e es

jump  l       = mkLast $ Jump l
cond e t f    = mkLast $ Cond e t f
switch e cs d = mkLast $ Switch e cs d
tailcall e    = mkLast $ TailCall e

expr :: JExpr -> Expr
expr ve@(ValExpr v)       = ValE (val ve)
expr (IdxExpr e1 e2)      = IdxE (expr e1) (expr e2)
expr (InfixExpr o e1 e2)  = BinaryOpE (Op o) (expr e1) (expr e2)
expr (IfExpr be te fe)    = CondE (expr be) (expr te) (expr fe)
expr (PPostExpr True o e) = UnaryOpE (Op o) (expr e)
expr (SelExpr e (StrI i)) = PropE (expr e) (Id i)
expr e                    = panic $ "expr: unsupported expression: " ++ show e

var :: JExpr -> Id
var (ValExpr (JVar (StrI i))) = Id i
var v               = error $ "var: not an identifier: " ++ show v

lval :: JExpr -> LVal
lval (ValExpr (JVar (StrI i)))             = LVar (Id i)
lval (IdxExpr (ValExpr (JVar (StrI i))) e) = LIdx (Id i) (expr e)
lval v                                     = error $ "lval: not an assignable value: " ++ show v

val :: JExpr -> Val
val (ValExpr v) = toVal v
    where toVal (JDouble d)     = DoubleV d
          toVal (JInt i)        = IntV i
          toVal (JStr s)        = StrV s
          toVal (JVar (StrI i)) = IdV (Id i)
          toVal v           = error $ "val: toVal, unsupported value: " ++ show v
val _ = error "val: val"

--------
-- ir -> jmacro here
-------

fromProc :: Proc -> JStat
fromProc (Proc _ is vs) = decls <> mempty -- fromIR is
    where
      decls = BlockStat $ map ((\s -> DeclStat s Nothing) . StrI . unId) (S.toList vs)

fromExpr :: Expr -> JExpr
fromExpr (UnaryOpE (Op o) e)      = PPostExpr True o (fromExpr e)
fromExpr (BinaryOpE (Op o) e1 e2) = InfixExpr o (fromExpr e1) (fromExpr e2)
fromExpr (ValE v)                 = ValExpr (fromVal v)
fromExpr (IdxE e1 e2)             = IdxExpr (fromExpr e1) (fromExpr e2)
fromExpr (PropE e (Id i))         = SelExpr (fromExpr e) (StrI i)
fromExpr (CondE ec et ef)         = IfExpr (fromExpr ec) (fromExpr et) (fromExpr ef)

fromVal :: Val -> JVal
fromVal (DoubleV d)  = JDouble d
fromVal (IntV i)     = JInt i
fromVal (StrV s)     = JStr s
fromVal (IdV (Id i)) = JVar (StrI i)
{-
data JBlock = JBlock Label JStat

fromIR :: Graph I C C -> JStat
fromIR = undefined

fromIR' :: Graph I C C -> Map Label JBlock
fromIR' g = foldBlockNodesB3 (co, oo, oc) mempty
  where
    convertBlock :: Label -> JStat
    convertBlock l = case lookupBlock g l of
                       NoBlock     -> mempty
                       BodyBlock b -> foldBlockNodesB3 (co, oo, oc) mempty b
                       ExitBlock   -> mempty

    co :: I C O -> JStat -> JStat
    co (Entry l)       m = undefined

    oo :: I O O -> JStat -> JStat
    oo (Apply e args)  m = ApplStat (fromExpr e) (map fromExpr args)
    oo (Assign v e)    m = AssignStat (fromExpr e) (fromExpr e)

    oc :: I O C -> JStat -> JStat
    oc (Switch e cs d) m = SwitchStat (fromExpr e) (map (fromExpr *** ))
--    oc (Loop b c b ex) m = WhileStat 
    oc (Cond e l1 l2)  m = undefined
    oc (Jump l)        m = undefined
    oc (TailCall l)    m = undefined
-}

{-




-}
