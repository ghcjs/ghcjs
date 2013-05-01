{-# LANGUAGE TemplateHaskell, TupleSections, DeriveDataTypeable, DeriveFunctor, ScopedTypeVariables, GADTs #-}

{-
   Dataflow analysis on the bastard child of a JavaScript AST and a control flow graph

   some valid JavaScript syntax may be unsupported here
-}

module Gen2.Dataflow ( Graph(..), nodes, arcsIn, arcsOut, entry, nodeid, labels
                     , Node(..)
                     , NodeId
                     , Arc(..), arcFrom, arcTo, arcType
                     , ArcType(..)
                     , Facts(..)
                     , noFacts, addFact, lookupFact
                     , cfg
                     , unCfg
                     , Forward(..)
                     , constForward
                     , foldForward, foldBackward
                     ) where

import           Control.Applicative
import           Control.Lens
import           Control.Lens.Plated
import           Data.Data.Lens
import           Control.Monad.State
import           Data.Default
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Data.List (foldl')
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Set (Set)
import           Data.Foldable (traverse_)
import           Language.Javascript.JMacro
import           Data.Typeable
import           Data.Data

type NodeId  = Int
type Label = String
data Node where
  SimpleNode   :: JStat    -> Node
  SequenceNode :: [NodeId] -> Node
  IfNode       :: JExpr -> NodeId -> NodeId -> Node
  WhileNode    :: JExpr -> NodeId -> Node
  DoWhileNode  :: JExpr -> NodeId -> Node
  ForInNode    :: Bool -> Ident -> JExpr -> NodeId -> Node
  SwitchNode   :: JExpr -> [(JExpr,NodeId)] -> NodeId -> Node
  ReturnNode   :: JExpr -> Node
  TryNode      :: NodeId -> Ident -> NodeId -> NodeId -> Node
  BreakNode    :: Maybe String -> NodeId -> Node
  ContinueNode :: Maybe String -> NodeId -> Node
  LabelNode    :: String -> NodeId -> Node
    deriving (Show, Data, Typeable, Eq, Ord)

isLoop :: Node -> Bool
isLoop (WhileNode{}) = True
isLoop (DoWhileNode{}) = True
isLoop (ForInNode{}) = True
isLoop _ = False

isSwitch :: Node -> Bool
isSwitch (SwitchNode{}) = True
isSwitch _ = False

isContinue :: Node -> Bool
isContinue (ContinueNode{}) = True
isContinue _ = False

isBreak :: Node -> Bool
isBreak (BreakNode{}) = True
isBreak _ = False

data Graph = Graph { _nodes :: IntMap Node
                   , _arcsIn :: IntMap (Set Arc)
                   , _arcsOut :: IntMap (Set Arc)
                   , _entry :: NodeId
                   , _nodeid :: NodeId
                   , _labels :: Map String NodeId
                   }
  deriving (Data, Typeable)

data Arc = Arc { _arcFrom :: NodeId
               , _arcTo   :: NodeId
               , _arcType :: ArcType
               }
  deriving (Eq, Ord, Data, Typeable)

data ArcType = BreakArc | ContinueArc deriving (Eq, Ord, Show, Data, Typeable)

makeLenses ''Graph
makeLenses ''Node
makeLenses ''Arc

instance Show Graph
  where
    show g = unlines [ns, arcs, lbls, extra]
      where
        ns   = unlines $ g ^. nodes . to (map (\(i,n) -> show i ++ ": " ++ show n) . IM.toList)
        arcs   = unlines . map show . S.toList . S.unions $ g ^. arcsIn . to IM.elems
        lbls = unlines $ g ^. labels . to (map (\(l,i) -> "label: " ++ l ++ ": " ++ show i) . M.toList)
        extra  = unlines [ "entry: " ++ (g ^. entry . to show) ]

instance Show Arc where
  show (Arc f t ty) = show f ++ " -> " ++ show t ++ " (" ++ show ty ++ ")"

emptyGraph = Graph IM.empty IM.empty IM.empty 0 1 M.empty

addArc :: Arc -> State Graph  ()
addArc a = do
  let sa = S.singleton a
  arcsOut %= IM.insertWith S.union (a^.arcFrom) sa
  arcsIn  %= IM.insertWith S.union (a^.arcTo) sa

newNodeId :: State Graph NodeId
newNodeId = do
  n <- use nodeid
  nodeid %= (+1)
  return n

newLabel :: String -> NodeId -> State Graph ()
newLabel lbl lid = do
  labels %= M.insert lbl lid

lookupLabel :: String -> State Graph NodeId
lookupLabel lbl = do
  lbls <- use labels
  case M.lookup lbl lbls of
    Just nid -> return nid
    Nothing  -> error ("lookupLabel: unknown label: " ++ lbl)

newNode :: Node -> NodeId -> State Graph NodeId
newNode s n = nodes %= IM.insert n s >> return n

continueTo :: Maybe String -> NodeId -> NodeId -> State Graph NodeId
continueTo lbl nid n
  | nid < 0 = error "continueTo: continuing to invalid node, not in a loop?"
  | otherwise = do
      cnid <- newNode (ContinueNode lbl nid) n
      addArc (Arc n nid ContinueArc)
      return n

breakTo :: Maybe String -> NodeId -> NodeId -> State Graph NodeId
breakTo lbl nid n
  | nid < 0 = error "breakTo: breaking to invalid node, not in a loop?"
  | otherwise = do
      newNode (BreakNode lbl nid) n
      addArc (Arc n nid BreakArc)
      return n

-- | after the initial conversion, labels may point to a SequenceNode or another label
--   fix them, and make sure that all labels point to a loop or switch now
fixLabels :: State Graph ()
fixLabels = do
  lbls <- use labels
  ns   <- use nodes
  forM_ (M.toList lbls) (\(s,n) -> updLabel s ns n n)
  checkLabels
  checkJumps
    where
      updLabel :: String -> IntMap Node -> NodeId -> NodeId -> State Graph ()
      updLabel s ns orig n =
        case IM.lookup n ns of
          (Just (SequenceNode (s0:_))) -> updLabel s ns orig s0
          (Just (LabelNode _ l0))      -> updLabel s ns orig l0
          _  -> when (n /= orig) $ do
                  allArcs <- S.toList . S.unions . IM.elems <$> use arcsOut
                  let f m      = if m == orig then n else m
                      f' (BreakNode l m)    | m == orig = BreakNode l n
                      f' (ContinueNode l m) | m == orig = ContinueNode l n
                      f' x = x
                      allArcs' = map ((arcTo %~ f) . (arcFrom %~ f)) allArcs
                  arcsOut .= IM.fromListWith S.union (map (\a -> (a ^. arcFrom, S.singleton a)) allArcs')
                  arcsIn  .= IM.fromListWith S.union (map (\a -> (a ^. arcTo,   S.singleton a)) allArcs')
                  labels  %= fmap f
                  nodes   %= fmap f'

-- | check that labels are only used for loops
checkLabels :: State Graph ()
checkLabels = do
  lbls <- use labels
  ns   <- use nodes
  forM_ (M.toList lbls) (\(_,n) -> checkValidLabelTarget ns n)
    where
      checkValidLabelTarget ns l =
        case IM.lookup l ns of
          Just x | isLoop x -> return ()
          Just x            -> error ("invalid label target: " ++ show x)
          _                 -> error ("unknown label target: " ++ show l)

-- | check that continue only jumps to loops, break to switch or loop
checkJumps :: State Graph ()
checkJumps = do
  out <- S.toList . S.unions . IM.elems <$> use arcsOut
  ns  <- use nodes
  forM_ out (checkJump ns)
    where
      checkJump ns (Arc fr to ContinueArc)
        | check isLoop ns to && check isContinue ns fr = return ()
      checkJump ns (Arc fr to BreakArc)
        | (check isLoop ns to || check isSwitch ns to) && check isBreak ns fr = return ()
      checkJump _ (Arc fr to _) = error ("invalid jump: " ++ show fr ++ " -> " ++ show to)
      check p ns n = maybe False p (IM.lookup n ns)

cfg :: JStat -> Graph
cfg s = execState buildGraph emptyGraph
  where
    source = 0
    sink = 1
    buildGraph = do
      start <- newNodeId
      go s (-1) (-1) start
      entry .= start
      fixLabels
    loopOf s1 f n = do
      s1n <- go s1 n n =<< newNodeId
      newNode (f s1n) n
    newSimpleNode s n = newNode (SimpleNode s) n
    go :: JStat -> NodeId -> NodeId -> NodeId -> State Graph NodeId
    go s@(DeclStat{})   lb lc n = newSimpleNode s n
    go s@(ReturnStat e) lb lc n = do
      newNode (ReturnNode e) n
      return n
    go s@(IfStat e s1 s2) lb lc n = do
      s1n <- go s1 lb lc =<< newNodeId
      s2n <- go s2 lb lc =<< newNodeId
      newNode (IfNode e s1n s2n) n
    go s@(WhileStat True e s1) lb lc n = loopOf s1 (DoWhileNode e) n
    go s@(WhileStat False e s1) lb lc n = loopOf s1 (WhileNode e) n
    go s@(ForInStat b i e s1) lb lc n = loopOf s1 (ForInNode b i e) n
    go s@(SwitchStat e es sd) lb lc n = do
      ns <- mapM (\(e',s') -> (e',) <$> (go s' n lc =<< newNodeId)) es
      sd' <- go sd n lc =<< newNodeId
      newNode (SwitchNode e ns sd') n
    go s@(TryStat t i c f) lb lc n = do
      tn <- go t lb lc =<< newNodeId
      cn <- go c lb lc =<< newNodeId
      fn <- go f lb lc =<< newNodeId
      newNode (TryNode tn i cn fn) n
    go s@(BlockStat ss)           lb lc n = do
      ss' <- (SequenceNode <$> mapM (\s' -> go s' lb lc =<< newNodeId) ss)
      newNode ss' n
    go s@(ApplStat{})                    lb lc n = newSimpleNode s n
    go s@(PPostStat{})                   lb lc n = newSimpleNode s n
    go s@(AssignStat{})                  lb lc n = newSimpleNode s n
    go s@(UnsatBlock{})                  lb lc n = error "cfg: unsaturated block"
    go s@(AntiStat{})                    lb lc n = error "cfg: antistat"
    go s@(ForeignStat{})                 lb lc n = error "cfg: foreignstat"
    go s@(LabelStat lbl s1)              lb lc n = do
      lid <- newNodeId
      newLabel lbl lid
      go s1                              lb lc lid
      newNode (LabelNode lbl lid) n
    go s@(BreakStat lbl@(Just lbl'))     lb lc n = do
      ll <- lookupLabel lbl'
      breakTo lbl ll n
    go s@(BreakStat lbl)                 lb lc n = breakTo lbl lb n
    go s@(ContinueStat lbl@(Just lbl'))  lb lc n = do
      ll <- lookupLabel lbl'
      continueTo lbl ll n
    go s@(ContinueStat lbl)              lb lc n = continueTo lbl lc n


unCfg :: Graph -> JStat
unCfg g = go' (g ^. entry)
  where
    go' :: Int -> JStat
    go' n = case IM.lookup n (g ^. nodes) of
              Just x  -> go x
              Nothing -> error ("unCfg: unknown node: " ++ show n)
    go :: Node -> JStat
    go (SimpleNode s)        = s
    go (SequenceNode ss)     = BlockStat (map go' ss)
    go (IfNode e n1 n2)      = IfStat e (go' n1) (go' n2)
    go (WhileNode e s)       = WhileStat False e (go' s)
    go (DoWhileNode e s)     = WhileStat True e (go' s)
    go (ForInNode b i e s)   = ForInStat b i e (go' s)
    go (SwitchNode e ss s)   = SwitchStat e (map (\(e,s') -> (e, go' s')) ss) (go' s)
    go (ReturnNode e)        = ReturnStat e
    go (TryNode t i c f)     = TryStat (go' t) i (go' c) (go' f)
    go (BreakNode lbl _)     = BreakStat lbl
    go (ContinueNode lbl _)  = ContinueStat lbl
    go (LabelNode lbl s)     = LabelStat lbl (go' s)

-- facts, forward flow:
-- index 0: combined fact when entering for the first time (continue, looping excluded)
-- index 1: combined fact leaving the node
-- index 2: facts just before the expressions in the node, if any
-- index n: other interesting positions, depend on node
newtype Facts a = Facts (Map (NodeId,Int) a)
  deriving (Eq)

instance Show a => Show (Facts a) where
  show (Facts xs) = unlines . map (\(k,v) -> show k ++ " -> " ++ show v) . M.toList $ xs

noFacts :: Facts a
noFacts = Facts M.empty

addFact :: NodeId -> Int -> a -> Facts a -> Facts a
addFact node idx x (Facts m) = Facts $ M.insert (node,idx) x m

lookupFact :: NodeId -> Int -> Facts a -> Maybe a
lookupFact node idx (Facts m) = M.lookup (node,idx) m

data Forward a =
  Forward { fIf       :: NodeId -> JExpr                   -> a -> (a,a)
          , fWhile    :: NodeId -> JExpr                   -> a -> (a,a)
          , fDoWhile  :: NodeId -> JExpr                   -> a -> (a,a)
          , fSimple   :: NodeId -> JStat                   -> a -> a
          , fBreak    :: NodeId                            -> a -> a
          , fContinue :: NodeId                            -> a -> a
          , fReturn   :: NodeId -> JExpr                   -> a -> a
          , fTry      :: NodeId                            -> a -> a
          , fForIn    :: NodeId -> Bool  -> Ident -> JExpr -> a -> a
          , fSwitch   :: NodeId -> JExpr -> [JExpr]        -> a -> ([a],a)
          }

instance Default (Forward a) where
  def = Forward c2tup c2tup c2tup fconst2 fconst fconst fconst2 fconst fconst4 defSwitch

constForward :: a -> (Forward a)
constForward z = Forward (const3 (z,z)) (const3 (z,z)) (const3 (z,z)) (const3 z)
                         (const2 z) (const2 z) (const3 z) (const2 z) (const5 z) (zSwitch z)

c2tup :: a -> b -> c -> (c,c)
c2tup _ _ x = (x,x)

fconst :: a -> b -> b
fconst _ x = x

fconst2 :: a -> b -> c -> c
fconst2 _ _ x = x

const2 :: a -> b -> c -> a
const2 x _ _ = x

const3 :: a -> b -> c -> d -> a
const3 x _ _ _ = x

const4 :: a -> b -> c -> d -> e -> a
const4 x _ _ _ _ = x

const5 :: a -> b -> c -> d -> e -> f -> a
const5 x _ _ _ _ _ = x

fconst3 :: a -> b -> c -> d -> d
fconst3 _ _ _ x = x

fconst4 :: a -> b -> c -> d -> e -> e
fconst4 _ _ _ _ x = x

defSwitch :: a -> b -> [c] -> d -> ([d],d)
defSwitch _ _ xs y = (replicate (length xs) y, y)

zSwitch :: d -> a -> b -> [c] -> d -> ([d],d)
zSwitch z _ _ xs _ = (replicate (length xs) z, z)

foldForward :: forall a. Eq a
            => (a -> a -> a)
            -> Forward a
            -> a
            -> Graph
            -> Facts a
foldForward c f z g = fixed (goEntry $ g^.entry) noFacts
  where
    combine :: Maybe a -> Maybe a -> Maybe a
    combine (Just x) (Just y) = Just (x `c` y)
    combine (Just x) _        = Just x
    combine _        y        = y

    combineMaybe :: a -> Maybe a -> a
    combineMaybe x my = maybe x (c x) my

    -- combine x with data from nodes, only if nodes have available data
    combineWith :: a -> [NodeId] -> State (Facts a) a
    combineWith x nids = do
      m <- get
      return $ foldl' c x (catMaybes $ map (\n -> lookupFact n 1 m) nids)

    combineFrom :: [NodeId] -> State (Facts a) (Maybe a)
    combineFrom nids = do
      m <- get
      case catMaybes (map (\n -> lookupFact n 1 m) nids) of -- retrieve fact 1, flows out of node
        (x:xs) -> return $ Just (foldl' c x xs)
        _      -> return Nothing

    upd :: NodeId -> Int -> a -> State (Facts a) ()
    upd nid i x = modify (addFact nid i x)

    upds :: NodeId -> [Int] -> a -> State (Facts a) ()
    upds nid is x = mapM_ (\i -> upd nid i x) is

    upd' :: NodeId -> Int -> Maybe a -> State (Facts a) ()
    upd' _   _ Nothing = return ()
    upd' nid i (Just x) = upd nid i x

    fact :: NodeId -> Int -> State (Facts a) (Maybe a)
    fact n i = lookupFact n i <$> get

    goEntry :: NodeId -> Facts a -> Facts a
    goEntry nid = execState (go' nid z)

    go' :: NodeId -> a -> State (Facts a) (Maybe a)
    go' nid x = go nid (lookupNode nid g) x

    go :: NodeId -> Node -> a -> State (Facts a) (Maybe a)
    go nid (SimpleNode s) x = do
      upds nid [0,2] x
      let x' = fSimple f nid s x
      upd nid 1 x'
      return (Just x')
    go nid (SequenceNode ss) x  = do
      upd nid 0 x
      go0 ss x
        where
          go0 (y:ys) x0 = do
            r <- go' y x0
            case r of
              Nothing -> return Nothing
              Just x1 -> go0 ys x1
          go0 [] x0 = do
            upd nid 1 x0
            return (Just x0)
    go nid (IfNode e s1 s2) x = do
      upds nid [0,2] x
      let (xi,xe) = fIf f nid e x
      xi' <- go' s1 xi
      xe' <- go' s2 xe
      let x' = combine xi' xe'
      upd' nid 1 x'
      return x'
    go nid (WhileNode e s) x = do
      upd nid 0 x
      let (brks, conts) = getBreaksConts nid g
      x0 <- combineWith x conts
      x1 <- combineMaybe x0 <$> fact nid 3
      upd nid 2 x1
      let (xt,xf) = fWhile f nid e x1
      s0 <- go' s xt
      upd' nid 3 s0
      x3 <- combineWith xf brks
      upd nid 1 x3
      return (Just x3)
    go nid (DoWhileNode e s) x = do
      upd nid 0 x
      let (brks, conts) = getBreaksConts nid g
      x0 <- combineMaybe x <$> fact nid 3
      s0 <- go' s x0
      case s0 of
        Nothing -> do
          s1 <- combineFrom conts
          case s1 of
            Nothing -> return Nothing
            Just s2 -> do
              upd nid 2 s2
              let (xt,xf) = fDoWhile f nid e s2
              upd nid 3 xt
              s3 <- combineWith xf brks
              upd nid 1 s3
              return (Just s3)
        Just x1 -> do
          x2 <- combineWith x1 conts
          upd nid 2 x2
          let (xt,xf) = fDoWhile f nid e x2
          upd nid 3 xt
          x3 <- combineWith xf brks
          upd nid 1 x3
          return (Just x3)
    go nid (SwitchNode e ss s) x = do
      let (brks, _) = getBreaksConts nid g
          (xes,xd)       = fSwitch f nid e (map fst ss) x
          go0 [] []      = return
          go0 (z:zs) ((e,y):ys) = go0 zs ys <=< go' y . combineMaybe z
          go0 _ _ = error "foldForward: unmatched list length for switch"
      upds nid [0,2] x
      s0 <- go0 xes ss Nothing
      s1 <- go' s (combineMaybe xd s0)
      s2 <- combine s1 <$> combineFrom brks
      upd' nid 1 s2
      return s2
    go nid (ReturnNode e) x = do
      upds nid [0,2] x
      let x' = fReturn f nid e x
      upd nid 3 x'
      return Nothing
    go nid (BreakNode{}) x = do
      upds nid [0,2] x
      let x'= fBreak f nid x
      upd nid 3 x'
      return Nothing
    go nid (ContinueNode{}) x = do
      upds nid [0,2] x
      let x' = fContinue f nid x
      upd nid 3 x'
      return Nothing
    go nid (LabelNode _ s) x = do
      upds nid [0,2] x
      x0 <- go' s x
      upd' nid 1 x0
      return x0
    go nid (TryNode t _ c fin) x = do
      upd nid 0 x
      let x' = fTry f nid x
      t' <- go' t x'
      upd' nid 2 t'
      c' <- go' c z
      upd' nid 3 c'
      case combine t' c' of
        Just x0 -> do
          s <- go' fin x0
          upd' nid 1 s
          return s
        Nothing  -> return Nothing
--    go _ _ _ = error "unsupported node type"

foldBackward :: forall a. Eq a
             => (a -> a -> a)
             -> (NodeId -> Node -> a -> a)
             -> a
             -> Graph
             -> Facts a
foldBackward c f z g = fixed (goEntry $ g^.entry) noFacts
  where
    upd :: NodeId -> Int -> a -> State (Facts a) ()
    upd nid i x = modify (addFact nid i x)

    fact :: NodeId -> Int -> State (Facts a) (Maybe a)
    fact n i = lookupFact n i <$> get

    goEntry :: NodeId -> Facts a -> Facts a
    goEntry nid = execState (go' nid z)

    go' :: NodeId -> a -> State (Facts a) a
    go' nid x = go nid (lookupNode nid g) x

    go :: NodeId -> Node -> a -> State (Facts a) a
    go nid n@(SimpleNode s) x = do
      upd nid 0 x
      let x0 = f nid n x
      upd nid 1 x0
      return x0
    go nid n@(SequenceNode ss) x = do
      upd nid 0 x
      go0 (reverse ss) x
        where
          go0 (y:ys) x0 =
            go0 ys =<< go' y x0
          go0 [] x0 = do
            upd nid 1 x0
            return x0
    go nid n@(IfNode _ s1 s2) x = do
      upd nid 0 x
      xi <- go' s1 x
      xe <- go' s2 x
      let x0 = f nid n (xi `c` xe)
      upd nid 1 x0
      return x0
    go nid n@(WhileNode _ s) x = do
      upd nid 0 x
      x0 <- go' s x
      let x1 = f nid n (x `c` x0)
      upd nid 1 x1
      return x1
    go nid n@(DoWhileNode _ s) x = do
      upd nid 0 x
      let x0 = f nid n x
      upd nid 2 x0
      x1 <- go' s x0
      let x2 = x `c` x1
      upd nid 1 x2
      return x2
    go nid n@(SwitchNode e ss s) x = do
      upd nid 0 x
      x0  <- go' s x
      xs1 <- go0 (reverse ss) x0
      let x2 = f nid n (foldl' c x0 xs1)
      upd nid 1 x2
      return x2
        where
          go0 [] x = return [x]
          go0 ((_,y):ys) x = do
            x' <- go' y x
            (x':) <$> go0 ys x'
    go nid n@(ReturnNode{}) _ = do
      upd nid 0 z
      let x = f nid n z
      upd nid 1 x
      return x
    go nid n@(BreakNode _ tgt) _ = do
      x <- fromMaybe z <$> fact tgt 0
      upd nid 0 x
      let x0 = f nid n x
      upd nid 1 x0
      return x0
    go nid n@(ContinueNode _ tgt) _ = do
      x <- fromMaybe z <$> fact tgt 2
      upd nid 0 x
      let x0 = f nid n x
      upd nid 1 x0
      return x0
    go nid n@(LabelNode _ s) x = do
      upd nid 0 x
      x0 <- go' s x
      upd nid 1 x0
      return x0
    go nid n@(TryNode t _ ctch fin) x = do
      upd nid 0 x
      f' <- go' fin x
      c' <- go' ctch f'
      t' <- go' t (f' `c` c')
      upd nid 1 t'
      return t'
    go _ _ _ = error "unsupported node type"

lookupNode :: NodeId -> Graph -> Node
lookupNode nid g = fromMaybe
                     (error $ "lookupNode: unknown node " ++ show nid)
                     (IM.lookup nid $ g ^. nodes)

getBreaksConts :: NodeId -> Graph -> ([NodeId], [NodeId])
getBreaksConts nid g = go (maybe [] S.toList . IM.lookup nid $ g ^. arcsIn) ([],[])
  where
    go [] x = x
    go ((Arc fr _ BreakArc):ss)    (xs,ys) = go ss (fr:xs,ys)
    go ((Arc fr _ ContinueArc):ss) (xs,ys) = go ss (fr:xs,ys)

-- iterate until fixed point
fixed :: Eq a => (a -> a) -> a -> a
fixed f a | fa == a   = a
          | otherwise = fixed f fa
  where fa = f a


