{-# LANGUAGE CPP, QuasiQuotes, TupleSections, OverloadedStrings, LambdaCase, MultiWayIf, TemplateHaskell, ViewPatterns, BangPatterns #-}

{-
  Main generator module
-}

module Gen2.Generator (generate) where

import           Fingerprint
import           ForeignCall
import           CostCentre
import           FastString
import           TysWiredIn
import           BasicTypes
import           PrelNames
import           DynFlags
import           Encoding
import           UniqSet
import           Literal
import           DataCon
import           CoreSyn
import           IdInfo
import           TcType
import           UniqFM
import           Unique
import           StgSyn
import           PrimOp
import           Module
import           Panic
import           TyCon
import           Util
import           Type hiding (typeSize)
import           Name
import           GHC
import           Id

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens hiding ((||=))
import           Control.Monad.State.Strict

import           Data.Array
import           Data.Bits ((.|.), shiftL, shiftR, (.&.), testBit, xor, complement)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Char (ord, chr, isDigit)
import           Data.Either (partitionEithers)
import           Data.Function (on)
import           Data.Generics.Aliases (mkT)
import           Data.Generics.Schemes (everywhere)
import           Data.Int
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet        as IS
import           Data.Monoid
import           Data.Maybe (isJust, isNothing, catMaybes, fromMaybe, maybeToList, listToMaybe)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.List (partition, intercalate, sort, sortBy, foldl')
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)

import           Compiler.JMacro
import qualified Text.Parsec as P

import           Compiler.Compat
import           Compiler.Settings

import           Gen2.Base
import           Gen2.Utils
import           Gen2.Prim
import           Gen2.Rts
import           Gen2.RtsTypes
import           Gen2.StgAst
import           Gen2.RtsAlloc
import           Gen2.RtsApply
import qualified Gen2.Linker    as Linker
import           Gen2.ClosureInfo
import qualified Gen2.Optimizer as O
import qualified Gen2.Object    as Object
import           Gen2.Sinker
import           Gen2.Profiling

data DependencyDataCache = DDC
         { _ddcModule   :: !(IntMap        Object.Package)  -- Unique Module -> Object.Package
         , _ddcId       :: !(IntMap        Object.Fun)      -- Unique Id     -> Object.Fun (only to other modules)
         , _ddcOther    :: !(Map OtherSymb Object.Fun)
         }

makeLenses ''DependencyDataCache

type StgPgm     = [StgBinding]

data ExprCtx = ExprCtx
       { _ctxTop        :: Id
       , _ctxTarget     :: [JExpr]
       , _ctxEval       :: UniqSet Id
       , _ctxLne        :: UniqSet Id -- all lne-bound things
       , _ctxLneFrameBs :: UniqFM Int -- binds in current lne frame (defined at size)
       , _ctxLneFrame   :: [(Id,Int)] -- contents of current lne frame
       }

makeLenses ''ExprCtx

clearCtxStack :: ExprCtx -> ExprCtx
clearCtxStack ctx = ctx & ctxLneFrameBs .~ emptyUFM
                        & ctxLneFrame   .~ []

adjustCtxStack :: Int -> ExprCtx -> ExprCtx
adjustCtxStack n ctx
  | l < n     = panic $ "adjustCtxStack: let-no-escape stack too short: " ++
                        show l ++ " < " ++ show n
  | otherwise = ctx & ctxLneFrame %~ take n
  where
    l = ctx ^. ctxLneFrame . to length

addEval :: Id -> ExprCtx -> ExprCtx
addEval i = over ctxEval (flip addOneToUniqSet i)

generate :: GhcjsSettings
         -> DynFlags
         -> Module
         -> StgPgm
         -> CollectedCCs
         -> ByteString -- ^ binary data for the .js_o object file
generate settings df m s cccs =
  let (uf, s') = sinkPgm m s
  in  flip evalState (initState df m uf) $ do
        ifProfiling' $ initCostCentres cccs
        (st, lus) <- genUnits df m s'
        -- (exported symbol names, javascript statements) for each linkable unit
        p <- forM lus $ \u -> mapM (fmap (\(TxtI i) -> i) . jsIdI) (luIdExports u) >>=
                                \ts -> return (ts ++ luOtherExports u, luStat u)
        let (st', dbg) = dumpAst st settings df s'
        deps <- genDependencyData df m lus
        return . BL.toStrict $
          Object.object' st' deps (p ++ dbg) -- p first, so numbering of linkable units lines up

{- |
  Generate an extra linkable unit for the object file if -debug is active.
  this unit is never actually linked, but it contains the optimized STG AST
  so it can be easily reviewed using ghcjs --print-obj to aid in solving
  code generator problems.
 -}
dumpAst :: Object.SymbolTable
        -> GhcjsSettings
        -> DynFlags
        -> StgPgm
        -> (Object.SymbolTable, [([Text], BL.ByteString)])
dumpAst st _settings dflags s
  | buildingDebug dflags = (st', [(["h$debug", "h$dumpAst"], bs)])
  | otherwise            = (st, [])
      where
        (st', bs) = Object.serializeStat st [] [] [j| h$dumpAst = `x` |]
        x = intercalate "\n\n" (map showIndent s)

-- | variable prefix for the nth block in module
modulePrefix :: Module -> Int -> Text
modulePrefix m n =
  let encMod = zEncodeString . moduleNameString . moduleName $ m
  in  T.pack $ "h$" ++ encMod ++ "_id_" ++ show n

-- | data used to generate one ObjUnit in our object file
data LinkableUnit = LinkableUnit
  { luStat         :: BL.ByteString -- ^ serialized JS AST
  , luIdExports    :: [Id]          -- ^ exported names from haskell identifiers
  , luOtherExports :: [Text]        -- ^ other exports
  , luIdDeps       :: [Id]          -- ^ identifiers this unit depends on
  , luOtherDeps    :: [OtherSymb]   -- ^ symbols not from a haskell id that this unit depends on
  , luRequired     :: Bool          -- ^ always link this unit
  } deriving (Eq, Ord, Show)

-- | Generate the ingredients for the linkable units for this module
genUnits :: DynFlags
         -> Module
         -> StgPgm
         -> G (Object.SymbolTable, [LinkableUnit]) -- ^ the final symbol table and the linkable units
genUnits dflags m ss = generateGlobalBlock =<< go 2 Object.emptySymbolTable ss
    where
      go :: Int                 -- ^ the block we're generating (block 0 is the global unit for the module)
         -> Object.SymbolTable  -- ^ the shared symbol table
         -> StgPgm
         -> G (Object.SymbolTable, [LinkableUnit])
      go n st (x:xs) = do
        (st', lu) <- generateBlock st x n
        (st'', lus)  <- go (n+1) st' xs
        return (st'', lu:lus)
      go _ st []     = return (st, [])

      -- | Generate the global unit that all other blocks in the module depend on
      --   used for cost centres and static initializers
      --   the global unit has no dependencies, exports the moduleGlobalSymbol
      generateGlobalBlock :: (Object.SymbolTable, [LinkableUnit])
                          -> G (Object.SymbolTable, [LinkableUnit])
      generateGlobalBlock (st, lus) = do
        glbl <- use gsGlobal
        staticInit <- initStaticPtrs (collectStaticInfo ss)
        (st', [], bs) <- serializeLinkableUnit m st [] [] []
                         . O.optimize
                         . jsSaturate (Just $ modulePrefix m 1)
                         $ mconcat (reverse glbl) <> staticInit
        return (st', LinkableUnit bs [] [moduleGlobalSymbol dflags m] [] [] False : lus)

      -- | Generate the linkable unit for one binding or group of
      --   mutually recursive bindings
      generateBlock :: Object.SymbolTable
                    -> StgBinding
                    -> Int
                    -> G (Object.SymbolTable, LinkableUnit)
      generateBlock st decl n = do
        tl        <- genToplevel decl
        extraTl   <- use (gsGroup . ggsToplevelStats)
        ci        <- use (gsGroup . ggsClosureInfo)
        si        <- use (gsGroup . ggsStatic)
        unf       <- use gsUnfloated
        extraDeps <- use (gsGroup . ggsExtraDeps)
        resetGroup
        let allDeps  = collectIds unf decl
            topDeps  = collectTopIds decl
            required = hasExport decl
        (st', _ss, bs) <- serializeLinkableUnit m st topDeps ci si
                           . O.optimize
                           . jsSaturate (Just $ modulePrefix m n)
                           $ mconcat (reverse extraTl) <> tl
        return $! seqList topDeps `seq` seqList allDeps `seq` st' `seq`
          (st', LinkableUnit bs topDeps [] allDeps (S.toList extraDeps) required)

data SomeStaticPtr = SomeStaticPtr
  { sspId          :: Id
  , sspInfo        :: Id
  , sspTarget      :: Id
  , sspFingerprint :: Fingerprint
  }

initStaticPtrs :: [SomeStaticPtr] -> C
initStaticPtrs ptrs = mconcat <$> mapM initStatic ptrs
  where
    initStatic p = do
      i <- jsId (sspId p)
      let Fingerprint w1 w2 = sspFingerprint p
      fpa <- concat <$> mapM (genLit . MachWord64 . fromIntegral) [w1,w2]
      let sptInsert = ApplExpr (ValExpr (JVar (TxtI "h$hs_spt_insert"))) (fpa ++ [i])
      return [j| h$initStatic.push(function() {
                   `sptInsert`;
                 })
               |]

collectStaticInfo :: StgPgm -> [SomeStaticPtr]
#if __GLASGOW_HASKELL__ >= 709
collectStaticInfo pgm = eltsUFM (collect collectStaticPtr emptyUFM pgm)
  where
    fingerprints :: UniqFM Fingerprint
    fingerprints = collect collectFingerprint emptyUFM pgm

    collect :: (UniqFM a -> Id -> StgRhs -> UniqFM a)
            -> UniqFM a -> StgPgm -> UniqFM a
    collect f !m [] = m
    collect f !m (d:ds) = collect f (collectDecl f m d) ds
    collectDecl :: (UniqFM a -> Id -> StgRhs -> UniqFM a)
                -> UniqFM a -> StgBinding -> UniqFM a
    collectDecl f !m (StgNonRec b e) = f m b e
    collectDecl f !m (StgRec bs)     = foldl' (\m (b,e) -> f m b e) m bs

    collectFingerprint !m b
      (StgRhsCon _cc con [StgLitArg (MachWord64 w1), StgLitArg (MachWord64 w2)])
      | getUnique con == fingerprintDataConKey
      = addToUFM m b $ Fingerprint (fromIntegral w1) (fromIntegral w2)
    collectFingerprint !m _ _ = m

    collectStaticPtr !m b
      (StgRhsCon _cc con [StgVarArg fpId, StgVarArg info, StgVarArg tgt])
      | getUnique con == staticPtrDataConKey
      = let Just fp = lookupUFM fingerprints fpId
        in  addToUFM m b (SomeStaticPtr b info tgt fp)
    collectStaticPtr !m _ _ = m
#else
collectStaticInfo _pgm = []
#endif

hasExport :: StgBinding -> Bool
#if __GLASGOW_HASKELL__ >= 709
hasExport bnd =
  case bnd of
    StgNonRec b e -> isExportedBind b e
    StgRec bs     -> any (uncurry isExportedBind) bs
  where
    isExportedBind _i (StgRhsCon _cc con _) = getUnique con == staticPtrDataConKey
    isExportedBind _ _ = False
#else
hasExport _bnd = False
#endif

{- |
   serialize the payload of a linkable unit in the object file, adding
   strings to the SymbolTable where necessary
-}
serializeLinkableUnit :: Module
                      -> Object.SymbolTable  -- symbol table to start with
                      -> [Id]                -- id's exported by unit
                      -> [ClosureInfo]
                      -> [StaticInfo]
                      -> JStat               -- generated code for the unit
                      -> G (Object.SymbolTable, [Text], BL.ByteString)
serializeLinkableUnit _m st i ci si stat = do
  i' <- mapM idStr i
  let (st', o) = Object.serializeStat st ci si stat
  rnf i' `seq` rnf o `seq` return (st', i', o)
    where
      idStr i = itxt <$> jsIdI i

collectTopIds :: StgBinding -> [Id]
collectTopIds (StgNonRec b _) = [b]
collectTopIds (StgRec bs) = let xs = map (zapFragileIdInfo . fst) bs
                            in  seqList xs `seq` xs

collectIds :: UniqFM StgExpr -> StgBinding -> [Id]
collectIds unfloated b =
  let xs = map zapFragileIdInfo . filter acceptId $ S.toList (bindingRefs unfloated b)
  in  seqList xs `seq` xs
  where
    acceptId i = all ($ i) [not . isForbidden] -- fixme test this: [isExported[isGlobalId, not.isForbidden]
    -- the GHC.Prim module has no js source file
    isForbidden i
      | Just m <- nameModule_maybe (getName i) =
                    moduleNameText m   == T.pack "GHC.Prim" &&
                    modulePackageKey m == primPackageKey
      | otherwise = False

{- |
     generate the object's dependy data, taking care that package and module names
     are only stored once
 -}
genDependencyData :: DynFlags -> Module -> [LinkableUnit]
                  -> G Object.Deps
genDependencyData dflags mod units = do
    -- [(blockindex, blockdeps, required, exported)]
    ds <- evalStateT (sequence (map (uncurry oneDep) blocks))
                     (DDC IM.empty IM.empty M.empty)
    return $ Object.Deps (Linker.mkPackage $ modulePackageKey mod)
                         (moduleNameText mod)
                         (IS.fromList [ n | (n, _, True, _) <- ds ])
                         (M.fromList $ (\(n,_,_,es) -> map (,n) es) =<< ds)
                         (listArray (0, length blocks-1) (ds ^.. traverse . _2))
  where
      -- Id -> Block
      unitIdExports :: UniqFM Int
      unitIdExports = listToUFM $
                      concatMap (\(u,n) -> map (,n) (luIdExports u)) blocks

      -- OtherSymb -> Block
      unitOtherExports :: Map OtherSymb Int
      unitOtherExports = M.fromList $
                         concatMap (\(u,n) -> map (,n)
                                                  (map (OtherSymb mod)
                                                       (luOtherExports u)))
                                   blocks

      blocks :: [(LinkableUnit, Int)]
      blocks = zip units [0..]

      -- generate the list of exports and set of dependencies for one unit
      oneDep :: LinkableUnit
             -> Int
             -> StateT DependencyDataCache G (Int, Object.BlockDeps, Bool, [Object.Fun])
      oneDep (LinkableUnit _ idExports otherExports idDeps otherDeps req) n = do
        (edi, bdi) <- partitionEithers <$> mapM (lookupIdFun n) idDeps
        (edo, bdo) <- partitionEithers <$> mapM lookupOtherFun otherDeps
        expi <- mapM lookupExportedId (filter isExportedId idExports)
        expo <- mapM lookupExportedOther otherExports
        -- fixme thin deps, remove all transitive dependencies!
        let bdeps = Object.BlockDeps
                      (IS.toList . IS.fromList . filter (/=n) $ bdi++bdo)
                      (S.toList . S.fromList $ edi++edo)
                      [] -- fixme support foreign exported
        return (n, bdeps, req, expi++expo)

      idModule :: Id -> Maybe Module
      idModule i = nameModule_maybe (getName i) >>= \m ->
                   guard (m /= mod) >> return m

      -- get the function for an Id from the cache, add it if necessary
      -- result: Left Object.Fun   if function refers to another module
      --         Right blockNumber if function refers to current module
      --
      --         assumes function is internal to the current block if it's
      --         from teh current module and not in the unitIdExports map.
      lookupIdFun :: Int -> Id
                  -> StateT DependencyDataCache G (Either Object.Fun Int)
      lookupIdFun n i = case lookupUFM unitIdExports i of
        Just k  -> return (Right k)
        Nothing -> case idModule i of
          Nothing -> return (Right n)
          Just m ->
            let k = getKey . getUnique $ i
                addEntry :: StateT DependencyDataCache G Object.Fun
                addEntry = do
                  (TxtI idTxt) <- lift (jsIdI i)
                  lookupExternalFun (Just k) (OtherSymb m idTxt)
            in  if m == mod
                   then panic ("local id not found: " ++ show m)
                    else Left <$> (maybe addEntry return =<<
                                   use (ddcId . to (IM.lookup k)))

      -- get the function for an OtherSymb from the cache, add it if necessary
      lookupOtherFun :: OtherSymb
                     -> StateT DependencyDataCache G (Either Object.Fun Int)
      lookupOtherFun od@(OtherSymb m idTxt) =
        case M.lookup od unitOtherExports of
          Just n  -> return (Right n)
          Nothing | m == mod -> panic ("genDependencyData.lookupOtherFun: unknown local other id: " ++ T.unpack idTxt)
          Nothing ->  Left <$> (maybe (lookupExternalFun Nothing od) return =<<
                        use (ddcOther . to (M.lookup od)))

      lookupExportedId :: Id -> StateT DependencyDataCache G Object.Fun
      lookupExportedId i = do
        (TxtI idTxt) <- lift (jsIdI i)
        lookupExternalFun (Just . getKey . getUnique $ i) (OtherSymb mod idTxt)

      lookupExportedOther :: Text -> StateT DependencyDataCache G Object.Fun
      lookupExportedOther = lookupExternalFun Nothing . OtherSymb mod

      -- lookup a dependency to another module, add to the id cache if there's
      -- an id key, otherwise add to other cache
      lookupExternalFun :: Maybe Int
                        -> OtherSymb -> StateT DependencyDataCache G Object.Fun
      lookupExternalFun mbIdKey od@(OtherSymb m idTxt) = do
        let mk        = getKey . getUnique $ m
            mpk       = Linker.mkPackage (modulePackageKey m)
            inCache p = Object.Fun p (moduleNameText m) idTxt
            addCache  = do
              let cache' = IM.insert mk mpk
              ddcModule %= cache'
              cache' `seq` return (Object.Fun mpk (moduleNameText m) idTxt)
        f <- maybe addCache (return . inCache) =<<
                   use (ddcModule . to (IM.lookup mk))
        maybe (ddcOther %= M.insert od f) (\k -> ddcId %= IM.insert k f) mbIdKey
        return f

moduleNameText :: Module -> Text
moduleNameText m
  | xs == ":Main" = T.pack "Main"
  | otherwise     = T.pack xs
    where xs      = moduleNameString . moduleName $ m
{-
modulePackageText :: Module -> Object.Package
modulePackageText m
  | isWiredInPackage pkgStr = Object.Package n ""
  | otherwise               = Object.Package n v
  where
    pkgStr = packageKeyString (modulePackageKey m)
    (n, v) = Linker.splitVersion . T.pack . packageKeyString . modulePackageKey $ m
-}
genToplevel :: StgBinding -> C
genToplevel (StgNonRec bndr rhs) = genToplevelDecl bndr rhs
genToplevel (StgRec bs)          =
  mconcat $ map (\(bndr, rhs) -> genToplevelDecl bndr rhs) bs

-- entry function of the worker
enterDataCon :: DataCon -> G JExpr
enterDataCon d = jsDcEntryId (dataConWorkId d)

enterDataConI :: DataCon -> G Ident
enterDataConI d = jsDcEntryIdI (dataConWorkId d)

genToplevelDecl :: Id -> StgRhs -> C
genToplevelDecl i rhs = do
  s1 <- resetSlots (genToplevelConEntry i rhs)
  s2 <- resetSlots (genToplevelRhs i rhs)
  return (s1 <> s2)

genToplevelConEntry :: Id -> StgRhs -> C
genToplevelConEntry i (StgRhsCon _cc con _args)
    | i `elem` dataConImplicitIds con = genSetConInfo i con NoSRT
genToplevelConEntry i (StgRhsClosure _cc _bi [] _upd_flag srt _args (StgConApp dc _cargs))
    | i `elem` dataConImplicitIds dc = genSetConInfo i dc srt
genToplevelConEntry _ _ = mempty

genStaticRefs :: SRT -> G CIStatic
genStaticRefs NoSRT = return noStatic
genStaticRefs (SRTEntries s) = do
  unfloated <- use gsUnfloated
  let xs = filter (\x -> not $ elemUFM x unfloated) (uniqSetToList s)
  CIStaticRefs <$> mapM getStaticRef xs
genStaticRefs (SRT{}) =
  panic "genStaticRefs: unexpected SRT"

getStaticRef :: Id -> G Text
getStaticRef = fmap (itxt.head) . genIdsI

genToplevelRhs :: Id
               -> StgRhs
               -> C
-- special cases
genToplevelRhs i (StgRhsClosure cc _bi _ upd _ args body)
  -- foreign exports
  | (StgOpApp (StgFCallOp (CCall (CCallSpec (StaticTarget t _ _) _ _)) _)
     [StgLitArg (MachInt _is_js_conv), StgLitArg (MachStr _js_name), StgVarArg _tgt] _) <- body,
     t == fsLit "__mkExport" = return mempty -- fixme error "export not implemented"
  -- top-level strings
  | (StgApp upk [StgLitArg (MachStr bs)]) <- body, getUnique upk == unpackCStringIdKey
     && isUpdatable upd && null args = genStrThunk i False bs cc
  | (StgApp upk [StgLitArg (MachStr bs)]) <- body, getUnique upk == unpackCStringUtf8IdKey
     && isUpdatable upd && null args = genStrThunk i True bs cc
-- general cases:
genToplevelRhs i (StgRhsCon cc con args) = do
  ii <- jsIdI i
  allocConStatic ii cc con args
  return mempty
genToplevelRhs i (StgRhsClosure cc _bi [] _upd_flag srt args body) = do
  eid@(TxtI eidt) <- jsEnIdI i
  (TxtI idt)   <- jsIdI i
  body <- genBody (ExprCtx i [] emptyUniqSet emptyUniqSet emptyUFM []) i R2 args body
  sr <- genStaticRefs srt
  et <- genEntryType args
  (static, regs, upd) <-
        if et == CIThunk
          then (StaticThunk (Just eidt), CIRegs 0 [PtrV],) <$> updateThunk
          else return (StaticFun eidt, CIRegs 1 (concatMap idVt args), mempty)
  setcc <- ifProfiling $
             if et == CIThunk
               then enterCostCentreThunk
               else enterCostCentreFun cc
  emitClosureInfo (ClosureInfo eidt regs idt (CILayoutFixed 0 []) et sr)
  ccId <- costCentreStackLbl cc
  emitStatic idt static ccId
  return $ eid ||= JFunc [] (upd <> setcc <> body)
genToplevelRhs _ _ = panic "genToplevelRhs: top-level values cannot have live variables"

loadLiveFun :: [Id] -> C
loadLiveFun l = do
   l' <- concat <$> mapM genIdsI l
   case l' of
     []  -> return mempty
     [v] -> return (decl' v [je| `R1`.d1 |])
     [v1,v2] -> return (decl' v1 [je| `R1`.d1 |] <> decl' v2 [je| `R1`.d2 |])
     (v:vs)  -> do
       d <- makeIdent
       let l'' = mconcat . zipWith (loadLiveVar $ toJExpr d) [(1::Int)..] $ vs
       return (decl' v [je| `R1`.d1 |] <> [j| `decl d`; `d` = `R1`.d2 |] <> l'')
  where
        loadLiveVar d n v = let ident = dataFields ! n
                            in  decl' v (SelExpr d ident)

dataFields :: Array Int Ident
dataFields = listArray (1,1024) (map (TxtI . T.pack . ('d':) . show) [(1::Int)..1024])

genBody :: ExprCtx -> Id -> StgReg -> [Id] -> StgExpr -> C
genBody ctx i startReg args e = do
  la <- loadArgs startReg args
  let ids = take (resultSize args $ idType i) (map toJExpr $ enumFrom R1)
  (e, _r) <- genExpr (ctx & ctxTarget .~ ids) e
  return $ la <> e <> [j| return `Stack`[`Sp`]; |]

-- find the result type after applying the function to the arguments
resultSize :: [Id] -> Type -> Int
resultSize (x:xs) t
  | UbxTupleRep _ <- repType (idType x) = panic "genBody: unboxed tuple argument"
  | otherwise =
      case repType t of
       (UnaryRep t') | isFunTy t' ->
                         let (fa,fr) = splitFunTy t'
                             t''     = mkFunTys (flattenRepType $ repType fa) fr
                         in  resultSize xs (snd . splitFunTy $ t'')
       _                          -> 1 -- possibly newtype family, must be boxed
resultSize [] t =
  case repType t of
    UnaryRep t'     -> typeSize t'
    UbxTupleRep tys -> sum (map typeSize tys)

loadArgs :: StgReg -> [Id] -> C
loadArgs start args = do
  args' <- concatMapM genIdArgI args
  return (mconcat $ zipWith (||=) args' [start..])

data ExprResult = ExprCont
                | ExprInline (Maybe [JExpr])
  deriving (Eq, Ord, Show)

data ExprValData = ExprValData [JExpr]
  deriving (Eq, Ord, Show)

-- not a Monoid
branchResult :: [ExprResult] -> ExprResult
branchResult []           = panic "branchResult: empty list"
branchResult [e]          = e
branchResult (ExprCont:_) = ExprCont
branchResult (_:es)
  | any (==ExprCont) es   = ExprCont
  | otherwise             = ExprInline Nothing

genExpr :: ExprCtx -> StgExpr -> G (JStat, ExprResult)
genExpr top (StgApp f args) = genApp top f args
genExpr top (StgLit l) = (,ExprInline Nothing)
                             . assignAllCh ("genExpr StgLit " ++ show (top ^. ctxTarget))
                                           (top ^. ctxTarget)
                         <$> genLit l
genExpr top (StgConApp con args) = do
  as <- concatMapM genArg args
  c <- genCon top con as
  return (c, ExprInline (Just as))
genExpr top (StgOpApp (StgFCallOp f _) args t) =
   genForeignCall f t (top ^. ctxTarget) args
genExpr top (StgOpApp (StgPrimOp op) args t)    = genPrimOp top op args t
genExpr top (StgOpApp (StgPrimCallOp c) args t) = genPrimCall top c args t
genExpr _   (StgLam{}) = panic "genExpr: StgLam"
genExpr top (StgCase e _ liveRhs b srt at alts) = genCase top b e at alts liveRhs srt
genExpr top (StgLet b e) = do
  (b',top') <- genBind top b
  (s,r)     <- genExpr top' e
  return (b' <> s, r)
genExpr top (StgLetNoEscape _ live b e) = do
  (b', top') <- genBindLne top live b
  (s, r)     <- genExpr top' e
  return (b' <> s, r)
#if __GLASGOW_HASKELL__ < 709
genExpr top (StgSCC cc tick push e) = do
  setSCCstats <- ifProfilingM $ setCC cc tick push
  (stats, result) <- genExpr top e
  return (setSCCstats <> stats, result)
genExpr top (StgTick _m _n e) = genExpr top e
#else
genExpr top (StgTick (ProfNote cc count scope) e) = do
  setSCCstats <- ifProfilingM $ setCC cc count scope
  (stats, result) <- genExpr top e
  return (setSCCstats <> stats, result)
genExpr top (StgTick _m e) = genExpr top e
#endif

might_be_a_function :: Type -> Bool
-- Return False only if we are *sure* it's a data type
-- Look through newtypes etc as much as poss
might_be_a_function ty
  | UnaryRep rep <- repType ty
  , Just tc <- tyConAppTyCon_maybe rep
  , isDataTyCon tc
  = False
  | otherwise
  = True

genApp :: ExprCtx -> Id -> [StgArg] -> G (JStat, ExprResult)
-- special cases for unpacking C Strings, avoid going through a typed array when possible
genApp ctx i [StgLitArg (MachStr bs)]
    | [top] <- (ctx ^. ctxTarget), getUnique i == unpackCStringIdKey =
        (,ExprInline Nothing) . assignj top <$> do
          prof <- csProf <$> use gsSettings
          let profArg = if prof then [jCafCCS] else []
          return $ case decodeModifiedUTF8 bs of
            Just t  -> ApplExpr (jsv "h$ustra") $ [toJExpr t] ++ profArg
            Nothing -> ApplExpr (jsv "h$urstra") $ [toJExpr $ map (chr.fromIntegral) (B.unpack bs)] ++ profArg
    | [top] <- (ctx ^. ctxTarget), getUnique i == unpackCStringUtf8IdKey =
        (,ExprInline Nothing) . assignj top <$> do
          prof <- csProf <$> use gsSettings
          let profArg = if prof then [jCafCCS] else []
          return $ case decodeModifiedUTF8 bs of
            Just t  -> ApplExpr (jsv "h$ustr") $ [toJExpr t] ++ profArg
            Nothing -> ApplExpr (jsv "h$urstr") $ [toJExpr $ map toInteger (B.unpack bs)] ++ profArg
-- special cases for JSString literals
--    | [top] <- ctxTarget ctx, Just t <- decodeModifiedUtf8 bs, matchVarName "ghcjs-prim" "GHCJS.Prim.unpackJSString" i =
--        (,ExprInline Nothing) . assignj top <$> do
--    | [top] <- ctxTarget ctx, Just t <- decodeModifiedUtf8 bs, matchVarName "ghcjs-prim" "GHCJS.Prim.unpackJSStringUtf8" i =
--        (,ExprInline Nothing) . assignj top <$> do
 -- we could handle unpackNBytes# here, but that's probably not common
 -- enough to warrant a special case
genApp ctx i [StgLitArg (MachStr bs), x]
    | [top] <- (ctx ^. ctxTarget), getUnique i == unpackCStringAppendIdKey, Just d <- decodeModifiedUTF8 bs = do
        -- fixme breaks assumption in codegen if bs doesn't decode
        prof <- csProf <$> use gsSettings
        let profArg = if prof then [jCafCCS] else []
        a <- genArg x
        return ([j| `top` = `ApplExpr (jsv "h$appendToHsStringA") $ [toJExpr d, toJExpr a] ++ profArg`; |]
               ,ExprInline Nothing)
genApp top i a
    | Just n <- top ^. ctxLneFrameBs . to (flip lookupUFM i) = do -- let-no-escape
        as'      <- concatMapM genArg a
        ei       <- jsEntryId i
        let ra = mconcat . reverse $
                   zipWith (\r a -> [j| `r` = `a`; |]) [R1 ..] as'
        p <- pushLneFrame n top
        a <- adjSp 1 -- for the header (which will only be written when the thread is suspended)
        return (ra <> p <> a <> [j| return `ei`; |], ExprCont)
    | n == 0 && (isUnboxedTupleType (idType i) || isStrictType (idType i)) = do
                a <- assignAllCh "genApp" (top ^. ctxTarget) <$> genIds i
                return (a, ExprInline Nothing)
    | [vt] <- idVt i, isUnboxable vt && n == 0 && i `elementOfUniqSet` (top ^. ctxEval) = do
                let [c] =  top ^. ctxTarget
                [i'] <- genIds i
                return ([j| `c` = (typeof `i'` === 'object') ? `i'`.d1 : `i'`; |]
                       ,ExprInline Nothing)
    | n == 0 && (i `elementOfUniqSet` (top ^. ctxEval) || isStrictId i) = do
                a <- assignAllCh ("genApp:" ++ show i ++ " " ++ show (idRepArity i, idVt i))
                                 (top ^. ctxTarget)
                                 <$> genIds i
                settings <- use gsSettings
                let ww = case top ^. ctxTarget of
                           [t] | csAssertRts settings ->
                                   [j| if(typeof `t` === 'object' && `isThunk t`)
                                         throw "unexpected thunk";
                                     |]
                           _   -> mempty
                return (a <> ww, ExprInline Nothing)
    | DataConWrapId dc <- idDetails i, isNewTyCon (dataConTyCon dc) = do
                [ai] <- concatMapM genArg a
                let [t] = top ^. ctxTarget
                    [StgVarArg a'] = a
                if isStrictId a' || a' `elementOfUniqSet` (top ^. ctxEval)
                  then return ([j| `t` = `ai`; |], ExprInline Nothing)
                  else return ([j| return h$e(`ai`); |], ExprCont)
    | idRepArity i == 0 && n == 0 && not (might_be_a_function (idType i)) = do
             ii <- enterId
             return ([j| return h$e(`ii`) |], ExprCont)
    | idRepArity i == n && not (isLocalId i) && isStrictId i && n /= 0 = do
        as' <- concatMapM genArg a
        jmp <- jumpToII i as' =<< r1
        return (jmp, ExprCont)
    | idRepArity i < n && isStrictId i && idRepArity i > 0 =
         let (reg,over) = splitAt (idRepArity i) a
         in  do
           reg' <- concatMapM genArg reg
           pc   <- pushCont over
           jmp  <- jumpToII i reg' =<< r1
           return (pc <> jmp, ExprCont)
    | otherwise = do
           jmp <- jumpToFast a =<< r1
           return (jmp, ExprCont)
  where
    enterId :: G JExpr
    enterId = genArg (StgVarArg i) >>=
                \case
                   [x] -> return x
                   _   -> panic "genApp: unexpected multi-var argument"

    r1 :: C
    r1 = do
      ids <- genIds i
      return $ mconcat $ zipWith (\r u -> [j| `r`=`u`; |]) (enumFrom R1) ids
    n = length a

pushCont :: [StgArg] -> C
pushCont as = do
  as' <- concatMapM genArg as
  (app, spec) <- selectApply False (as,as')
  if spec
    then push $ reverse $ app : as'
    else push $ reverse $ app : mkTag as' as : as'
  where
    mkTag rs ns = toJExpr ((length rs `shiftL` 8) .|. length ns)

-- regular let binding: allocate heap object
genBind :: ExprCtx -> StgBinding -> G (JStat, ExprCtx)
genBind ctx bndr =
  case bndr of
    StgNonRec b r -> do
       j <- assign b r >>= \case
         Just ja -> return ja
         Nothing -> allocCls Nothing [(b,r)]
       return (j, addEvalRhs ctx [(b,r)])
    StgRec bs     -> do
       jas <- mapM (uncurry assign) bs -- fixme these might depend on parts initialized by allocCls
       let m = if null jas then Nothing else Just (mconcat $ catMaybes jas)
       j <- allocCls m . map snd . filter (isNothing . fst) $ zip jas bs
       return (j, addEvalRhs ctx bs)
   where
     ctx' = clearCtxStack ctx

     assign :: Id -> StgRhs -> G (Maybe JStat)
     assign b (StgRhsClosure _ccs _bi _free _upd _str [] expr)
       | snd (isInlineExpr (ctx ^. ctxEval) expr) = do
           d   <- declIds b
           tgt <- genIds b
           (j, _) <- genExpr (ctx & ctxTarget .~ tgt) expr
           return (Just (d <> j))
     assign b (StgRhsCon{}) = return Nothing
     assign b r             = genEntry ctx' b r >> return Nothing

     addEvalRhs c [] = c
     addEvalRhs c ((b,r):xs)
       | (StgRhsCon{}) <- r                         = addEvalRhs (addEval b c) xs
       | (StgRhsClosure _ _ _ ReEntrant _ _ _) <- r = addEvalRhs (addEval b c) xs
       | otherwise                                  = addEvalRhs c xs

genBindLne :: ExprCtx -> StgLiveVars -> StgBinding -> G (JStat, ExprCtx)
genBindLne ctx live bndr = do
  vis  <- map (\(x,y,_) -> (x,y)) <$>
            optimizeFree oldFrameSize (newLvs++map fst updBinds)
  declUpds <- mconcat <$> mapM (fmap (||= jnull) . jsIdI . fst) updBinds
  let newFrameSize = oldFrameSize + length vis
      ctx' = ctx & ctxLne        %~ flip addListToUniqSet bound
                 & ctxLneFrameBs %~ flip addListToUFM (map (,newFrameSize) bound)
                 & ctxLneFrame   %~ (++vis)
  mapM_ (uncurry $ genEntryLne ctx') binds
  return (declUpds, ctx')
  where
    oldFrame     = ctx ^. ctxLneFrame
    oldFrameSize = length oldFrame
    isOldLv i    = i `elementOfUniqSet` (ctx ^. ctxLne) || i `elem` (map fst oldFrame)
    newLvs       = filter (not . isOldLv) (uniqSetToList live)
    binds = case bndr of
              StgNonRec b e -> [(b,e)]
              StgRec    bs  -> bs
    bound = map fst binds
    (updBinds, _nonUpdBinds) = partition (isUpdatableRhs . snd) binds

isUpdatableRhs :: StgRhs -> Bool
isUpdatableRhs (StgRhsClosure _ _ _ u _ _ _) = isUpdatable u
isUpdatableRhs _                             = False

{-
  Let-no-escape entries live on the stack. There is no heap object associated with them.

  A let-no-escape entry is called like a normal stack frame, although as an optimization,
  `Stack`[`Sp`] is not set when making the call. This done later if the thread needs to
  be suspended.

  Updatable let-no-escape binders have one 'private' slot in the stack frame. This slot
  is initially set to null, changed to h$blackhole when the thunk is being evaluated.
 -}

genEntryLne :: ExprCtx -> Id -> StgRhs -> G ()
genEntryLne ctx i (StgRhsClosure _cc _bi _live2 update srt args body) = resetSlots $ do
  let payloadSize = length frame
      frame       = ctx ^. ctxLneFrame
      myOffset    =
        maybe (panic "genEntryLne: updatable binder not found in let-no-escape frame")
              ((payloadSize-) . fst)
              (listToMaybe $ filter ((==i).fst.snd) (zip [0..] frame))
      bh | isUpdatable update =
             [j| var x = h$bh_lne(`Sp`-`myOffset`, `payloadSize+1`);
                 if(x) return(x);
               |]
         | otherwise = mempty
  lvs  <- popLneFrame True payloadSize ctx
  body <- genBody ctx i R1 args body
  ei   <- jsEntryIdI i
  sr   <- genStaticRefs srt
  let f = JFunc [] (bh <> lvs <> body)
  emitClosureInfo $
    ClosureInfo (itxt ei)
                (CIRegs 0 $ concatMap idVt args)
                (itxt ei <> ", " <> T.pack (show i))
                (fixedLayout . reverse $
                    map (stackSlotType . fst) (ctx ^. ctxLneFrame))
                CIStackFrame
                sr
  emitToplevel (ei ||= f)
genEntryLne ctx i (StgRhsCon cc con args) = resetSlots $ do
  let payloadSize = length (ctx ^. ctxLneFrame)
  ei <- jsEntryIdI i
  di <- enterDataCon con
  ii <- makeIdent
  p  <- popLneFrame True payloadSize ctx
  args' <- concatMapM genArg args
  ac    <- allocCon ii con cc args'
  emitToplevel $ ei ||= JFunc []
    (decl ii <> p <> ac <> [j| `R1` = `ii`; return `Stack`[`Sp`]; |])

-- generate the entry function for a local closure
genEntry :: ExprCtx -> Id -> StgRhs -> G ()
genEntry _ _i (StgRhsCon _cc _con _args) = return () -- mempty -- error "local data entry"

genEntry ctx i (StgRhsClosure cc _bi live upd_flag srt args body) = resetSlots $ do
  ll    <- loadLiveFun live
  upd   <- genUpdFrame upd_flag i
  body  <- genBody entryCtx i R2 args body
  ei    <- jsEntryIdI i
  et    <- genEntryType args
  setcc <- ifProfiling $
             if et == CIThunk
               then enterCostCentreThunk
               else enterCostCentreFun cc
  sr <- genStaticRefs srt
  emitClosureInfo $ ClosureInfo (itxt ei)
                                (CIRegs 0 $ PtrV : concatMap idVt args)
                                (itxt ei <> ", " <> T.pack (show i))
                                (fixedLayout $ map (uTypeVt . idType) live)
                                et
                                sr
  emitToplevel (ei ||= JFunc [] (ll <> upd <> setcc <> body))
  where
    entryCtx = ExprCtx i [] (ctx ^. ctxEval) (ctx ^. ctxLne) emptyUFM []

genEntryType :: [Id] -> G CIType
genEntryType []   = return CIThunk
genEntryType args = do
  args' <- mapM genIdArg args
  return $ CIFun (length args) (length $ concat args')

genSetConInfo :: Id -> DataCon -> SRT -> C
genSetConInfo i d srt = do
  ei <- jsDcEntryIdI i
  sr <- genStaticRefs srt
  emitClosureInfo $ ClosureInfo (itxt ei)
                                (CIRegs 0 [PtrV])
                                (T.pack $ show d)
                                (fixedLayout $ map uTypeVt fields)
                                (CICon $ dataConTag d)
                                sr
  return (ei ||= mkDataEntry)
    where
      -- dataConRepArgTys sometimes returns unboxed tuples. is that a bug?
      fields = concatMap (flattenRepType . repType) (dataConRepArgTys d)

mkDataEntry :: JExpr
mkDataEntry = ValExpr $ JFunc [] [j| return `Stack`[`Sp`]; |]

genUpdFrame :: UpdateFlag -> Id -> C
genUpdFrame u i
  | isReEntrant u   = mempty
  | isOneShotBndr i = maybeBh
  | isUpdatable u   = updateThunk
  | otherwise       = maybeBh
  where
    isReEntrant ReEntrant = True
    isReEntrant _         = False
    maybeBh = do
      settings <- use gsSettings
      assertRtsStat (return $ bhSingleEntry settings)

-- allocate local closures
allocCls :: Maybe JStat -> [(Id, StgRhs)] -> C
allocCls dynMiddle xs = do
   (stat, dyn) <- splitEithers <$> mapM toCl xs
   cs <- use gsSettings
   return (mconcat stat) <> allocDynAll cs True dynMiddle dyn
  where
    -- left = static, right = dynamic
    toCl :: (Id, StgRhs)
         -> G (Either JStat (Ident,JExpr,[JExpr],CostCentreStack))
    -- statics
    toCl (i, StgRhsCon cc con []) = do
      ii <- jsIdI i
      Left <$> (return (decl ii) <> allocCon ii con cc [])
    toCl (i, StgRhsCon cc con [a]) | isUnboxableCon con = do
      ii <- jsIdI i
      Left <$> (return (decl ii) <> (allocCon ii con cc =<< genArg a))

    -- dynamics
    toCl (i, StgRhsCon cc con ar) =
      -- fixme do we need to handle unboxed?
      Right <$> ((,,,) <$> jsIdI i <*> enterDataCon con <*> concatMapM genArg ar <*> pure cc)
    toCl (i, StgRhsClosure cc _bi live _upd_flag _srt _args _body) =
      Right <$> ((,,,) <$> jsIdI i <*> jsEntryId i <*> concatMapM genIds live <*> pure cc)

-- fixme CgCase has a reps_compatible check here
genCase :: ExprCtx -> Id -> StgExpr -> AltType -> [StgAlt] -> StgLiveVars -> SRT -> G (JStat, ExprResult)
genCase top bnd e at alts l srt
  | snd (isInlineExpr (top ^. ctxEval) e) = withNewIdent $ \ccsVar -> do
      bndi <- genIdsI bnd
      (ej, r) <- genExpr (top & ctxTop .~ bnd & ctxTarget .~ map toJExpr bndi) e -- ExprCtx bnd (map toJExpr bndi) (top ^. ctxEval) (top ^. ctxLneV) (top ^. ctxLneB) (top ^. ctxLne)) e
      let d = case r of
                ExprInline d0 -> d0
                ExprCont -> panic $ "genCase: expression was not inline:\n" ++ show e
          ww = mempty -- if snd (isInlineExpr emptyUniqSet e) then mempty else [j| h$log('danger will robinson'); |]
      (aj, ar) <- genAlts (addEval bnd top) bnd at d alts
      saveCCS <- ifProfiling $ ccsVar |= jCurrentCCS
      restoreCCS <- ifProfiling $ [j| `jCurrentCCS` = `ccsVar`; |]
      return (decl ccsVar <> mconcat (map decl bndi) <> saveCCS <> ww <> ej <> restoreCCS <> aj, ar)
  | otherwise = do
      n        <- length <$> genIdsI bnd
      rj       <- genRet (addEval bnd top) bnd at alts l srt
      (ej, _r) <- genExpr (top & ctxTop .~ bnd & ctxTarget .~ take n (map toJExpr [R1 ..])) e
      return (rj <> ej, ExprCont)

assignAll :: (ToJExpr a, ToJExpr b) => [a] -> [b] -> JStat
assignAll xs ys = mconcat (zipWith assignj xs ys)

assignAllCh :: (ToJExpr a, ToJExpr b) => String -> [a] -> [b] -> JStat
assignAllCh msg xs ys
  | length xs == length ys = mconcat (zipWith assignj xs ys)
  | otherwise              = panic $ "assignAllCh: lengths do not match: " ++ show (length xs, length ys) ++ "\n    " ++ msg

genRet :: ExprCtx -> Id -> AltType -> [StgAlt] -> StgLiveVars -> SRT -> C
genRet ctx e at as l srt = withNewIdent f
  where
    allRefs :: [Id]
    allRefs =  S.toList . S.unions $ as ^.. traverse . _4 . to (exprRefs emptyUFM)
    lneLive :: Int
    lneLive    = maximum $ 0 : map (fromMaybe 0 . lookupUFM (ctx ^. ctxLneFrameBs)) allRefs
    ctx'       = adjustCtxStack lneLive ctx
    lneVars    = map fst $ take lneLive (ctx ^. ctxLneFrame)
    isLne i    = i `elem` lneVars || i `elementOfUniqSet` (ctx ^. ctxLne)
    nonLne     = filter (not . isLne) (uniqSetToList l)

    f :: Ident -> C
    f r    =  do
      pushLne  <- pushLneFrame lneLive ctx
      saveCCS  <- ifProfilingM $ push [jCurrentCCS]
      free     <- optimizeFree 0 nonLne
      pushRet  <- pushRetArgs free (iex r)
      fun'     <- fun free
      sr       <- genStaticRefs srt
      prof     <- profiling
      emitClosureInfo $
        ClosureInfo (itxt r)
                    (CIRegs 0 altRegs)
                    (itxt r)
                    (fixedLayout . reverse $
                       map (stackSlotType . fst3) free
                       ++ if prof then [ObjV] else []
                       ++ map stackSlotType lneVars)
                    CIStackFrame
                    sr
      emitToplevel $ r ||= JFunc [] fun'
      return (pushLne <> saveCCS <> pushRet)
    fst3 ~(x,_,_)  = x

    altRegs = case at of
      PrimAlt ptc  -> tyConVt ptc
      UbxTupAlt _n -> idVt e
      _            -> [PtrV]

    fun free = resetSlots $ do
      decs          <- declIds e
      load          <- flip assignAll [R1 ..] <$> genIdsI e
      ras           <- loadRetArgs free
      restoreCCS    <- ifProfilingM $ popUnknown [jCurrentCCS]
      rlne          <- popLneFrame False lneLive ctx'
      (alts, _altr) <- genAlts ctx' e at Nothing as
      return $ decs <> load  <> ras <> restoreCCS <> rlne <> alts <>
               [j| return `Stack`[`Sp`]; |]


-- 2-var values might have been moved around separately, use DoubleV as substitute
-- ObjV is 1 var, so this is no problem for implicit metadata
stackSlotType :: Id -> VarType
stackSlotType i
  | varSize otype == 1 = otype
  | otherwise          = DoubleV
  where otype = uTypeVt (idType i)

popLneFrame :: Bool -> Int -> ExprCtx -> C
popLneFrame inEntry size ctx
  | l < size  = panic $ "popLneFrame: let-no-escape frame too short: " ++
                        show l ++ " < " ++ show size
  | otherwise = popSkipI skip
                  =<< mapM (\(i,n) -> (,SlotId i n) <$> genIdsIN i n)
                           (take size $ ctx ^. ctxLneFrame)
  where
    skip = if inEntry then 1 else 0 -- pop the frame header
    l    = ctx ^. ctxLneFrame . to length

pushLneFrame :: Int -> ExprCtx -> C
pushLneFrame size ctx
  | l < size  = panic $ "pushLneFrame: let-no-escape frame too short " ++
                        show l ++ " < " ++ show size
  | otherwise = pushOptimized' (take size $ ctx ^. ctxLneFrame)
  where
    l = ctx ^. ctxLneFrame . to length

-- reorder the things we need to push to reuse existing stack values as much as possible
-- True if already on the stack at that location
optimizeFree :: Int -> [Id] -> G [(Id,Int,Bool)]
optimizeFree offset ids = do
  let ids' = concat $ map (\i -> map (i,) [1..varSize . uTypeVt . idType $ i]) ids
      l    = length ids'
  slots <- drop offset . take l . (++repeat SlotUnknown) <$> getSlots
  let slm                = M.fromList (zip slots [0..])
      (remaining, fixed) = partitionEithers $
         map (\inp@(i,n) -> maybe (Left inp) (\j -> Right (i,n,j,True))
            (M.lookup (SlotId i n) slm)) ids'
      takenSlots         = S.fromList (fixed ^.. traverse . _3)
      freeSlots          = filter (`S.notMember` takenSlots) [0..l-1]
      remaining'         = zipWith (\(i,n) j -> (i,n,j,False)) remaining freeSlots
      allSlots           = sortBy (compare `on` \(_,_,x,_) -> x) (fixed ++ remaining')
  return $ map (\(i,n,_,b) -> (i,n,b)) allSlots


pushRetArgs :: [(Id,Int,Bool)] -> JExpr -> C
pushRetArgs free fun = do
  p <- pushOptimized . (++[(fun,False)]) =<< mapM (\(i,n,b) -> (\es->(es!!(n-1),b)) <$> genIdArg i) free
--  p <- push . (++[fun]) =<< mapM (\(i,n,b) -> (\es->es!!(n-1)) <$> genIdArg i) free
  return p
{-    where
      showSlot SlotUnknown = return "unknown"
      showSlot (SlotId i n) = do
        (TxtI i') <- jsIdI i
        return (T.unpack i'++"("++show n ++ ")")
-}

loadRetArgs :: [(Id,Int,Bool)] -> C
loadRetArgs free = popSkipI 1 =<< ids
    where
       ids = mapM (\(i,n,_b) -> (!!(n-1)) <$> genIdStackArgI i) free

genAlts :: ExprCtx        -- ^ lhs to assign expression result to
        -> Id             -- ^ id being matched
        -> AltType        -- ^ type
        -> Maybe [JExpr]  -- ^ if known, fields in datacon from earlier expression
        -> [StgAlt]       -- ^ the alternatives
        -> G (JStat, ExprResult)
genAlts top e PolyAlt _ [alt] = (\(_,s,r) -> (s,r)) <$> mkAlgBranch top e alt
genAlts _   _ PolyAlt _ _ = error "genAlts: multiple polyalt"
genAlts top e (PrimAlt _tc) _ [(_, bs, _use, expr)] = do
  ie       <- genIds e
  dids     <- mconcat (map declIds bs)
  bss      <- concatMapM genIds bs
  (ej, er) <- genExpr top expr
  return (dids <> assignAll {- Ch ("genAlts PrimAlt: " ++ show (idType e)) -} bss ie <> ej, er)
genAlts top e (PrimAlt tc) _ alts = do
  ie <- genIds e
  (r, bss) <- normalizeBranches top <$> mapM (isolateSlots . mkPrimIfBranch top (tyConVt tc)) alts
  setSlots []
  return (mkSw ie bss, r)
genAlts top e (UbxTupAlt n) _ [(_, bs, _use, expr)] = do
  eids     <- genIds e
  l        <- loadUbxTup eids bs n
  (ej, er) <- genExpr top expr
  return (l <> ej, er)
genAlts _   _ (AlgAlt tc) _ [_alt] | isUnboxedTupleTyCon tc = error "genAlts: unexpected unboxed tuple"
genAlts top _ (AlgAlt _tc) (Just es) [(DataAlt dc, bs, use, expr)] | not (isUnboxableCon dc) = do
  bsi <- mapM genIdsI bs
  let bus  = concat $ zipWith (\bss u -> zip bss (repeat u)) bsi use
      args = zipWith (\(i,u) de -> if u then i ||= de else mempty) bus es
  (ej, er) <- genExpr top expr
  return (mconcat args <> ej, er)
genAlts top e (AlgAlt _tc) _ [alt] = do
  (_,s,r) <- mkAlgBranch top e alt
  return (s, r)
genAlts top e (AlgAlt _tc) _ alts@[(DataAlt dc,_,_,_),_]
  | isBoolTy (dataConType dc) = do
      i <- jsId e
      (r, [(_,s1,_), (_,s2,_)]) <- normalizeBranches top <$> mapM (isolateSlots . mkAlgBranch top e) alts
      let s = if dataConTag dc == 2 then [j| if(`i`) { `s1` } else { `s2` } |]
                                    else [j| if(`i`) { `s2` } else { `s1` } |]
      setSlots []
      return (s, r)
-- fixme, add all alts
genAlts top e (AlgAlt _tc) _ alts = do
      ei <- jsId e
      (r, brs) <- normalizeBranches top <$> mapM (isolateSlots . mkAlgBranch top e) alts
      setSlots []
      return (mkSwitch [je| `ei`.f.a |] brs, r)
genAlts _  _ a _ l = do
  ap <- showPpr' a
  error $ "genAlts: unhandled case variant: " ++ ap ++ " (" ++ show (length l) ++ ")"

-- if one branch ends in a continuation but another is inline, we need to adjust the inline branch
-- to use the continuation convention
normalizeBranches :: ExprCtx -> [(a, JStat, ExprResult)] -> (ExprResult, [(a, JStat, ExprResult)])
normalizeBranches e brs
    | all (==ExprCont) (brs ^.. traverse . _3)         = (ExprCont, brs)
    | branchResult (brs ^.. traverse  ._3) == ExprCont = (ExprCont, map mkCont brs)
    | otherwise                                        = (ExprInline Nothing, brs)
  where
    mkCont (me, s, ExprInline{}) = (me, s <> assignAll (enumFrom R1) (e ^. ctxTarget), ExprCont)
    mkCont x                     = x

loadUbxTup :: [JExpr] -> [Id] -> Int -> C
loadUbxTup es bs _n = do
  bs' <- concatMapM genIdsI bs
  return $ mconcat $ zipWith (||=) bs' es

mkSw :: [JExpr] -> [(Maybe [JExpr], JStat, ExprResult)] -> JStat
mkSw [e] cases = mkSwitch e (over (mapped._1.mapped) head cases)
mkSw es cases  = mkIfElse es cases

-- switch for pattern matching on constructors or prims
mkSwitch :: JExpr -> [(Maybe JExpr, JStat, ExprResult)] -> JStat
mkSwitch e cases
    | [(Just c1,s1,_)] <- n, [(_,s2,_)] <- d = IfStat [je| `e` === `c1` |] s1 s2
    | [(Just c1,s1,_),(_,s2,_)] <- n, null d = IfStat [je| `e` === `c1` |] s1 s2
    | null d                                 = SwitchStat e (map addBreak (init n)) (last n ^. _2)
    | [(_,d0,_)] <- d                        = SwitchStat e (map addBreak n) d0
    | otherwise                              = error "mkSwitch: multiple default cases"
    where
      addBreak (Just c, s, _) = (c, s <> [j| break; |])
      addBreak _              = error "mkSwitch: addBreak"
      (n,d) = partition (isJust . (^. _1)) cases

-- if/else for pattern matching on things that js cannot switch on
mkIfElse :: [JExpr] -> [(Maybe [JExpr], JStat, ExprResult)] -> JStat
mkIfElse e s = go (reverse $ sort s)
    where
      go [] = error "mkIfElse: empty expression list"
      go [(_, s, _)] = s -- only one 'nothing' allowed
      go ((Just e0, s, _):xs) =
          [j| if( `mkEq e e0` ) { `s` } else { `go xs` } |]
      go _ = error "mkIfElse: multiple DEFAULT cases"

mkEq :: [JExpr] -> [JExpr] -> JExpr
mkEq es1 es2
  | length es1 == length es2 = foldl1 and (zipWith eq es1 es2)
  | otherwise                = error "mkEq: incompatible expressions"
    where
      and e1 e2 = [je| `e1` && `e2`  |]
      eq  e1 e2 = [je| `e1` === `e2` |]


mkAlgBranch :: ExprCtx  -- ^ toplevel id for the result
            -> Id      -- ^ datacon to match
            -> StgAlt  -- ^ match alternative with binders
            -> G (Maybe JExpr, JStat, ExprResult)
mkAlgBranch top d (DataAlt dc,[b],_,expr)
  | isUnboxableCon dc = do
      idd      <- jsId d
      [fld]    <- genIdsI b
      (ej, er) <- genExpr top expr
      return (Nothing, decl fld <> [j| `fld` = `idd` |] <> ej, er)
mkAlgBranch top d (a,bs,use,expr) = do
  cc       <- caseCond a
  idd      <- jsId d
  b        <- loadParams idd bs use
  (ej, er) <- genExpr top expr
  return (cc, b <> ej, er)

-- single-var prim
{-
mkPrimBranch :: ExprCtx -> [VarType] -> StgAlt -> G (Maybe JExpr, JStat, ExprResult)
mkPrimBranch top _vt (cond, _bs, _us, e) =
  (\cc (ej,er) -> (cc,ej,er)) <$> caseCond cond <*> genExpr top e
-}

mkPrimIfBranch :: ExprCtx -> [VarType] -> StgAlt -> G (Maybe [JExpr], JStat, ExprResult)
mkPrimIfBranch top _vt (cond, _bs, _us, e) =
  (\ic (ej,er) -> (ic,ej,er)) <$> ifCond cond <*> genExpr top e

-- fixme are bool things always checked correctly here?
ifCond :: AltCon -> G (Maybe [JExpr])
ifCond (DataAlt da) = return $ Just [[je| `dataConTag da` |]]
ifCond (LitAlt l)   = Just <$> genLit l
ifCond DEFAULT      = return Nothing

caseCond :: AltCon -> G (Maybe JExpr)
caseCond (DataAlt da) = return $ Just [je| `dataConTag da` |]
caseCond (LitAlt l)   = Just <$> genSingleLit l
caseCond DEFAULT      = return Nothing

-- load parameters from constructor
-- fixme use single tmp var for all branches
loadParams :: JExpr -> [Id] -> [Bool] -> C
loadParams from args use = do
  as <- concat <$> sequence (zipWith (\a u -> map (,u) <$> genIdsI a) args use)
  return $ case as of
    []                 -> mempty
    [(x,u)]            -> loadIfUsed [je| `from`.d1 |] x  u
    [(x1,u1),(x2,u2)]  -> loadIfUsed [je| `from`.d1 |] x1 u1 <>
                          loadIfUsed [je| `from`.d2 |] x2 u2
    ((x,u):xs)         -> loadIfUsed [je| `from`.d1 |] x  u  <>
                          [j| var d = `from`.d2;
                              `loadConVarsIfUsed d xs`;
                            |]
  where
    loadIfUsed fr tgt True = decl' tgt fr
    loadIfUsed  _ _ _  = mempty

    loadConVarsIfUsed fr cs = mconcat $ zipWith f cs [(1::Int)..]
      where f (x,u) n = loadIfUsed (SelExpr fr (dataFields ! n)) x u

genPrimOp :: ExprCtx -> PrimOp -> [StgArg] -> Type -> G (JStat, ExprResult)
genPrimOp top op args t = do
  as <- concatMapM genArg args
  df <- use gsDynFlags
  return $ case genPrim df t op (map toJExpr $ top ^. ctxTarget) as of
             PrimInline s -> (s, ExprInline Nothing)
             PRPrimCall s -> (s, ExprCont)
{-
genStackArg :: StgArg -> G [(JExpr, StackSlot)]
genStackArg a@(StgLitArg _) = map (,SlotUnknown) <$> genArg a
genStackArg a@(StgVarArg i) = zipWith f [1..] <$> genArg a
  where
    f :: Int -> JExpr -> (JExpr, StackSlot)
    f n e = (e, SlotId i n)
-}

genArg :: StgArg -> G [JExpr]
genArg (StgLitArg l) = genLit l
genArg a@(StgVarArg i) = do
  unFloat <- use gsUnfloated
  case lookupUFM unFloat i of
    Nothing -> reg
    Just expr -> unfloated expr
   where
     r = uTypeVt . stgArgType $ a
     reg
       | isVoid r     = return []
       | i == trueDataConId  = return [ [je| true  |] ]
       | i == falseDataConId = return [ [je| false |] ]
       | isMultiVar r = mapM (jsIdN i) [1..varSize r]
       | otherwise    = (:[]) <$> jsId i

     unfloated :: StgExpr -> G [JExpr]
     unfloated (StgLit l) = genLit l
     unfloated (StgConApp dc args)
       | isBoolTy (dataConType dc) || isUnboxableCon dc =
           (:[]) . allocUnboxedCon dc . concat <$> mapM genArg args
       | null args = (:[]) <$> jsId (dataConWorkId dc)
       | otherwise = do
           as <- concat <$> mapM genArg args
           e  <- enterDataCon dc
           cs <- use gsSettings
           return [allocDynamicE cs e as Nothing] -- FIXME: ccs
     unfloated x = error ("genArg: unexpected unfloated expression: " ++ show x)

genStaticArg :: StgArg -> G [StaticArg]
genStaticArg (StgLitArg l) = map StaticLitArg <$> genStaticLit l
genStaticArg a@(StgVarArg i) = do
  unFloat <- use gsUnfloated
  case lookupUFM unFloat i of
    Nothing -> reg
    Just expr -> unfloated expr
   where
     r = uTypeVt . stgArgType $ a
     reg
       | isVoid r            = return []
       | i == trueDataConId  = return [StaticLitArg (BoolLit True)]
       | i == falseDataConId = return [StaticLitArg (BoolLit False)]
       | isMultiVar r        = map (\(TxtI t) -> StaticObjArg t) <$> mapM (jsIdIN i) [1..varSize r] -- this seems wrong, not an obj?
       | otherwise           = (\(TxtI it) -> [StaticObjArg it]) <$> jsIdI i

     unfloated :: StgExpr -> G [StaticArg]
     unfloated (StgLit l) = map StaticLitArg <$> genStaticLit l
     unfloated (StgConApp dc args)
       | isBoolTy (dataConType dc) || isUnboxableCon dc =
           (:[]) . allocUnboxedConStatic dc . concat <$> mapM genStaticArg args -- fixme what is allocunboxedcon?
       | null args = (\(TxtI t) -> [StaticObjArg t]) <$> jsIdI (dataConWorkId dc)
       | otherwise = do
           as       <- concat <$> mapM genStaticArg args
           (TxtI e) <- enterDataConI dc
           return [StaticConArg e as]
     unfloated x = error ("genArg: unexpected unfloated expression: " ++ show x)

allocateStaticList :: [StgArg] -> StgArg -> G StaticVal
allocateStaticList xs a@(StgVarArg i)
  | isDataConId_maybe i == Just nilDataCon = listAlloc xs Nothing
  | otherwise = do
      unFloat <- use gsUnfloated
      case lookupUFM unFloat i of
        Just (StgConApp dc [h,t])
          | dc == consDataCon -> allocateStaticList (h:xs) t
        _ -> listAlloc xs (Just a)
  where
    listAlloc :: [StgArg] -> Maybe StgArg -> G StaticVal
    listAlloc xs Nothing  = do
      as <- concat . reverse <$> mapM genStaticArg xs
      return (StaticList as Nothing)
    listAlloc xs (Just r) = do
      as <- concat . reverse <$> mapM genStaticArg xs
      r' <- genStaticArg r
      case r' of
        [StaticObjArg ri] -> return (StaticList as (Just ri))
        _                 -> error ("allocateStaticList: invalid argument (tail): " ++ show xs ++ " " ++ show r)
allocateStaticList _ _ = error "allocateStaticList: unexpected literal in list"

-- generate arg to be passed to FFI call, with marshalling JStat to be run before the call
-- currently marshalling:
--   String literals passed as real JS string
--   Ptr ghcjs-base.GHCJS.Types.JSChar -> JavaScript String
genFFIArg :: StgArg -> G (JStat, [JExpr])
genFFIArg (StgLitArg (MachStr str)) =
  case decodeModifiedUTF8 str of
    Just t -> return (mempty, [toJExpr $ T.unpack t])
    _      -> error "genFFIArg: cannot encode FFI string literal"
genFFIArg (StgLitArg l) = (mempty,) <$> genLit l
genFFIArg a@(StgVarArg i)
    | isVoid r                  = return (mempty, [])
--    | Just x <- marshalFFIArg a = x
    | isMultiVar r              = (mempty,) <$> mapM (jsIdN i) [1..varSize r]
    | otherwise                 = (\x -> (mempty,[x])) <$> jsId i
   where
     r = uTypeVt . stgArgType $ a

genIdArg :: Id -> G [JExpr]
genIdArg i = genArg (StgVarArg i)

genIdArgI :: Id -> G [Ident]
genIdArgI i
    | isVoid r     = return []
    | isMultiVar r = mapM (jsIdIN i) [1..varSize r]
    | otherwise    = (:[]) <$> jsIdI i
    where
      r = uTypeVt . idType $ i


genIdStackArgI :: Id -> G [(Ident,StackSlot)]
genIdStackArgI i = zipWith f [1..] <$> genIdArgI i
  where
    f :: Int -> Ident -> (Ident,StackSlot)
    f n ident = (ident, SlotId i n)

r2d :: Rational -> Double
r2d = realToFrac

genStrThunk :: Id -> Bool -> B.ByteString -> CostCentreStack -> C
genStrThunk i nonAscii str cc = do
  ii@(TxtI iit) <- jsIdI i
  let d = decl ii
  ccs <- costCentreStackLbl cc
  let ccsArg = map toJExpr $ maybeToList ccs
  emitStatic iit (StaticThunk Nothing) Nothing
  return $ case decodeModifiedUTF8 str of
             Just t -> d <> if nonAscii
                              then [j| `ii` =  `ApplExpr (jvar "h$strt") $
                                                  [toJExpr $ T.unpack t] ++ ccsArg`; |]
                              else [j| `ii` =  `ApplExpr (jvar "h$strta") $
                                                  [toJExpr $ T.unpack t] ++ ccsArg`; |]
             Nothing -> d <> if nonAscii
                               then [j| `ii` = `ApplExpr (jvar "h$strtb") $
                                                  [toJExpr $ map toInteger (B.unpack str)] ++ ccsArg`; |]
                               else [j| `ii` = `ApplExpr (jvar "h$strta") $
                                                  [toJExpr $ map (chr.fromIntegral) (B.unpack str)] ++ ccsArg`; |]

genLit :: Literal -> G [JExpr]
genLit (MachChar c)      = return [ [je| `ord c` |] ]
genLit (MachStr  str)    =
  case decodeModifiedUTF8 str of
    Just t -> withNewIdent $ \ident -> do
      emitToplevel [j| `decl ident`;
                       `ident` = h$str(`T.unpack t`);
                     |]
      return [ [je| `ident`() |], [je| 0 |] ]
    Nothing -> withNewIdent $ \ident -> do
      emitToplevel [j| `decl ident`;
                       `ident` = h$rstr(`map toInteger (B.unpack str)`);
                     |]
      return [ [je| `ident`() |], [je| 0 |] ]
genLit MachNullAddr      = return [ [je| null |], [je| 0 |] ]
genLit (MachInt i)       = return [ [je| `intLit i` |] ]
genLit (MachInt64 i)     = return [ [je| `intLit (shiftR i 32)` |] , [je| `toSigned i` |] ]
genLit (MachWord w)      = return [ [je| `toSigned w` |] ]
genLit (MachWord64 w)    = return [ [je| `toSigned (shiftR w 32)` |] , [je| `toSigned w` |] ]
genLit (MachFloat r)     = return [ [je| `r2d r` |] ]
genLit (MachDouble r)    = return [ [je| `r2d r` |] ]
genLit (MachLabel name _size fod)
  | fod == IsFunction = return [ [je| h$mkFunctionPtr(`TxtI . T.pack $ "h$" ++ unpackFS name`) |], [je| 0 |] ]
  | otherwise         = return [ iex (TxtI . T.pack $ "h$" ++ unpackFS name), [je| 0 |] ]
genLit (LitInteger _i _id) = error ("genLit: LitInteger") -- removed by CorePrep

-- | generate a literal for the static init tables
genStaticLit :: Literal -> G [StaticLit]
genStaticLit (MachChar c)         = return [ IntLit (fromIntegral $ ord c) ]
genStaticLit (MachStr  str)       =
  case T.decodeUtf8' str of
                         Right t -> return [ StringLit t, IntLit 0 ]
                         Left _  -> return [ BinLit str, IntLit 0]
genStaticLit MachNullAddr         = return [ NullLit, IntLit 0 ]
genStaticLit (MachInt i)          = return [ IntLit (fromIntegral i) ]
genStaticLit (MachInt64 i)        = return [ IntLit (i `shiftR` 32), IntLit (toSigned i) ]
genStaticLit (MachWord w)         = return [ IntLit (toSigned w) ]
genStaticLit (MachWord64 w)       = return [ IntLit (toSigned (w `shiftR` 32)), IntLit (toSigned w) ]
genStaticLit (MachFloat r)        = return [ DoubleLit . SaneDouble . r2d $ r ]
genStaticLit (MachDouble r)       = return [ DoubleLit . SaneDouble . r2d $ r ]
genStaticLit (MachLabel name _size fod) =
  return [ LabelLit (fod == IsFunction) (T.pack $ "h$" ++ unpackFS name) , IntLit 0 ]
genStaticLit l = error ("genStaticLit: " ++ show l)

-- make a signed 32 bit int from this unsigned one, lower 32 bits
toSigned :: Integer -> Integer
toSigned i | testBit i 31 = complement (0x7FFFFFFF `xor` (i.&.0x7FFFFFFF))
           | otherwise    = i.&.0xFFFFFFFF

-- truncate literal to fit in 32 bit int
intLit :: Integer -> Integer
intLit i = fromIntegral (fromIntegral i :: Int32)

genSingleLit :: Literal -> G JExpr
genSingleLit l = do
  es <- genLit l
  case es of
    [e] -> return e
    _   -> error "genSingleLit: expected single-variable literal"

genCon :: ExprCtx -> DataCon -> [JExpr] -> C
genCon tgt con args
  | isUnboxedTupleCon con && length (tgt^.ctxTarget) == length args =
      return $ assignAll (tgt ^. ctxTarget) args
genCon tgt con args | isUnboxedTupleCon con =
  error ("genCon: unhandled DataCon: " ++ show con ++ " " ++ show (tgt ^. ctxTop, length args))
genCon tgt con args | [ValExpr (JVar tgti)] <- tgt ^. ctxTarget =
  allocCon tgti con currentCCS args
genCon tgt con args =
  error ("genCon: unhandled DataCon: " ++ show con ++ " " ++ show (tgt ^. ctxTop, length args))

allocCon :: Ident -> DataCon -> CostCentreStack -> [JExpr] -> C
allocCon to con cc xs
  | isBoolTy (dataConType con) || isUnboxableCon con = do
      return [j| `to` = `allocUnboxedCon con xs`; |]
  | null xs = do
      i <- jsId (dataConWorkId con)
      return (assignj to i)
  | otherwise = do
      e <- enterDataCon con
      cs <- use gsSettings
      prof <- profiling
      ccsJ <- if prof then ccsVarJ cc else return Nothing
      return $ allocDynamic cs False to e xs ccsJ

allocUnboxedCon :: DataCon -> [JExpr] -> JExpr
allocUnboxedCon con []
  | isBoolTy (dataConType con) && dataConTag con == 1 = [je| false |]
  | isBoolTy (dataConType con) && dataConTag con == 2 = [je| true  |]
allocUnboxedCon con [x]
  | isUnboxableCon con = x
allocUnboxedCon con xs = panic ("allocUnboxedCon: not an unboxed constructor: " ++ show con ++ " " ++ show xs)

allocUnboxedConStatic :: DataCon -> [StaticArg] -> StaticArg
allocUnboxedConStatic con []
  | isBoolTy (dataConType con) && dataConTag con == 1      = StaticLitArg (BoolLit False)
  | isBoolTy (dataConType con) && dataConTag con == 2      = StaticLitArg (BoolLit True)
allocUnboxedConStatic _   [a@(StaticLitArg (IntLit _i))]    = a
allocUnboxedConStatic _   [a@(StaticLitArg (DoubleLit _d))] = a
allocUnboxedConStatic con _                                =
  error ("allocUnboxedConStatic: not an unboxed constructor: " ++ show con)

allocConStatic :: Ident -> CostCentreStack -> DataCon -> [GenStgArg Id] {- -> Bool -} -> G ()
allocConStatic (TxtI to) cc con args -- isRecursive
{-  | Debug.Trace.trace ("allocConStatic: " ++ show to ++ " " ++ show con ++ " " ++ show args) True -} = do
  as <- mapM genStaticArg args
  cc' <- costCentreStackLbl cc
  allocConStatic' cc' (concat as)
  where
    allocConStatic' :: Maybe Ident -> [StaticArg] -> G ()
    allocConStatic' cc' []
      | isBoolTy (dataConType con) && dataConTag con == 1 =
           emitStatic to (StaticUnboxed $ StaticUnboxedBool False) cc'
      | isBoolTy (dataConType con) && dataConTag con == 2 =
           emitStatic to (StaticUnboxed $ StaticUnboxedBool True) cc'
      | otherwise = do
           (TxtI e) <- enterDataConI con
           emitStatic to (StaticData e []) cc'
    allocConStatic' cc' [x]
      | isUnboxableCon con =
        case x of
          StaticLitArg (IntLit i)    -> emitStatic to (StaticUnboxed $ StaticUnboxedInt i) cc'
          StaticLitArg (BoolLit b)   -> emitStatic to (StaticUnboxed $ StaticUnboxedBool b) cc'
          StaticLitArg (DoubleLit d) -> emitStatic to (StaticUnboxed $ StaticUnboxedDouble d) cc'
          _                          -> error $ "allocConStatic: invalid unboxed literal: " ++ show x
    allocConStatic' cc' xs =
           if con == consDataCon
              then flip (emitStatic to) cc' =<< allocateStaticList [args !! 0] (args !! 1)
              else do
                (TxtI e) <- enterDataConI con
                emitStatic to (StaticData e xs) cc'

-- avoid one indirection for global ids
-- fixme in many cases we can also jump directly to the entry for local?
jumpToII :: Id -> [JExpr] -> JStat -> C
jumpToII i args afterLoad
  | isLocalId i = do
     ii <- jsId i
     return (ra <> afterLoad <> [j| return `ii`.f; |])
  | otherwise   = do
     ei <- jsEntryId i
     return (ra <> afterLoad <> [j| return `ei`; |])
  where
    ra = mconcat . reverse $ zipWith (\r a -> [j| `r` = `a`; |]) (enumFrom R2) args

-- load arguments and jump to fun directly (not going through trampoline)
{-
jumpTo' :: JExpr -> [JExpr] -> JStat
jumpTo' fun args = ra <> [j| return `fun`(); |]
  where
      ra = assignAll (enumFrom R2) args
-}

jumpToFast :: [StgArg] -> JStat -> C
jumpToFast as afterLoad = do
  regs <- concatMapM genArg as
  (fun, spec) <- selectApply True (as,regs)
  if spec
    then return $ mconcat (ra regs) <> afterLoad <> [j| return `fun`(); |]
    else return $ mconcat (ra regs) <> afterLoad <> [j| return `fun`(`mkTag regs as`); |]
    where
      ra regs   = reverse $ zipWith (\r e -> [j| `r` = `e` |]) (enumFrom R2) regs
      mkTag rs as = (length rs `shiftL` 8) .|. length as


-- find a specialized application path if there is one
selectApply :: Bool           -- ^ true for fast apply, false for stack apply
            -> ([StgArg], [JExpr])       -- ^ arguments
            -> G (JExpr,Bool) -- ^ the function to call, true if specialized path
selectApply fast (args, as) = do
  case specApply fast (length args) (length as) of
    Just e  -> return (e, True)
    Nothing -> return (jsv $ "h$ap_gen" <> fastSuff, False)
  where
    fastSuff | fast      = "_fast"
             | otherwise = ""

-- fixme: what if the call returns a thunk?
genPrimCall :: ExprCtx -> PrimCall -> [StgArg] -> Type -> G (JStat, ExprResult)
genPrimCall top (PrimCall lbl _) args t = do
  j <- parseFFIPattern False False False ("h$" ++ unpackFS lbl) t (map toJExpr $ top ^. ctxTarget) args
  return (j, ExprInline Nothing)

getObjectKeyValuePairs :: [StgArg] -> Maybe [(Text, StgArg)]
getObjectKeyValuePairs [] = Just []
getObjectKeyValuePairs (k:v:xs)
  | Just t <- argJSStringLitUnfolding k =
      fmap ((t,v):) (getObjectKeyValuePairs xs)
getObjectKeyValuePairs _ = Nothing                                     

argJSStringLitUnfolding :: StgArg -> Maybe Text
argJSStringLitUnfolding (StgVarArg v)
  | False = Just "abc" -- fixme
argJSStringLitUnfolding _ = Nothing

genForeignCall :: ForeignCall -> Type -> [JExpr] -> [StgArg] -> G (JStat, ExprResult)
genForeignCall (CCall (CCallSpec (StaticTarget ccLbl Nothing True) PrimCallConv PlayRisky)) _ _ [StgVarArg i1, StgVarArg i2]
  | ccLbl == fsLit "__ghcjsi_capture" = do
      ii1 <- jsIdI i1
      ii2 <- jsIdI i2
      return ([j| `ii2` = `ii1`; |], ExprInline Nothing)
genForeignCall (CCall (CCallSpec (StaticTarget tgt Nothing True) JavaScriptCallConv PlayRisky)) t [obj] args
  | tgt == fsLit "h$buildObject", Just pairs <- getObjectKeyValuePairs args = do
      pairs' <- mapM (\(k,v) -> genArg v >>= \([v']) -> return (k,v')) pairs
      return (assignj obj (ValExpr (JHash $ M.fromList pairs')), ExprInline Nothing)
genForeignCall (CCall (CCallSpec ccTarget cconv safety)) t tgt args =
  (,exprResult) <$> parseFFIPattern catchExcep async isJsCc lbl t tgt' args
  where
    isJsCc = cconv == JavaScriptCallConv

    lbl | (StaticTarget clbl _mpkg _isFunPtr) <- ccTarget
            = let clbl' = unpackFS clbl
              in  if | isJsCc -> clbl'
                     | wrapperPrefix `L.isPrefixOf` clbl' ->
                         ("h$" ++ (drop 2 $ dropWhile isDigit $ drop (length wrapperPrefix) clbl'))
                     | otherwise -> "h$" ++ clbl'
        | otherwise = "h$callDynamic"

    exprResult | async     = ExprCont
               | otherwise = ExprInline Nothing

    catchExcep = (cconv == JavaScriptCallConv) &&
                 playSafe safety || playInterruptible safety

    async | isJsCc    = playInterruptible safety
          | otherwise = playInterruptible safety || playSafe safety

    tgt'  | async     = take (length tgt) (map toJExpr $ enumFrom R1)
          | otherwise = tgt

    wrapperPrefix = "ghczuwrapperZC"

-- | generate the actual call
{-
  parse FFI patterns:
   "&value         -> value
  1. "function"      -> ret = function(...)
  2. "$r = $1.f($2)  -> r1 = a1.f(a2)

  arguments, $1, $2, $3 unary arguments
     $1_1, $1_2, for a binary argument

  return type examples
  1. $r                      unary return
  2. $r1, $r2                binary return
  3. $r1, $r2, $r3_1, $r3_2  unboxed tuple return
 -}
parseFFIPattern :: Bool  -- ^ catch exception and convert them to haskell exceptions
                -> Bool  -- ^ async (only valid with javascript calling conv)
                -> Bool  -- ^ using javascript calling convention
                -> String
                -> Type
                -> [JExpr]
                -> [StgArg]
                -> C
parseFFIPattern catchExcep async jscc pat t es as
  | catchExcep = do
      c <- parseFFIPatternA async jscc pat t es as
      return [j| try {
                   `c`;
                 } catch(e) {
                   return h$throwJSException(e);
                 }
               |]
  | otherwise  = parseFFIPatternA async jscc pat t es as

parseFFIPatternA :: Bool  -- ^ async
                 -> Bool  -- ^ using JavaScript calling conv
                 -> String
                 -> Type
                 -> [JExpr]
                 -> [StgArg]
                 -> C
-- async calls get an extra callback argument
-- call it with the result
parseFFIPatternA True True pat t es as  = do
  cb <- makeIdent
  stat <- parseFFIPattern' (Just (toJExpr cb)) True pat t es as
  return [j| `decl cb`;
             var x = { mv: null };
             `cb` = h$mkForeignCallback(x);
             `stat`;
             if(x.mv === null) {
               x.mv = new h$MVar();
               `Sp` = `Sp` + 1;
               `Stack`[`Sp`] = h$unboxFFIResult;
               return h$takeMVar(x.mv);
             } else {
               var d = x.mv;
               `copyResult d`;
             }
           |]
     where nrst = typeSize t
           copyResult d = assignAll es (map (\i -> [je| `d`[`i`] |]) [0..nrst-1])
parseFFIPatternA _async javascriptCc pat t es as =
  parseFFIPattern' Nothing javascriptCc pat t es as

-- parseFFIPatternA _ _ _ _ _ _ = error "parseFFIPattern: non-JavaScript pattern must be synchronous"

parseFFIPattern' :: Maybe JExpr -- ^ Nothing for sync, Just callback for async
                 -> Bool        -- ^ javascript calling convention used
                 -> String      -- ^ pattern called
                 -> Type        -- ^ return type
                 -> [JExpr]     -- ^ expressions to return in (may be more than necessary)
                 -> [StgArg]    -- ^ arguments
                 -> C
parseFFIPattern' callback javascriptCc pat t ret args
  | not javascriptCc = mkApply pat
  | otherwise = do
      u <- freshUnique
      case parseFfiJME pat u of
        Right (ValExpr (JVar (TxtI _ident))) -> mkApply pat
        Right expr | not async && length tgt < 2 -> do
          (statPre, ap) <- argPlaceholders args
          let rp  = resultPlaceholders async t ret
              env = M.fromList (rp ++ ap)
          if length tgt == 1
            then return $ statPre <> (everywhere (mkT $ replaceIdent env) [j| $r = `expr`; |])
            else return $ statPre <> (everywhere (mkT $ replaceIdent env) (toStat expr))
        Right _ -> p $ "invalid expression FFI pattern. Expression FFI patterns can only be used for synchronous FFI " ++
                       " imports with result size 0 or 1.\n" ++ pat
        Left _ -> case parseFfiJM pat u of
          Left err -> p (show err)
          Right stat -> do
            let rp = resultPlaceholders async t ret
            let cp = callbackPlaceholders callback
            (statPre, ap) <- argPlaceholders args
            let env = M.fromList (rp ++ ap ++ cp)
            return $ statPre <> (everywhere (mkT $ replaceIdent env) stat) -- fixme trace?
  where
    async = isJust callback
    tgt = take (typeSize t) ret
    -- automatic apply, build call and result copy
    mkApply f
      | Just cb <- callback = do
         (stats, as) <- unzip <$> mapM genFFIArg args
         cs <- use gsSettings
         return $ traceCall cs as <> mconcat stats <> ApplStat f' (concat as++[cb])
      | (ts@(_:_)) <- tgt = do
         (stats, as) <- unzip <$> mapM genFFIArg args
         (statR, (t:ts')) <- return (mempty, ts)
         cs <- use gsSettings
         return $ traceCall cs as
                <> mconcat stats
                <> [j| `t` = `ApplExpr f' (concat as)`; |]
                <> copyResult ts'
                <> statR
      | otherwise = do
         (stats, as) <- unzip <$> mapM genFFIArg args
         cs <- use gsSettings
         return $ traceCall cs as <> mconcat stats <> ApplStat f' (concat as)
        where f' = toJExpr (TxtI $ T.pack f)
    copyResult rs = mconcat $ zipWith (\t r -> [j| `r`=`t`;|]) (enumFrom Ret1) rs
    p e = error ("Parse error in FFI pattern: " ++ pat ++ "\n" ++ e)
    replaceIdent :: Map Ident JExpr -> JExpr -> JExpr
    replaceIdent env e@(ValExpr (JVar i))
      | isFFIPlaceholder i = fromMaybe err (M.lookup i env)
      | otherwise = e
        where
          (TxtI i') = i
          err = error (pat ++ ": invalid placeholder, check function type: " ++ show i')
    replaceIdent _ e = e
    traceCall cs as
        | csTraceForeign cs = [j| h$traceForeign(`pat`, `as`); |]
        | otherwise         = mempty

-- parse and saturate ffi splice
parseFfiJME :: String -> Int -> Either P.ParseError JExpr
parseFfiJME xs u = fmap (saturateFFI u) . parseJME $ xs

-- parse and saturate ffi splice, check for unhygienic declarations
parseFfiJM :: String -> Int -> Either P.ParseError JStat
parseFfiJM xs u = fmap (makeHygienic . saturateFFI u) . parseJM $ xs
  where
    makeHygienic :: JStat -> JStat
    makeHygienic s = snd $ O.renameLocalsFun (map addFFIToken newLocals) ([], s)

--    addFFIToken (StrI xs) = TxtI (T.pack $ "ghcjs_ffi_" ++ show u ++ "_" ++ xs)
    addFFIToken (TxtI xs) = TxtI (T.pack ("ghcjs_ffi_" ++ show u ++ "_") <> xs)

saturateFFI :: JMacro a => Int -> a -> a
saturateFFI u = jsSaturate (Just . T.pack $ "ghcjs_ffi_sat_" ++ show u)

-- $r for single, $r1,$r2 for dual
-- $r1, $r2, etc for ubx tup, void args not counted
resultPlaceholders :: Bool -> Type -> [JExpr] -> [(Ident,JExpr)] -- ident, replacement
resultPlaceholders True _ _ = [] -- async has no direct resuls, use callback
resultPlaceholders False t rs =
  case repType t of
    UbxTupleRep uts ->
      let sizes = filter (>0) (map typeSize uts)
          f _ 0 = []
          f n 1 = [["$r" ++ show n]]
          f n k = ["$r" ++ sn, "$r" ++ sn ++ "_1"] : map (\x -> ["$r" ++ sn ++ "_" ++ show x]) [2..k]
            where sn = show n
          phs   = zipWith (\size n -> f n size) sizes [(1::Int)..]
      in case sizes of
           [n] -> mkUnary n
           _   -> concat $ zipWith (\phs' r -> map (\i -> (TxtI (T.pack i), r)) phs') (concat phs) rs
    UnaryRep t' -> mkUnary (typeSize t')
  where
    mkUnary 0 = []
    mkUnary 1 = [(TxtI "$r",head rs)] -- single
    mkUnary n = [(TxtI "$r",head rs),(TxtI "$r1", head rs)] ++
       zipWith (\n r -> (TxtI . T.pack $ "$r" ++ show n, toJExpr r)) [2..n] (tail rs)

-- $1, $2, $3 for single, $1_1, $1_2 etc for dual
-- void args not counted
argPlaceholders :: [StgArg] -> G (JStat, [(Ident,JExpr)])
argPlaceholders args = do
  (stats, idents0) <- unzip <$> mapM genFFIArg args
  let idents = filter (not . null) idents0
  return $ (mconcat stats, concat
    (zipWith (\is n -> mkPlaceholder True ("$"++show n) is) idents [(1::Int)..]))

callbackPlaceholders :: Maybe JExpr -> [(Ident,JExpr)]
callbackPlaceholders Nothing  = []
callbackPlaceholders (Just e) = [((TxtI "$c"), e)]

mkPlaceholder :: Bool -> String -> [JExpr] -> [(Ident, JExpr)]
mkPlaceholder undersc prefix aids =
      case aids of
             []       -> []
             [x]      -> [(TxtI . T.pack $ prefix, x)]
             xs@(x:_) -> (TxtI . T.pack $ prefix, x) :
                zipWith (\x m -> (TxtI . T.pack $ prefix ++ u ++ show m,x)) xs [(1::Int)..]
   where u = if undersc then "_" else ""

-- ident is $N, $N_R, $rN, $rN_R or $r or $c
isFFIPlaceholder :: Ident -> Bool
isFFIPlaceholder (TxtI x) =
  either (const False) (const True) (P.parse parser "" x)
    where
      parser = void (P.try $ P.string "$r") <|>
               void (P.try $ P.string "$c") <|> do
        P.char '$'
        P.optional (P.char 'r')
        P.many1 P.digit
        P.optional (P.char '_' >> P.many1 P.digit)

withNewIdent :: (Ident -> G a) -> G a
withNewIdent m = makeIdent >>= m

makeIdent :: G Ident
makeIdent = do
  gsId += 1
  i <- use gsId
  mod <- use gsModule
  return (TxtI . T.pack $ "h$$" ++ zEncodeString (show mod) ++ "_" ++ encodeUnique i)

freshUnique :: G Int
freshUnique = gsId += 1 >> use gsId

-- returns True if the expression is definitely inline
isInlineExpr :: UniqSet Id -> StgExpr -> (UniqSet Id, Bool)
isInlineExpr v (StgApp i args)                    = (emptyUniqSet, isInlineApp v i args)
isInlineExpr _ (StgLit{})                         = (emptyUniqSet, True)
isInlineExpr _ (StgConApp{})                      = (emptyUniqSet, True)
isInlineExpr _ (StgOpApp (StgFCallOp f _) _ _)    = (emptyUniqSet, isInlineForeignCall f)
isInlineExpr v (StgOpApp (StgPrimOp SeqOp) [StgVarArg e] t) = (emptyUniqSet, e `elementOfUniqSet` v || isStrictType t)
isInlineExpr _ (StgOpApp (StgPrimOp op) _ _)      = (emptyUniqSet, isInlinePrimOp op)
isInlineExpr _ (StgOpApp (StgPrimCallOp _c) _ _)  = (emptyUniqSet, True)
isInlineExpr _ (StgLam{})                         = (emptyUniqSet, True)
isInlineExpr v (StgCase e _ _ b _ _ alts)         = let (_ve, ie)   = isInlineExpr v e
                                                        v'          = addOneToUniqSet v b
                                                        (vas, ias)  = unzip $ map (isInlineExpr v') (alts ^.. traverse . _4)
                                                        vr         = foldl1' intersectUniqSets vas
                                                    in (vr, (ie || b `elementOfUniqSet` v) && and ias)
isInlineExpr v (StgLet b e)                       = isInlineExpr (inspectInlineBinding v b) e
isInlineExpr v (StgLetNoEscape _ _ b e)           = isInlineExpr (inspectInlineBinding v b) e
#if __GLASGOW_HASKELL__ < 709
isInlineExpr v (StgSCC _ _ _ e)                   = isInlineExpr v e
isInlineExpr v (StgTick _ _ e)                    = isInlineExpr v e
#else
isInlineExpr v (StgTick  _ e)                     = isInlineExpr v e
#endif

inspectInlineBinding :: UniqSet Id -> StgBinding -> UniqSet Id
inspectInlineBinding v (StgNonRec i r) = inspectInlineRhs v i r
inspectInlineBinding v (StgRec bs)       =
  foldl' (\v' (i,r) -> inspectInlineRhs v' i r) v bs

inspectInlineRhs :: UniqSet Id -> Id -> StgRhs -> UniqSet Id
inspectInlineRhs v i (StgRhsCon{})                         = addOneToUniqSet v i
inspectInlineRhs v i (StgRhsClosure _ _ _ ReEntrant _ _ _) = addOneToUniqSet v i
inspectInlineRhs v _ _                                     = v

isInlineForeignCall :: ForeignCall -> Bool
isInlineForeignCall (CCall (CCallSpec _ cconv safety)) =
  not (playInterruptible safety) &&
  not (cconv /= JavaScriptCallConv && playSafe safety)

isInlineApp :: UniqSet Id -> Id -> [StgArg] -> Bool
isInlineApp v i [] = isUnboxedTupleType (idType i) || isStrictType (idType i) || i `elementOfUniqSet` v || isStrictId i
isInlineApp _ i [StgLitArg (MachStr _)]
  | getUnique i `elem` [unpackCStringIdKey, unpackCStringUtf8IdKey, unpackCStringAppendIdKey] = True
isInlineApp v i [StgVarArg a]
  | DataConWrapId dc <- idDetails i, isNewTyCon (dataConTyCon dc), isStrictType (idType a) || a `elementOfUniqSet` v || isStrictId a = True
isInlineApp _ _ _ = False
