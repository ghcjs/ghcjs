{-# LANGUAGE QuasiQuotes, TupleSections, CPP, OverloadedStrings, LambdaCase, MultiWayIf #-}

{-
  Main generator module
-}

module Gen2.Generator (generate) where

import           ForeignCall
import           CostCentre
import           FastString
import           TysWiredIn
import           BasicTypes
import           PrelNames
import           DynFlags
import           Encoding
import           HscTypes
import           TysPrim
import           UniqSet
import           NameSet
import           Literal
import           DataCon
import           CoreSyn
import           TcType
import           UniqFM
import           Unique
import           StgSyn
import           PrimOp
import           Module
import           VarSet
import           TyCon
import           Util
import           Type hiding (typeSize)
import           Name
import           GHC
import           Id

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens
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
import qualified Data.IntMap.Strict as IM
import           Data.Monoid
import           Data.Maybe (isJust, fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List (partition, intercalate, sort, sortBy, foldl')
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)

import           Compiler.JMacro
import qualified Text.Parsec as P

import           Compiler.Settings

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

import qualified Debug.Trace

type StgPgm     = [StgBinding]
type StaticRefs = [Id]

type ExprCtx = (Id, [JExpr],UniqSet Id)
ctxTop :: ExprCtx -> Id
ctxTop c = c ^. _1
ctxTarget :: ExprCtx -> [JExpr]
ctxTarget c = c ^. _2
ctxEval :: ExprCtx -> UniqSet Id
ctxEval c = c ^. _3
addEval :: Id -> ExprCtx -> ExprCtx
addEval i = over _3 (flip addOneToUniqSet i)

generate :: GhcjsSettings
         -> DynFlags
         -> CgGuts
         -> StgPgm
         -> CollectedCCs
         -> ByteString -- ^ binary data for the .js_o object file
generate settings df guts s cccs =
  let (uf, s') = sinkPgm m s
      m        = cg_module guts
  in  flip evalState (initState df m uf) $ do
        initCostCentres cccs
        (st, g) <- genUnits df m s'
        let p = map (\lu -> (luSymbols lu, luStat lu)) g
            d = map (\lu -> (luTopDeps lu, luAllDeps lu)) g
            (st', dbg) = dumpAst st settings df s'
        deps <- genMetaData d
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
dumpAst st settings dflags s
  | buildingDebug dflags = (st', [(["h$debug", "h$dumpAst"], bs)])
  | otherwise        = (st, [])
      where
        (st', bs) = Object.serializeStat st [] [] [j| h$dumpAst = `x` |]
        x = (intercalate "\n\n" (map showIndent s))

-- | variable prefix for the nth block in module
modulePrefix :: Module -> Int -> Text
modulePrefix m n =
  let encMod = zEncodeString . moduleNameString . moduleName $ m
  in  T.pack $ "h$" ++ encMod ++ "_id_" ++ show n


-- | data used to generate one ObjUnit in our object file
data LinkableUnit = LinkableUnit
                    { luSymbols  :: [Text]        -- ^ exported symbols as Text
                    , luStat     :: BL.ByteString -- ^ serialized JS AST
                    , luTopDeps  :: [Id]
                    , luAllDeps  :: [Id]          -- ^ identifiers this unit depends on
                    } deriving (Eq, Ord, Show)

-- | Generate the ingredients for the linkable units for this module
genUnits :: DynFlags
         -> Module
         -> StgPgm
         -> G (Object.SymbolTable, [LinkableUnit]) -- ^ the final symbol table and the linkable units
genUnits df m ss = generateGlobalBlock =<< go 2 Object.emptySymbolTable ss
    where
      go :: Int                 -- ^ the block we're generating (block 1 is the global block for the module)
         -> Object.SymbolTable  -- ^ the shared symbol table
         -> StgPgm
         -> G (Object.SymbolTable, [LinkableUnit])
      go n st (x:xs) = do
        (st', lu) <- generateBlock st x n
        (st'', lus)  <- go (n+1) st' xs
        return (st'', lu:lus)
      go _ st []     = return (st, [])


      generateGlobalBlock :: (Object.SymbolTable, [LinkableUnit])
                          -> G (Object.SymbolTable, [LinkableUnit])
      generateGlobalBlock (st, lus) = do
        glbl <- use gsGlobal
        (st', ss, bs) <- objectEntry m st [] [] []
                         . O.optimize
                         . jsSaturate (Just $ modulePrefix m 1)
                         $ mconcat (reverse glbl)
        return (st', LinkableUnit ss bs [] [] : lus)

      -- | Generate the linkable unit for one binding or group of
      --   mutually recursive bindings
      generateBlock :: Object.SymbolTable
                    -> StgBinding
                    -> Int
                    -> G (Object.SymbolTable, LinkableUnit)
      generateBlock st decl n = do
        tl      <- genToplevel decl
        extraTl <- use (gsGroup . ggsToplevelStats)
        ci      <- use (gsGroup . ggsClosureInfo)
        si      <- use (gsGroup . ggsStatic)
        unf     <- use gsUnfloated
        resetGroup
        let allDeps = collectIds unf decl
            topDeps = collectTopIds decl
        (st', ss, bs) <- objectEntry m st topDeps ci si
                           . O.optimize
                           . jsSaturate (Just $ modulePrefix m n)
                           $ mconcat (reverse extraTl) <> tl
        return $! seqList topDeps `seq` seqList allDeps `seq` st' `seq`
          (st', LinkableUnit ss bs topDeps allDeps)

objectEntry :: Module
            -> Object.SymbolTable
            -> [Id]
            -> [ClosureInfo]
            -> [StaticInfo]
            -> JStat
            -> G (Object.SymbolTable, [Text], BL.ByteString)
objectEntry m st i ci si stat = do
  i' <- mapM idStr i
  let (st', o) = Object.serializeStat st ci si stat
  rnf i' `seq` rnf o `seq` return (st', i', o)
    where
      idStr i = itxt <$> jsIdI i

collectTopIds :: StgBinding -> [Id]
collectTopIds (StgNonRec b _) = [b]
collectTopIds (StgRec bs) = map fst bs

collectIds :: UniqFM StgExpr -> StgBinding -> [Id]
collectIds unfloated b = filter acceptId $ S.toList (bindingRefs unfloated b)
  where
    acceptId i = all ($ i) [not . isForbidden] -- fixme test this: [isExported[isGlobalId, not.isForbidden]
    -- the GHC.Prim module has no js source file
    isForbidden i
      | Just m <- nameModule_maybe (getName i) =
                    moduleNameText m    == T.pack "GHC.Prim" &&
                    Object.packageName (modulePackageText m) == T.pack "ghc-prim"
      | otherwise = False

data MetadataCache = MDC
         { mdcPackage    :: (IM.IntMap Object.Package)  -- Unique PackageId -> Object.Package
         , mdcId         :: (IM.IntMap Object.Fun)      -- Unique Id -> Object.Fun
         }

genMetaData :: [([Id], [Id])] -> G Object.Deps
genMetaData p1 = do
  m <- use gsModule
  (ds, (MDC pkgs funs)) <- runStateT (sequence (zipWith oneDep p1 [0..])) (MDC IM.empty IM.empty)
  let sp = S.fromList (IM.elems pkgs)
      sf = S.fromList (IM.elems funs)
      (dl, blocks) = unzip ds
      ba = listArray (0, length blocks - 1) blocks
      dm = M.fromList (concat dl)
      es = M.empty -- fixme exports
  return $ Object.Deps (modulePackageText m) (moduleNameText m) es ba dm
   where
    oneDep (symbs, deps) n = do
      ds <- S.fromList <$> mapM idFun deps
      ss <- mapM idFun symbs
      return (map (,n) ss, ds)
    idFun i = do
      let k = getKey . getUnique $ i
      (MDC ps is) <- get
      case IM.lookup k is of
        Just x  -> return x
        Nothing -> do
          m <- lift (use gsModule)
          let mod = fromMaybe m $ nameModule_maybe (getName i)
              mk = getKey . getUnique $ mod
          (TxtI idTxt) <- lift (jsIdI i)
          case IM.lookup mk ps of
            Just p -> do
              let f = Object.Fun p (moduleNameText mod) idTxt
              put (MDC ps $ IM.insert k f is)
              return f
            Nothing -> do
              let p = modulePackageText mod
                  f = Object.Fun p (moduleNameText mod) idTxt
              put (MDC (IM.insert mk p ps) (IM.insert k f is))
              return f

moduleNameText :: Module -> Text
moduleNameText m
  | xs == ":Main" = T.pack "Main"
  | otherwise     = T.pack xs
    where xs      = moduleNameString . moduleName $ m

modulePackageText :: Module -> Object.Package
modulePackageText m 
  | isWiredInPackage pkgStr = Object.Package n ""
  | otherwise               = Object.Package n v
  where
    pkgStr = packageIdString (modulePackageId m)
    (n, v) = Linker.splitVersion . T.pack . packageIdString . modulePackageId $ m

genToplevel :: StgBinding -> C
genToplevel (StgNonRec bndr rhs) = genToplevelDecl bndr rhs
genToplevel (StgRec bs)          =
  mconcat $ map (\(bndr, rhs) -> genToplevelDecl bndr rhs) bs

lookupStaticRefs :: Id -> [(Id, [Id])] -> StaticRefs
lookupStaticRefs i xs = fromMaybe [] (lookup i xs)

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
genToplevelConEntry i (StgRhsCon _cc con args)
    | i `elem` dataConImplicitIds con = genSetConInfo i con NoSRT
genToplevelConEntry i (StgRhsClosure _cc _bi [] upd_flag srt args (StgConApp dc cargs))
    | i `elem` dataConImplicitIds dc = genSetConInfo i dc srt
genToplevelConEntry _ _ = mempty

genStaticRefs :: SRT -> G CIStatic
genStaticRefs NoSRT = return noStatic
genStaticRefs (SRTEntries s) = do
  unfloated <- use gsUnfloated
  let xs = filter (\x -> not $ elemUFM x unfloated) (uniqSetToList s)
  CIStaticRefs <$> mapM getStaticRef xs
genStaticRefs (SRT n e bmp) =
  error "genStaticRefs: unexpected SRT"

getStaticRef :: Id -> G Text
getStaticRef = fmap (itxt.head) . genIdsI

genToplevelRhs :: Id
               -> StgRhs
               -> C
-- special cases
genToplevelRhs i (StgRhsClosure _cc _bi _ _ _ _ body)
  -- foreign exports
  | (StgOpApp (StgFCallOp (CCall (CCallSpec (StaticTarget t _ _) _ _)) _)
     [StgLitArg (MachInt is_js_conv), StgLitArg (MachStr js_name), StgVarArg tgt] _) <- body,
     t == fsLit "__mkExport" = return mempty -- fixme error "export not implemented"
  -- top-level strings
  | (StgApp upk [StgLitArg (MachStr bs)]) <- body, getUnique upk == unpackCStringIdKey     = genStrThunk i False bs
  | (StgApp upk [StgLitArg (MachStr bs)]) <- body, getUnique upk == unpackCStringUtf8IdKey = genStrThunk i True bs
-- general cases:
genToplevelRhs i (StgRhsCon _cc con args) = do
  ii <- jsIdI i
  allocConStatic ii con args
  return mempty
genToplevelRhs i (StgRhsClosure _cc _bi [] upd_flag srt args body) = do
  eid@(TxtI eidt) <- jsEnIdI i
  id@(TxtI idt)   <- jsIdI i
  body <- genBody emptyUniqSet i args body upd_flag
  sr <- genStaticRefs srt
  cs <- use gsSettings
  et <- genEntryType args
  let (static, regs, upd) =
        if et == CIThunk
          then (StaticThunk (Just eidt), CIRegs 0 [PtrV],                updateThunk cs)
          else (StaticFun eidt,          CIRegs 1 (concatMap idVt args), mempty)
  emitClosureInfo (ClosureInfo eidt regs idt (CILayoutFixed 0 []) et sr)
  emitStatic idt static
  return $ decl eid <> assignj eid (JFunc [] (upd <> body))

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

genBody :: UniqSet Id -> Id -> [Id] -> StgExpr -> UpdateFlag -> C
genBody ev i args e upd = do
  la     <- loadArgs args
  -- find the result type after applying the function to the arguments
  let resultSize xxs@(x:xs) t
        | isUnboxedTupleType (idType x) = error "genBody: unboxed tuple argument"
        | otherwise                     =
            case repType t of
              ur@(UnaryRep t') | isFunTy t' ->
                let (fa,fr) = splitFunTy t'
                    t''     = mkFunTys (flattenRepType $ repType fa) fr
                in  resultSize xs (snd . splitFunTy $ t'')
              _                             -> 1 -- possibly newtype family, must be boxed
      resultSize [] t = case repType t of
                               UnaryRep t'     -> typeSize t'
                               UbxTupleRep tys -> sum (map typeSize tys)
      ids = take (resultSize args $ idType i) (map toJExpr $ enumFrom R1)
  (e, r) <- genExpr (i, ids, ev) e
  return $ la <> e <> [j| return `Stack`[`Sp`]; |]


loadArgs :: [Id] -> C
loadArgs args = do
  args' <- concatMapM genIdArgI args
  return (mconcat $ zipWith loadArg args' (enumFrom R2))
   where
     loadArg a reg = decl a <> [j| `a` = `reg`; |]

data ExprResult = ExprCont
                | ExprInline (Maybe [JExpr])
  deriving (Eq, Ord, Show)

data ExprValData = ExprValData [JExpr]
  deriving (Eq, Ord, Show)

-- not a Monoid
branchResult :: [ExprResult] -> ExprResult
branchResult []           = error "branchResult: empty list"
branchResult [e]          = e
branchResult (ExprCont:_) = ExprCont
branchResult (_:es)
  | any (==ExprCont) es   = ExprCont
  | otherwise             = ExprInline Nothing

genExpr :: ExprCtx -> StgExpr -> G (JStat, ExprResult)
genExpr top (StgApp f args) = genApp top f args
genExpr top (StgLit l) = (,ExprInline Nothing) . assignAllCh ("genExpr StgLit " ++ show top) (ctxTarget top) <$> genLit l
genExpr top (StgConApp con args) = do
  as <- concatMapM genArg args
  c <- genCon top con as
  return (c, ExprInline (Just as))
genExpr top (StgOpApp (StgFCallOp f _) args t) =
   genForeignCall f t (ctxTarget top) args
genExpr top (StgOpApp (StgPrimOp op) args t)    = genPrimOp top op args t
genExpr top (StgOpApp (StgPrimCallOp c) args t) = genPrimCall top c args t
genExpr top (StgLam{}) = error "genExpr: StgLam"
genExpr top (StgCase e _ liveRhs b srt at alts) = genCase top b e at alts liveRhs srt
genExpr top (StgLet b e) = do
  (b',top') <- genBind top b
  (s,r)     <- genExpr top' e
  return (b' <> s, r)
genExpr top (StgLetNoEscape{}) = error "genExpr: StgLetNoEscape"
genExpr top (StgSCC cc b1 b2 e) = genExpr top e
genExpr top (StgTick m n e) = genExpr top e

getId :: JExpr -> Ident
getId (ValExpr (JVar i)) = i
getId _                  = error "getId: expression is not a variable"

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
    | [top] <- ctxTarget ctx, getUnique i == unpackCStringIdKey = return . (,ExprInline Nothing) . assignj top $
        case decodeModifiedUTF8 bs of
          Just t  -> [je| h$ustra(`t`) |]
          Nothing -> [je| h$urstra(`map (chr.fromIntegral) (B.unpack bs)`) |]
    | [top] <- ctxTarget ctx, getUnique i == unpackCStringUtf8IdKey = return . (,ExprInline Nothing) . assignj top $
        case decodeModifiedUTF8 bs of
          Just t  -> [je| h$ustr(`t`) |]
          Nothing -> [je| h$urstr(`map toInteger (B.unpack bs)`) |]
 -- we could handle unpackNBytes# here, but that's probably not common
 -- enough to warrant a special case
genApp ctx i [StgLitArg (MachStr bs), x]
    | [top] <- ctxTarget ctx, getUnique i == unpackCStringAppendIdKey, Just d <- decodeModifiedUTF8 bs = do -- fixme breaks assumption in codegen if bs doesn't decode
        a <- genArg x
        return ([j| `top` = h$appendToHsStringA(`d`, `a`); |], ExprInline Nothing)
genApp top i a
    | not (isUnboxedTupleType (idType i)) &&
      (isPrimitiveType (idType i) || isStrictType (idType i)) &&
      not (might_be_a_function (idType i))
            = do
                a <- assignAllCh "genApp" (ctxTarget top) <$> genIds i
                return (a, ExprInline Nothing)
    | n == 0 && i `elementOfUniqSet` ctxEval top = do
                a <- assignAllCh "genApp" (ctxTarget top) <$> genIds i
                return (a, ExprInline Nothing)
    | idRepArity i == 0 && n == 0 && not (might_be_a_function (idType i)) && not (isLocalId i) = do
          ii <- enterId
          cgs <- use gsSettings
          let e | csInlineEnter cgs = [j| var t = `ii`.f;
                                          var tt = t.t;
                                          `R1` = `ii`;
                                          if(tt === `Thunk`) {
                                            return t;
                                          } else if(tt === `Blackhole`) {
                                            return h$ap_0_0_fast();
                                          }
                                        |]
                | otherwise = [j| return h$e(`ii`); |]
          return (e, ExprCont)
    | idRepArity i == 0 && n == 0 && not (might_be_a_function (idType i))
          = do
             ii <- enterId
             cgs <- use gsSettings
             let e | csInlineEnter cgs = [j| var t = `ii`.f;
                                             var tt = t.t;
                                             `R1` = `ii`;
                                             if(tt === `Thunk`) {
                                               return t;
                                             } else if(tt === `Blackhole`) {
                                               return h$ap_0_0_fast();
                                             }
                                           |]
                   | otherwise = [j| return h$e(`ii`); |]
             return (e, ExprCont)
    | idRepArity i == n && not (isLocalId i) && n /= 0 = do
        as' <- concatMapM genArg a
        j <- jumpToII i as' =<< r1
        return (j, ExprCont)
    | idRepArity i < n && idRepArity i > 0 =
         let (reg,over) = splitAt (idRepArity i) a
         in  do
           reg' <- concatMapM genArg reg
           pc   <- pushCont over
           j    <- jumpToII i reg' =<< r1
           return (pc <> j, ExprCont)
    | otherwise = do
           j <- jumpToFast a =<< r1
           return (j, ExprCont)
  where
    stackTop = [je| `Stack`[`Sp`] |]
    enterId :: G JExpr
    enterId = genArg (StgVarArg i) >>=
                \case
                   [x] -> return x
                   _   -> error "genApp: unexpected multi-var argument"

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
    (StgNonRec b r) -> do
       assign b r
       j <- allocCls [(b,r)]
       return (j, addEvalRhs ctx [(b,r)])
    (StgRec bs)     -> do
       mapM_ (uncurry assign) bs
       j <- allocCls bs
       return (j, addEvalRhs ctx bs)
   where
     assign :: Id -> StgRhs -> G ()
     assign b r = genEntry ctx b r

     addEvalRhs c [] = c
     addEvalRhs c ((b,r):xs)
       | (StgRhsCon{}) <- r                         = addEvalRhs (addEval b c) xs
       | (StgRhsClosure _ _ _ ReEntrant _ _ _) <- r = addEvalRhs (addEval b c) xs
       | otherwise                                  = addEvalRhs c xs


-- generate the entry function for a local closure
genEntry :: ExprCtx -> Id -> StgRhs -> G ()
genEntry _ i (StgRhsCon _cc con args) = return () -- mempty -- error "local data entry"

genEntry ctx i cl@(StgRhsClosure _cc _bi live upd_flag srt args body) = resetSlots $ do
  ll <- loadLiveFun live
  upd <- genUpdFrame upd_flag
  body <- genBody (ctxEval ctx) i args body upd_flag
  let f = JFunc [] (ll <> upd <> body)
  ei <- jsEntryIdI i
  et <- genEntryType args
  sr <- genStaticRefs srt
  emitClosureInfo (ClosureInfo (itxt ei) (CIRegs 0 $ PtrV : concatMap idVt args) (itxt ei <> " ," <> T.pack (show i))
                     (fixedLayout $ map (uTypeVt . idType) live) et sr)
  emitToplevel
             [j| `decl ei`;
                 `iex ei` = `f`;
               |]

genEntryType :: [Id] -> G CIType
genEntryType []   = return CIThunk
genEntryType args = do
  args' <- mapM genIdArg args
  return $ CIFun (length args) (length $ concat args')

genSetConInfo :: Id -> DataCon -> SRT -> C
genSetConInfo i d srt = do
  ei <- jsDcEntryIdI i
  sr <- genStaticRefs srt
  emitClosureInfo (ClosureInfo (itxt ei) (CIRegs 0 [PtrV]) (T.pack $ show d) (fixedLayout $ map uTypeVt fields)
                 (CICon $ dataConTag d) sr)
  return [j| `decl ei`;
             `iex ei`     = `mkDataEntry`;
           |]
    where
      fields = dataConRepArgTys d

mkDataEntry :: JExpr
mkDataEntry = ValExpr $ JFunc [] [j| return `Stack`[`Sp`]; |]

genFunInfo :: Text -> [Id] -> JExpr
genFunInfo name as = ValExpr . JList $ [s, jstr name] ++ map (toJExpr . uTypeVt . idType) as
  where
    s = toJExpr (argSize (map idType as) + 1)

argSize :: [Type] -> Int
argSize = sum . map (varSize . uTypeVt)

genUpdFrame :: UpdateFlag -> C
genUpdFrame Updatable = updateThunk <$> use gsSettings
genUpdFrame _         = mempty

-- allocate local closures
allocCls :: [(Id, StgRhs)] -> C
allocCls xs = do
   (stat, dyn) <- splitEithers <$> mapM toCl xs
   cs <- use gsSettings
   return ((mconcat stat) <> allocDynAll cs True dyn)
  where
    -- left = static, right = dynamic
    toCl :: (Id, StgRhs) -> G (Either JStat (Ident,JExpr,[JExpr],CostCentreStack))

    -- statics
    toCl (i, StgRhsCon cc con []) = do
      ii <- jsIdI i
      Left <$> (return (decl ii) <> allocCon ii con [])
    toCl (i, StgRhsCon _cc con [a]) | isUnboxableCon con = do
      ii <- jsIdI i
      Left <$> (return (decl ii) <> (allocCon ii con =<< genArg a))

    -- dynamics
    toCl (i, StgRhsCon cc con ar) =
      -- fixme do we need to handle unboxed?
      Right <$> ((,,,) <$> jsIdI i <*> enterDataCon con <*> concatMapM genArg ar <*> pure cc)
    toCl (i, StgRhsClosure cc _bi live upd_flag _srt _args _body) =
      Right <$> ((,,,) <$> jsIdI i <*> jsEntryId i <*> concatMapM genIds live <*> pure cc)

-- fixme CgCase has a reps_compatible check here
genCase :: ExprCtx -> Id -> StgExpr -> AltType -> [StgAlt] -> StgLiveVars -> SRT -> G (JStat, ExprResult)
genCase top bnd e at alts l srt
  | snd (isInlineExpr (ctxEval top) e) = do
      bndi <- genIdsI bnd
      (ej, r) <- genExpr (bnd, map toJExpr bndi, ctxEval top) e
      when (r == ExprCont) (error "genCase: expression was not inline")
      let d = case r of
                (ExprInline d0) -> d0
                _               -> Nothing
      (aj, ar) <- genAlts (addEval bnd top) bnd at d alts
      return (mconcat (map decl bndi) <> ej <> aj, ar)
  | otherwise = do
      n       <- length <$> genIdsI bnd
      rj      <- genRet (addEval bnd top) bnd at alts l srt
      (ej, r) <- genExpr (bnd, take n (map toJExpr $ enumFrom R1), ctxEval top) e
      return (rj <> ej, ExprCont)

assignAll :: (ToJExpr a, ToJExpr b) => [a] -> [b] -> JStat
assignAll xs ys = mconcat (zipWith assignj xs ys)

assignAllCh :: (ToJExpr a, ToJExpr b) => String -> [a] -> [b] -> JStat
assignAllCh msg xs ys
  | length xs == length ys = mconcat (zipWith assignj xs ys)
  | otherwise              = error ("assignAllCh: lengths do not match: " ++ show (length xs, length ys) ++ "\n    " ++ msg)

genRet :: ExprCtx -> Id -> AltType -> [StgAlt] -> StgLiveVars -> SRT -> C
genRet top e at as l srt = withNewIdent f
  where
    f :: Ident -> C
    f r    =  do
      free    <- optimizeFree (uniqSetToList l)
      pushRet <- pushRetArgs free (iex r)
      fun'    <- fun free
      sr      <- genStaticRefs srt
      emitClosureInfo (ClosureInfo (itxt r) (CIRegs 0 altRegs) (itxt r)
                         (fixedLayout $ map (freeType . fst3) free) CIStackFrame sr)
      emitToplevel $
         decl r <> assignj r (ValExpr . JFunc [] $ fun')
      return pushRet
    fst3 ~(x,_,_)  = x

    -- 2-var values might have been moved around separately, use DoubleV as substitute
    -- ObjV is 1 var, so this is no problem for implicit metadata
    freeType i | varSize otype == 1 = otype
               | otherwise          = DoubleV
      where otype = uTypeVt (idType i)

    altRegs = case at of
      PrimAlt ptc -> tyConVt ptc
      UbxTupAlt n -> idVt e
      _           -> [PtrV]

    fun free = resetSlots $ do
      l <- do decs <- declIds e
              load <- flip assignAll (enumFrom R1) <$> genIdsI e
              return (decs <> load)
      ras  <- loadRetArgs free
      let top' = (ctxTop top, take (length $ ctxTarget top) (map toJExpr $ enumFrom R1), ctxEval top)
      (alts, altr) <- genAlts top' e at Nothing as
      return $ l <> ras <> alts <> [j| return `Stack`[`Sp`]; |]

-- reorder the things we need to push to reuse existing stack values as much as possible
-- True if already on the stack at that location
optimizeFree :: [Id] -> G [(Id,Int,Bool)]
optimizeFree ids = resetSlots $ do
  let ids' = concat $ map (\i -> map (i,) [1..varSize . uTypeVt . idType $ i]) ids
      l    = length ids'
  slots <- take l . (++repeat SlotUnknown) <$> getSlots
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
    where
      showSlot SlotUnknown = return "unknown"
      showSlot (SlotId i n) = do
        (TxtI i') <- jsIdI i
        return (T.unpack i'++"("++show n ++ ")")

loadRetArgs :: [(Id,Int,Bool)] -> C
loadRetArgs free = popSkipI 1 =<< ids
    where
       ids = mapM (\(i,n,b) -> (!!(n-1)) <$> genIdStackArgI i) free

genAlts :: ExprCtx     -- ^ lhs to assign expression result to
        -> Id             -- ^ id being matched
        -> AltType        -- ^ type
        -> Maybe [JExpr]  -- ^ if known, fields in datacon from earlier expression
        -> [StgAlt]       -- ^ the alternatives
        -> G (JStat, ExprResult)
genAlts top e PolyAlt _ [alt] = (\(_,s,r) -> (s,r)) <$> mkAlgBranch top e alt
genAlts top e PolyAlt _ _ = error "genAlts: multiple polyalt"
genAlts top e (PrimAlt tc) _ [(_, bs, use, expr)] = do
  ie       <- genIds e
  dids     <- mconcat (map declIds bs)
  bss      <- concatMapM genIds bs
  (ej, er) <- genExpr top expr
  return (dids <> assignAll {- Ch ("genAlts PrimAlt: " ++ show (idType e)) -} bss ie <> ej, er)
genAlts top e (PrimAlt tc) _ alts = do
  ie <- genIds e
  (r, bss) <- normalizeBranches top <$> mapM (mkPrimIfBranch top (tyConVt tc)) alts
  setSlots []
  return (mkSw ie bss, r)
genAlts top e (UbxTupAlt n) _ [(_, bs, use, expr)] = do
  eids     <- genIds e
  l        <- loadUbxTup eids bs n
  (ej, er) <- genExpr top expr
  return (l <> ej, er)
genAlts top e (AlgAlt tc) _ [alt] | isUnboxedTupleTyCon tc = error "genAlts: unexpected unboxed tuple"
genAlts top e (AlgAlt tc) (Just es) [(DataAlt dc, bs, use, expr)] = do
  bsi <- mapM genIdsI bs
  let bus  = concat $ zipWith (\bss u -> zip bss (repeat u)) bsi use
      args = zipWith (\(i,u) de -> if u then decl i <> assignj i de else mempty) bus es
  (ej, er) <- genExpr top expr
  return (mconcat args <> ej, er)
genAlts top e (AlgAlt tc) _ [alt] = do
  (_,s,r) <- mkAlgBranch top e alt
  return (s, r)
genAlts top e (AlgAlt tc) _ alts@[(DataAlt dc,_,_,_),_]
  | isBoolTy (dataConType dc) = do
      i <- jsId e
      (r, [(_,s1,_), (_,s2,_)]) <- normalizeBranches top <$> mapM (mkAlgBranch top e) alts
      let s = if dataConTag dc == 2 then [j| if(`i`) { `s1` } else { `s2` } |]
                                    else [j| if(`i`) { `s2` } else { `s1` } |]
      setSlots []
      return (s, r)
-- fixme, add all alts
genAlts top e (AlgAlt tc) _ alts = do
      ei <- jsId e
      (r, brs) <- normalizeBranches top <$> mapM (mkAlgBranch top e) alts
      setSlots []
      return (mkSwitch [je| `ei`.f.a |] brs, r)
genAlts top e a _ l = do
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
    mkCont (me, s, ExprInline{}) = (me, s <> assignAll (enumFrom R1) (ctxTarget e), ExprCont)
    mkCont x                     = x

loadUbxTup :: [JExpr] -> [Id] -> Int -> C
loadUbxTup es bs n = do
  bs' <- concatMapM genIdsI bs
  return $ mconcat $ zipWith loadArg bs' es -- (enumFrom R1)
    where
      loadArg b r = decl b <> assignj b r

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
mkAlgBranch top d alt@(a,bs,use,expr) = do
  cc       <- caseCond a
  idd      <- jsId d
  b        <- loadParams idd bs use
  (ej, er) <- genExpr top expr
  return (cc, b <> ej, er)

-- single-var prim
mkPrimBranch :: ExprCtx -> [VarType] -> StgAlt -> G (Maybe JExpr, JStat, ExprResult)
mkPrimBranch top vt (cond, bs, us, e) =
  (\cc (ej,er) -> (cc,ej,er)) <$> caseCond cond <*> genExpr top e

mkPrimIfBranch :: ExprCtx -> [VarType] -> StgAlt -> G (Maybe [JExpr], JStat, ExprResult)
mkPrimIfBranch top vt (cond, bs, us, e) =
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
  return $ case genPrim t op (map toJExpr $ ctxTarget top) as of
             PrimInline s -> (s, ExprInline Nothing)
             PRPrimCall s -> (s, ExprCont)

genStackArg :: StgArg -> G [(JExpr, StackSlot)]
genStackArg a@(StgLitArg _) = map (,SlotUnknown) <$> genArg a
genStackArg a@(StgVarArg i) = zipWith f [1..] <$> genArg a
  where
    f :: Int -> JExpr -> (JExpr, StackSlot)
    f n e = (e, SlotId i n)

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
           return [allocDynamicE cs e as]
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

genStrThunk :: Id -> Bool -> B.ByteString -> C
genStrThunk i nonAscii str = do
  ii@(TxtI iit) <- jsIdI i
  let d = decl ii
  emitStatic iit (StaticThunk Nothing)
  return $ case decodeModifiedUTF8 str of
             Just t -> d <> if nonAscii then [j| `ii` = h$strt(`T.unpack t`); |]
                                        else [j| `ii` = h$strta(`T.unpack t`); |]
             Nothing -> d <> if nonAscii then [j| `ii` = h$strtb(`map toInteger(B.unpack str)`); |]
                                         else [j| `ii` = h$strta(`map (chr.fromIntegral) (B.unpack str)`); |]
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
genLit (MachLabel name size fod)
  | fod == IsFunction = return [ [je| h$mkFunctionPtr(`TxtI . T.pack $ "h$" ++ unpackFS name`) |], [je| 0 |] ]
  | otherwise         = return [ iex (TxtI . T.pack $ "h$" ++ unpackFS name), [je| 0 |] ]
genLit (LitInteger i id) = error ("genLit: LitInteger") -- return [ [je| `intLit i` |] ] -- fixme, convert to bytes and JSBN int?

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
  | isUnboxedTupleCon con && length (ctxTarget tgt) == length args = return $
          assignAll (ctxTarget tgt) args
genCon tgt con args | isUnboxedTupleCon con =
  error ("genCon: unhandled DataCon: " ++ show con ++ " " ++ show (tgt, length args))
genCon tgt con args | [ValExpr (JVar tgti)] <- ctxTarget tgt = allocCon tgti con args
genCon tgt con args =
  error ("genCon: unhandled DataCon: " ++ show con ++ " " ++ show (tgt, length args))

allocCon :: Ident -> DataCon -> [JExpr] -> C
allocCon to con xs
  | isBoolTy (dataConType con) || isUnboxableCon con = do
      return [j| `to` = `allocUnboxedCon con xs`; |]
  | null xs = do
      i <- jsId (dataConWorkId con)
      return (assignj to i)
  | otherwise = do
      e <- enterDataCon con
      cs <- use gsSettings
      return $ allocDynamic cs False to e xs

allocUnboxedCon :: DataCon -> [JExpr] -> JExpr
allocUnboxedCon con []
  | isBoolTy (dataConType con) && dataConTag con == 1 = [je| false |]
  | isBoolTy (dataConType con) && dataConTag con == 2 = [je| true  |]
allocUnboxedCon con [x]
  | isUnboxableCon con = x
allocUnboxedCon con _ = error ("allocUnboxedCon: not an unboxed constructor: " ++ show con)

allocUnboxedConStatic :: DataCon -> [StaticArg] -> StaticArg
allocUnboxedConStatic con []
  | isBoolTy (dataConType con) && dataConTag con == 1      = StaticLitArg (BoolLit False)
  | isBoolTy (dataConType con) && dataConTag con == 2      = StaticLitArg (BoolLit True)
allocUnboxedConStatic _   [a@(StaticLitArg (IntLit i))]    = a
allocUnboxedConStatic _   [a@(StaticLitArg (DoubleLit d))] = a
allocUnboxedConStatic con _                                =
  error ("allocUnboxedConStatic: not an unboxed constructor: " ++ show con)

allocConStatic :: Ident -> DataCon -> [GenStgArg Id] {- -> Bool -} -> G ()
allocConStatic (TxtI to) con args -- isRecursive
{-  | Debug.Trace.trace ("allocConStatic: " ++ show to ++ " " ++ show con ++ " " ++ show args) True -} = do
  as <- mapM genStaticArg args
  allocConStatic' (concat as)
  where
    allocConStatic' :: [StaticArg] -> G ()
    allocConStatic' []
      | isBoolTy (dataConType con) && dataConTag con == 1 =
           emitStatic to $ StaticUnboxed (StaticUnboxedBool False)
      | isBoolTy (dataConType con) && dataConTag con == 2 =
           emitStatic to $ StaticUnboxed (StaticUnboxedBool True)
      | otherwise = do
           (TxtI e) <- enterDataConI con
           emitStatic to (StaticData e [])
    allocConStatic' [x]
      | isUnboxableCon con =
        case x of
          StaticLitArg (IntLit i)    -> emitStatic to (StaticUnboxed $ StaticUnboxedInt i)
          StaticLitArg (BoolLit b)   -> emitStatic to (StaticUnboxed $ StaticUnboxedBool b)
          StaticLitArg (DoubleLit d) -> emitStatic to (StaticUnboxed $ StaticUnboxedDouble d)
          _                          -> error $ "allocConStatic: invalid unboxed literal: " ++ show x
    allocConStatic' xs =
           if con == consDataCon
              then emitStatic to =<< allocateStaticList [args !! 0] (args !! 1)
              else do
                (TxtI e) <- enterDataConI con
                emitStatic to (StaticData e xs)

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
jumpTo' :: JExpr -> [JExpr] -> JStat
jumpTo' fun args = ra <> [j| return `fun`(); |]
  where
      ra = assignAll (enumFrom R2) args

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
  j <- parseFFIPattern False False False ("h$" ++ unpackFS lbl) t (map toJExpr $ ctxTarget top) args
  return (j, ExprInline Nothing)

genForeignCall :: ForeignCall -> Type -> [JExpr] -> [StgArg] -> G (JStat, ExprResult)
genForeignCall (CCall (CCallSpec ccTarget cconv safety)) t tgt args =
  (,exprResult) <$> parseFFIPattern catchExcep async isJsCc lbl t tgt' args
  where
    isJsCc = cconv == JavaScriptCallConv

    lbl | (StaticTarget clbl mpkg isFunPtr) <- ccTarget
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
parseFFIPatternA async javascriptCc pat t es as =
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
        Right (ValExpr (JVar (TxtI ident))) -> mkApply pat
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
    makeHygienic s = snd $ O.renameLocalsFun (map addFFIToken O.newLocals) ([], s)

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
          f n 0 = []
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
isInlineExpr v (StgLit{})                         = (emptyUniqSet, True)
isInlineExpr v (StgConApp{})                      = (emptyUniqSet, True)
isInlineExpr v (StgOpApp (StgFCallOp f _) _ _)    = (emptyUniqSet, isInlineForeignCall f)
isInlineExpr v (StgOpApp (StgPrimOp SeqOp) [StgVarArg e] t) = (emptyUniqSet, e `elementOfUniqSet` v || isStrictType t)
isInlineExpr v (StgOpApp (StgPrimOp op) _ _)      = (emptyUniqSet, isInlinePrimOp op)
isInlineExpr v (StgOpApp (StgPrimCallOp c) _ _)   = (emptyUniqSet, True)
isInlineExpr v (StgLam{})                         = (emptyUniqSet, True)
isInlineExpr v (StgCase e _ _ b _ _ alts)         = let (ve, ie)   = isInlineExpr v e
                                                        v'         = addOneToUniqSet v b
                                                        (vas, ias) = unzip $ map (isInlineExpr v') (alts ^.. traverse . _4)
                                                        vr         = foldl1' intersectUniqSets vas
                                                    in (vr, (ie || b `elementOfUniqSet` v) && and ias)
isInlineExpr v (StgLet b e)                       = isInlineExpr (inspectInlineBinding v b) e
isInlineExpr v (StgLetNoEscape _ _ b e)           = isInlineExpr (inspectInlineBinding v b) e
isInlineExpr v (StgSCC _ _ _ e)                   = isInlineExpr v e
isInlineExpr v (StgTick _ _ e)                    = isInlineExpr v e

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
isInlineApp v i [] = i `elementOfUniqSet` v
isInlineApp v i [StgLitArg (MachStr b)]
  | getUnique i `elem` [unpackCStringIdKey, unpackCStringUtf8IdKey, unpackCStringAppendIdKey] = True
isInlineApp v i args
  | not (might_be_a_function (idType i)) &&
    (isPrimitiveType (idType i) || isStrictType (idType i)) = True
  | otherwise = False

