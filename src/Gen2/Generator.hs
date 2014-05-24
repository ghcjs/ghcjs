{-# LANGUAGE QuasiQuotes, TupleSections, CPP, OverloadedStrings, LambdaCase #-}

{-
  Main generator module
-}

module Gen2.Generator (generate) where

import           ForeignCall
import           FastString
import           TysWiredIn
import           BasicTypes
import           PrelNames
import           DynFlags
import           Encoding
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
import           Data.Char (ord, isDigit)
import           Data.Either (partitionEithers)
import           Data.Function (on)
import           Data.Generics.Aliases (mkT)
import           Data.Generics.Schemes (everywhere)
import qualified Data.IntMap.Strict as IM
import           Data.Monoid
import           Data.Maybe (isJust, fromMaybe)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import           Data.List (partition, intercalate, sort, sortBy)
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

import qualified Debug.Trace

type StgPgm     = [StgBinding]
type StaticRefs = [Id]

generate :: GhcjsSettings
         -> DynFlags
         -> StgPgm
         -> Module
         -> ByteString -- ^ binary data for the .js_o object file
generate settings df s m =
  let (uf, s') = sinkPgm m s
  in  flip evalState (initState df m uf) $ do
        (st, g) <- pass df m s'
        let (p, d) = unzip g
            (st', dbg) = dumpAst st settings df s'
        deps <- genMetaData d
        return . BL.toStrict $
          Object.object' st' deps (p ++ dbg) -- p first, so numbering of linkable units lines up

{- |
  Generate an extra linkable unit for the object file if --debug is active.
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

pass :: DynFlags
     -> Module
     -> StgPgm
     -> G ( Object.SymbolTable                         -- object file symbol names table
          , [(([Text], BL.ByteString), ([Id], [Id]))]
          )

pass df m ss = go 1 Object.emptySymbolTable ss
    where
      go n st (x:xs) = do
        (st', o, d) <- generateBlock st x n
        (st'', ys)  <- go (n+1) st' xs
        return (st'', (o,d):ys)
      go _ st []     = return (st, [])
      generateBlock :: Object.SymbolTable
                    -> StgBinding
                    -> Int
                    -> G (Object.SymbolTable, ([Text], BL.ByteString), ([Id], [Id]))
      generateBlock st decl n = do
        tl      <- genToplevel decl
        extraTl <- use gsToplevelStats
        ci      <- use gsClosureInfo
        si      <- use gsStatic
        unf     <- use gsUnfloated
        resetToplevel
        let allDeps = collectIds unf decl
            topDeps = collectTopIds decl
        (st', ss, bs) <- objectEntry m st topDeps ci si
                           . O.optimize
                           . jsSaturate (Just $ modulePrefix m n)
                           $ mconcat (reverse extraTl) <> tl
        return $! seqList topDeps `seq` seqList allDeps `seq` st' `seq`
                    (st', (ss, bs), (topDeps, allDeps))

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
  return $ Object.Deps (modulePackageText m) (moduleNameText m) ba dm
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
  addInitialized i
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
genToplevelRhs i (StgRhsCon _cc con args) = do
  ii <- jsIdI i
  allocConStatic ii con args
  return mempty
genToplevelRhs i (StgRhsClosure _cc _bi [] Updatable srt [] body) = do
        eid@(TxtI eidt) <- jsEnIdI i
        id@(TxtI idt)   <- jsIdI i
        body0 <- genBody i [] body Updatable i
--        tci <- typeComment i
        sr <- genStaticRefs srt
        cs <- use gsCgSettings
        -- fixme can we skip the first reg here?
        emitClosureInfo (ClosureInfo eidt (CIRegs 0 [PtrV]) idt (CILayoutFixed 0 []) CIThunk sr)
        emitStatic idt (StaticThunk eidt)
        return $ decl eid <> [j| `eid` = `JFunc funArgs (preamble <> updateThunk cs <> body0)`; |]

genToplevelRhs i (StgRhsClosure _cc _bi [] upd_flag srt args body) = do
        eid@(TxtI eidt) <- jsEnIdI i
        id@(TxtI idt) <- jsIdI i
        body0 <- genBody i args body upd_flag i
        et <- genEntryType args
--        tci <- typeComment i
        sr <- genStaticRefs srt
        emitClosureInfo (ClosureInfo eidt
                           (CIRegs 1 $ concatMap idVt args)
                           idt (CILayoutFixed 0 []) et sr)
        emitStatic idt (StaticFun eidt)
        return $ decl eid <> [j| `eid` = `JFunc funArgs (preamble <> body0)`; |]

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

genBody :: Id -> [Id] -> StgExpr -> UpdateFlag -> Id -> C
genBody topid args e upd i = loadArgs args <> b0
    where
      b0      = genExpr topid e

loadArgs :: [Id] -> C
loadArgs args = do
  args' <- concatMapM genIdArgI args
  return (mconcat $ zipWith loadArg args' (enumFrom R2))
   where
     loadArg a reg = decl a <> [j| `a` = `reg`; |]

genExpr :: Id -> StgExpr -> C
genExpr top (StgApp f args)      = genApp False True f args
genExpr top (StgLit l)           = do
  es <- genLit l
  return $ (mconcat (zipWith assign (enumFrom R1) es) <>
              [j| return `Stack`[`Sp`]; |])
    where assign r v = [j| `r` = `v`; |]
genExpr top (StgConApp con args) = genCon con =<< concatMapM genArg args
genExpr top (StgOpApp (StgFCallOp f _) args t) = do
   (fc, async) <- genForeignCall0 f t (map toJExpr $ enumFrom R1) args
   case async of
     False -> return $ fc <> [j| return `Stack`[`Sp`]; |]
     True  -> return fc
genExpr top (StgOpApp (StgPrimOp op) args t) = genPrimOp op args t
genExpr top (StgOpApp (StgPrimCallOp c) args t) = genPrimCall c args t
genExpr top (StgLam{}) = error "genExpr: StgLam"
genExpr top (StgCase e live1 liveRhs b srt at alts) = genCase top b e at alts liveRhs srt
genExpr top (StgLet b e) = genBind top b <> genExpr top e

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

genApp :: Bool -> Bool -> Id -> [StgArg] -> C
-- special cases for unpacking C Strings, avoid going through a typed array when possible
genApp _ _ i [StgLitArg (MachStr bs)]
    | getUnique i == unpackCStringIdKey, Right d <- T.decodeUtf8' bs =
        return [j| `R1` = h$toHsStringA(`d`);
                   return `Stack`[`Sp`];
                 |]
    | getUnique i == unpackCStringUtf8IdKey, Right d <- T.decodeUtf8' bs =
        return [j| `R1` = h$toHsString(`d`);
                   return `Stack`[`Sp`];
                 |]
 -- we could handle unpackNBytes# here, but that's probably not common
 -- enough to warrant a special case
genApp _ _ i [StgLitArg (MachStr bs), x]
    | getUnique i == unpackCStringAppendIdKey, Right d <- T.decodeUtf8' bs = do
        a <- genArg x
        return [j| `R1` = h$appendToHsStringA(`d`, `a`);
                   return `Stack`[`Sp`];
                 |]
genApp force mstackTop i a
    | not (isUnboxedTupleType (idType i)) && (isPrimitiveType (idType i) || isStrictType (idType i))
            = r1 <> return [j| return `Stack`[`Sp`]; |]
    | idRepArity i == 0 && n == 0 && not (might_be_a_function (idType i)) && not (isLocalId i) = do
          ii <- enterId
          cgs <- use gsCgSettings
          if csInlineEnter cgs
             then return [j| var t = `ii`.f;
                             var tt = t.t;
                             `R1` = `ii`;
                             if(tt === `Thunk`) {
                               return t;
                             } else if(tt === `Blackhole`) {
                               return h$ap_0_0_fast();
                             } else { // con
                               return `stackTop`; // stack[sp];
                             }
                           |]
             else return [j| return h$e(`ii`); |]
    | idRepArity i == 0 && n == 0 && not (might_be_a_function (idType i))
          = do
             ii <- enterId
             cgs <- use gsCgSettings
             if csInlineEnter cgs
                then return [j| var t = `ii`.f;
                                var tt = t.t;
                                `R1` = `ii`;
                                if(tt === `Thunk`) {
                                  return t;
                                } else if(tt === `Blackhole`) {
                                  return h$ap_0_0_fast();
                                } else { // con
                                  return `stackTop`;
                                }
                              |]
                else return [j| return h$e(`ii`); |]
    | idRepArity i == n && not (isLocalId i) && n /= 0 = do
        as' <- concatMapM genArg a
        jumpToII i as' =<< r1
    | idRepArity i < n && idRepArity i > 0 =
         let (reg,over) = splitAt (idRepArity i) a
         in  do
           reg' <- concatMapM genArg reg
           pushCont over <> (jumpToII i reg' =<< r1)
    | otherwise = jumpToFast a =<< r1
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
genBind :: Id -> StgBinding -> C
genBind top bndr =
  case bndr of
    (StgNonRec b r) -> assign b r >> allocCls [(b,r)]
    (StgRec bs)     -> mapM_ (uncurry assign) bs >> allocCls bs
   where
     assign :: Id -> StgRhs -> G ()
     assign b r = genEntry top b r

-- generate the entry function for a local closure
genEntry :: Id -> Id -> StgRhs -> G ()
genEntry top i (StgRhsCon _cc con args) = return () -- mempty -- error "local data entry" -- mempty ??
genEntry top i (StgRhsClosure _cc _bi live Updatable srt [] (StgApp fun args)) = resetSlots $ do
  upd <- genUpdFrame Updatable
  ll <- loadLiveFun live
  app <- genApp False True fun args
  let f = JFunc funArgs $ preamble <> ll <> upd <> app
  ie <- jsEntryIdI i
  et <- genEntryType []
  sr <- genStaticRefs srt
  emitClosureInfo (ClosureInfo (itxt ie) (CIRegs 0 [PtrV]) (itxt ie <> " ," <> T.pack (show i))
                     (fixedLayout $ map (uTypeVt . idType) live) et sr)
-- fixme args correct here?
  emitToplevel
     [j| `decl ie`;
         `iex ie` = `f`;
       |]

genEntry top i cl@(StgRhsClosure _cc _bi live upd_flag srt args body) = resetSlots $ do
  ll <- loadLiveFun live
  upd <- genUpdFrame upd_flag
  body <- genBody top args body upd_flag i
  let f = JFunc funArgs (preamble <> ll <> upd <> body)
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
mkDataEntry = ValExpr $ JFunc funArgs [j| `preamble`; return `Stack`[`Sp`]; |]

genFunInfo :: Text -> [Id] -> JExpr
genFunInfo name as = ValExpr . JList $ [s, jstr name] ++ map (toJExpr . uTypeVt . idType) as
  where
    s = toJExpr (argSize (map idType as) + 1)

argSize :: [Type] -> Int
argSize = sum . map (varSize . uTypeVt)

genUpdFrame :: UpdateFlag -> C
genUpdFrame Updatable = updateThunk <$> use gsCgSettings
genUpdFrame _         = mempty

-- allocate local closures
allocCls :: [(Id, StgRhs)] -> C
allocCls xs = do
   (stat, dyn) <- splitEithers <$> mapM toCl xs
   cs <- use gsCgSettings
   return ((mconcat stat) <> allocDynAll cs True dyn)
  where
    -- left = static, right = dynamic
    toCl :: (Id, StgRhs) -> G (Either JStat (Ident,JExpr,[JExpr]))
    toCl (i, StgRhsCon _cc con []) = do
      ii <- jsIdI i
      Left <$> allocCon ii con []
    toCl (i, StgRhsCon _cc con [a]) | isUnboxableCon con = do
      ii <- jsIdI i
      Left <$> (allocCon ii con =<< genArg a)
    toCl (i, StgRhsCon _cc con ar) = Right <$> ((,,) <$> jsIdI i <*> enterDataCon con <*> concatMapM genArg ar)  -- fixme do we need to handle unboxed?
    toCl (i, StgRhsClosure _cc _bi live upd_flag _srt _args _body) =
        Right <$> ((,,) <$> jsIdI i <*> jsEntryId i <*> concatMapM genIds live)

-- fixme CgCase has a reps_compatible check here
genCase :: Id -> Id -> StgExpr -> AltType -> [StgAlt] -> StgLiveVars -> SRT -> C
genCase top bnd (StgApp i []) at@(PrimAlt tc) alts l srt
  | isUnLiftedType (idType i) = do
    ibnd <- jsId bnd
    ii   <- jsId i
    declIds bnd <> return [j| `ibnd` = `ii`; |]
      <> genInlinePrimCase top bnd (tyConVt tc) at alts

genCase top bnd (StgOpApp (StgPrimOp SeqOp) [StgVarArg a, _] _) at alts l srt =
  genCase top bnd (StgApp a []) at alts l srt

genCase top bnd (StgLit lit) at@(PrimAlt tc) [(_,[],_,e)] l srt = do
  ibnd <- genIdsI bnd
  ls <- genLit lit
  declIds bnd <>
    return (mconcat $ zipWith (\b e -> [j| `b` = `e` |]) ibnd ls) <>
    genExpr top e

genCase top bnd (StgApp i xs) at alts l srt =
  genRet top bnd at alts l srt <> genApp True False i xs

-- fixme?
genCase top bnd x@(StgCase {}) at alts l srt =
  genRet top bnd at alts l srt <> genExpr top x

genCase top bnd (StgOpApp (StgFCallOp fc _) args t) at alts@[(DataAlt{}, bndrs, _, e)] l srt =
  do
   ids <- concatMapM genIds bndrs
   (fc, async) <- genForeignCall0 fc t ids args
   case async of
     False -> concatMapM declIds bndrs <> return fc <> genExpr top e
     True -> genRet top bnd at alts l srt <> return fc

-- foreign calls can return ADT's directly in GHCJS
genCase top bnd (StgOpApp (StgFCallOp fc _) args t) at alts l srt =
  do
   ids <- genIds bnd
   (fc, async) <- genForeignCall0 fc t ids args
   case async of
     False -> declIds bnd <> return fc <> genAlts top bnd at alts
     True  -> genRet top bnd at alts l srt <> return fc

genCase top bnd (StgOpApp (StgPrimCallOp (PrimCall lbl _)) args t) at@(PrimAlt tc) alts l srt =
  do
    ids <- genIds bnd
    declIds bnd <>
      parseFFIPattern False False False ("h$" ++ unpackFS lbl) t ids args <>
      genAlts top bnd at alts

genCase top bnd (StgOpApp (StgPrimCallOp (PrimCall lbl _)) args t) _ [(DataAlt{}, bndrs, _, e)] l srt = do
  ids <- concatMapM genIds bndrs
  concatMapM declIds bndrs <>
     parseFFIPattern False False False ("h$" ++ unpackFS lbl) t ids args <>
     genExpr top e

-- pattern match on an unboxed tuple
genCase top bnd (StgOpApp (StgPrimOp p) args t) at@(UbxTupAlt n) alts@[(DataAlt{}, bndrs, _, e)] l srt = do
  args' <- concatMapM genArg args
  ids <- concatMapM genIds bndrs
  case genPrim t p ids args' of
      PrimInline s -> mconcat (map declIds bndrs) <> return s <> genExpr top e
      PRPrimCall s -> genRet top bnd at alts l srt <> return s

-- other primop
genCase top bnd x@(StgOpApp (StgPrimOp p) args t) at alts l srt = do
    args' <- concatMapM genArg args
    ids   <- genIds bnd
    case genPrim t p ids args' of
      PrimInline s -> declIds bnd <> return s <> genInlinePrimCase top bnd (typeVt t) at alts
      PRPrimCall s -> genRet top bnd at alts l srt <> return s

genCase top bnd x@(StgConApp c as) at [(DataAlt{}, bndrs, _, e)] l srt = do
  args' <- concatMapM genArg as
  ids   <- concatMapM genIds bndrs
  bndi  <- jsIdI bnd
  declIds bnd <> allocCon bndi c args' <> mconcat (map declIds bndrs)
              <> return (assignAll ids args') <> genExpr top e

genCase top bnd expr at alts l srt = do
  genRet top bnd at alts l srt <> genExpr top expr

genCase _ _ x at alts _ _ =
  error $ "genCase: unhandled format: " ++ show x ++ "\n"
            ++ show at ++ "\n" ++ show alts

assignAll :: (ToJExpr a, ToJExpr b) => [a] -> [b] -> JStat
assignAll xs ys = mconcat (zipWith assignj xs ys)

assignj :: (ToJExpr a, ToJExpr b) => a -> b -> JStat
assignj x y = [j| `x` = `y` |]

-- simple inline prim case, no return function needed
genInlinePrimCase :: Id -> Id -> [VarType] -> AltType -> [StgAlt] -> C
genInlinePrimCase top bnd tc _ [(DEFAULT, bs, used, e)] = genExpr top e
-- some primops return ADT's, we can assume they're in WHNF
genInlinePrimCase top bnd tc (AlgAlt dtc) alts@[_,(DataAlt dc,_,_,_)]
    | isBoolTy (dataConType dc) && dataConTag dc == 1 = do
        i <- jsId bnd
        [b1,b2] <- mapM (fmap snd . mkPrimIfBranch top tc) alts
        return [j| if(`i`) { `b1` } else { `b2` } |]
    | isBoolTy (dataConType dc) && dataConTag dc == 2 = do
        i <- jsId bnd
        [b1,b2] <- mapM (fmap snd . mkPrimIfBranch top tc) alts
        return [j| if(`i`) { `b2` } else { `b1` } |]
genInlinePrimCase top bnd tc (AlgAlt _) alts = do
        i <- jsId bnd
        mkSwitch [je| `i`.f.a |] <$> mapM (mkPrimBranch top tc) alts
genInlinePrimCase top bnd tc (PrimAlt ptc) alts
    | isMatchable tc    = liftM2 mkSwitch (jsId bnd) (mapM (mkPrimBranch top tc) alts)
    | otherwise         = liftM2 mkIfElse (genIdArg bnd) (mapM (mkPrimIfBranch top tc) alts)
genInlinePrimCase top bnd tc (UbxTupAlt n) [(DataAlt _, bndrs, _, body)] =
    loadUbxTup bndrs n <> genExpr top body
genInlinePrimCase _ _ _ at alt = error ("genInlinePrimCase: unhandled alt: (" ++
   show at ++ "," ++ show (length alt) ++ "]) " ++ show alt)


genRet :: Id -> Id -> AltType -> [StgAlt] -> StgLiveVars -> SRT -> C
genRet top e at as l srt = withNewIdent f
  where
    f :: Ident -> C
    f r    =  do
      free <- optimizeFree (uniqSetToList l)
      pushRet <- pushRetArgs free (iex r)
      fun' <- fun free
      topi <- jsIdI top
      tc <- typeComment e
      sr <- genStaticRefs srt
      emitClosureInfo (ClosureInfo (itxt r) (CIRegs 0 altRegs) (itxt r)
                         (fixedLayout $ map (freeType . fst3) free) CIStackFrame sr)
      emitToplevel $
         decl r <> [j| `r` = `fun'`; |]
      return pushRet
    fst3 ~(x,_,_)  = x

    -- 2-var values might have been moved around separately, use DoubleV as substitute
    -- ObjV is 1 var, so this is no problem for implicit metadata
    freeType i | varSize otype == 1 = otype
               | otherwise          = DoubleV
      where otype = uTypeVt (idType i)

    altRegs = case at of
      PrimAlt ptc -> tyConVt ptc
      _           -> [PtrV]

    isBoxedAlt = case at of
                   PrimAlt {} -> False
                   _          -> True
    fun free = resetSlots $ do
      l <- if isUnboxedTupleType (idType e)
             then return mempty
             else do
               decs <- declIds e
               load <- zipWith (\r i -> [j| `i`=`r`; |]) (enumFrom R1) <$> genIdsI e
               return (decs <> mconcat load)
      ras  <- loadRetArgs free
      alts <- genAlts top e at as
      return . ValExpr $ JFunc funArgs $ preamble <> l <> ras <> alts

-- reorder the things we need to push to reuse existing stack values as much as possible
-- True if already on the stack at that location
optimizeFree :: [Id] -> G [(Id,Int,Bool)]
optimizeFree ids = do
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
  return ({- c <> -} p)
    where
      showSlot SlotUnknown = return "unknown"
      showSlot (SlotId i n) = do
        (TxtI i') <- jsIdI i
        return (T.unpack i'++"("++show n ++ ")")

loadRetArgs :: [(Id,Int,Bool)] -> C
loadRetArgs free = popSkipI 1 =<< ids
    where
       ids = mapM (\(i,n,b) -> (!!(n-1)) <$> genIdStackArgI i) free

genAlts :: Id -> Id -> AltType -> [StgAlt] -> C
genAlts top e PolyAlt [alt] = snd <$> mkAlgBranch top e alt
genAlts top e PolyAlt _ = error "genAlts: multiple polyalt"
genAlts top e (PrimAlt tc) [(_, bs, use, expr)] = do
  ie <- genIds e
{-  loadParams ie bs use <> -}
  bss <- concatMapM genIds bs
  mconcat (map declIds bs) <> return (assignAll bss ie) <> genExpr top expr
genAlts top e (PrimAlt tc) alts = do
  ie <- genIds e
  mkSw ie <$> mapM (mkPrimIfBranch top (tyConVt tc)) alts
-- genAlts r e (PrimAlt tc) alts = mkSwitch [je| heap[r1] |] (map (mkPrimBranch r e) alts)
genAlts top e (UbxTupAlt n) [(_, bs, use, expr)] = loadUbxTup bs n <> genExpr top expr
--genAlts r e (AlgAlt tc) [alt] = mkSwitch [snd (mkAlgBranch r e alt)
genAlts top e (AlgAlt tc) [alt] | isUnboxedTupleTyCon tc = error "genAlts: unexpected unboxed tuple"
genAlts top e (AlgAlt tc) [alt] = snd <$> mkAlgBranch top e alt
genAlts top e (AlgAlt tc) alts@[(DataAlt dc,_,_,_),_]
  | isBoolTy (dataConType dc) && dataConTag dc == 2 = do
      i <- jsId e
      [b1,b2] <- mapM (fmap snd . mkAlgBranch top e) alts
      return [j| if(`i`) { `b1` } else { `b2` } |]
  | isBoolTy (dataConType dc) && dataConTag dc == 1 = do
      i <- jsId e
      [b1,b2] <- mapM (fmap snd . mkAlgBranch top e) alts
      return [j| if(`i`) { `b2` } else { `b1` } |]
-- fixme, add all alts
genAlts top e (AlgAlt tc) alts
--  | isEnumerationTyCon tc = do
--      i <- jsId e
--      mkSwitch [je| (`i`===true)?2:((typeof `i` === 'object')?(`i`.f.a):1) |] <$> mapM (mkAlgBranch top e) alts
  | otherwise           = do
      ei <- jsId e
      mkSwitch [je| `ei`.f.a |] <$> mapM (mkAlgBranch top e) alts
genAlts top e a l = do
  ap <- showPpr' a
  error $ "genAlts: unhandled case variant: " ++ ap ++ " (" ++ show (length l) ++ ")"

loadUbxTup :: [Id] -> Int -> C
loadUbxTup bs n = do
  bs' <- concatMapM genIdsI bs
  return $ mconcat $ zipWith loadArg bs' (enumFrom R1)
    where
      loadArg b r = decl b <> [j| `b` = `r`; |]

mkSw :: [JExpr] -> [(Maybe [JExpr], JStat)] -> JStat
mkSw [e] cases = mkSwitch e (over (mapped._1.mapped) head cases)
mkSw es cases = mkIfElse es cases

-- switch for pattern matching on constructors or prims
mkSwitch :: JExpr -> [(Maybe JExpr, JStat)] -> JStat
mkSwitch e cases
    | [(Just c1,s1)] <- n, [(_,s2)] <- d = IfStat [je| `e` === `c1` |] s1 s2
    | [(Just c1,s1),(_,s2)] <- n, null d = IfStat [je| `e` === `c1` |] s1 s2
    | null d        = SwitchStat e (map addBreak (init n)) (snd $ last n)
    | [(_,d0)] <- d = SwitchStat e (map addBreak n) d0
    | otherwise     = error "mkSwitch: multiple default cases"
    where
      addBreak (Just c,s) = (c, s) -- [j| `s`; break |]) -- fixme: rename, does not add break anymore
      addBreak _          = error "mkSwitch: addBreak"
      (n,d) = partition (isJust.fst) cases

-- if/else for pattern matching on things that js cannot switch on
mkIfElse :: [JExpr] -> [(Maybe [JExpr], JStat)] -> JStat
mkIfElse e s = go (reverse $ sort s)
    where
      go [] = error "mkIfElse: empty expression list"
      go [(_, s)] = s -- only one 'nothing' allowed
      go ((Just e0, s):xs) =
          [j| if( `mkEq e e0` ) { `s` } else { `go xs` } |]
      go _ = error "mkIfElse: multiple DEFAULT cases"

mkEq :: [JExpr] -> [JExpr] -> JExpr
mkEq es1 es2
  | length es1 == length es2 = foldl1 and (zipWith eq es1 es2)
  | otherwise                = error "mkEq: incompatible expressions"
    where
      and e1 e2 = [je| `e1` && `e2`  |]
      eq  e1 e2 = [je| `e1` === `e2` |]


mkAlgBranch :: Id -> Id -> StgAlt -> G (Maybe JExpr, JStat)
mkAlgBranch top d (DataAlt dc,[b],_,expr)
  | isUnboxableCon dc = isolateSlots $ do
      idd   <- jsId d
      [fld] <- genIdsI b
      e     <- genExpr top expr
      return (Nothing, decl fld <> [j| `fld` = `idd` |] <> e)
mkAlgBranch top d alt@(a,bs,use,expr) = isolateSlots $ 
  (,) <$> caseCond a <*> b
    where
      b = (jsId d >>= \idd -> loadParams idd bs use) <> genExpr top expr

-- single-var prim
mkPrimBranch :: Id -> [VarType] -> StgAlt -> G (Maybe JExpr, JStat)
mkPrimBranch top vt (DEFAULT, bs, us, e) = isolateSlots $
  (Nothing,) <$> genExpr top e
mkPrimBranch top vt (cond,    bs, us, e) = isolateSlots $ do
  (,) <$> caseCond cond <*> genExpr top e

-- possibly multi-var prim
-- fixme load binders?
mkPrimIfBranch :: Id -> [VarType] -> StgAlt -> G (Maybe [JExpr], JStat)
mkPrimIfBranch top vt (DEFAULT, bs, us, e) = isolateSlots $ do
--  (Nothing,) <$> genExpr top e
  expr <- genExpr top e
  return (Nothing, expr)
mkPrimIfBranch top vt (cond,    bs, us, e) = isolateSlots $ do
--  dec <- concatMapM declIds bs -- do we need this?
  (,) <$> ifCond cond <*> genExpr top e


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
  i <- makeIdent
--  p <- loadParams' (toJExpr i) args use
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

genPrimOp :: PrimOp -> [StgArg] -> Type -> C
genPrimOp op args t = do
  as <- concatMapM genArg args
  case genPrim t op rs as of
     PrimInline s -> return $ s <> [j| return `Stack`[`Sp`]; |]
     PRPrimCall s -> return s
    where
      rs = map toJExpr $ take (typeSize t) (enumFrom R1)

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
           cs <- use gsCgSettings
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
  case T.decodeUtf8' str of
    Right t -> return (mempty, [toJExpr $ T.unpack t])
    Left  _ -> error "cannot encode FFI string literal"
genFFIArg (StgLitArg l) = (mempty,) <$> genLit l
genFFIArg a@(StgVarArg i)
    | isVoid r                  = return (mempty, [])
--    | Just x <- marshalFFIArg a = x
    | isMultiVar r              = (mempty,) <$> mapM (jsIdN i) [1..varSize r]
    | otherwise                 = (\x -> (mempty,[x])) <$> jsId i
   where
     r = uTypeVt . stgArgType $ a

genIdArg :: Id -> G [JExpr]
genIdArg i
    | isVoid r     = return []
    | isMultiVar r = mapM (jsIdN i) [1..varSize r]
    | otherwise    = (:[]) <$> jsId i
    where
      r = uTypeVt . idType $ i

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

genLit :: Literal -> G [JExpr]
genLit (MachChar c)      = return [ [je| `ord c` |] ]
genLit (MachStr  str)    =
  case T.decodeUtf8' str of
    Right t -> withNewIdent $ \ident -> do
      emitToplevel [j| `decl ident`;
                       `ident` = h$str(`T.unpack t`);
                     |]
      return [ [je| `ident`() |], [je| 0 |] ]
    Left  _ -> withNewIdent $ \ident -> do
      emitToplevel [j| `decl ident`;
                       `ident` = h$rstr(`map toInteger (B.unpack str)`);
                     |]
      return [ [je| `ident`() |], [je| 0 |] ]
genLit MachNullAddr      = return [ [je| null |], [je| 0 |] ]
genLit (MachInt i)       = return [ [je| `i` |] ]
genLit (MachInt64 i)     = return [ [je| `shiftR i 32` |] , [je| `toSigned i` |] ]
genLit (MachWord w)      = return [ [je| `toSigned w` |] ]
genLit (MachWord64 w)    = return [ [je| `toSigned (shiftR w 32)` |] , [je| `toSigned w` |] ]
genLit (MachFloat r)     = return [ [je| `r2d r` |] ]
genLit (MachDouble r)    = return [ [je| `r2d r` |] ]
genLit (MachLabel name size fod)
  | fod == IsFunction = return [ [je| h$mkFunctionPtr(`TxtI . T.pack $ "h$" ++ unpackFS name`) |], [je| 0 |] ]
  | otherwise         = return [ iex (TxtI . T.pack $ "h$" ++ unpackFS name), [je| 0 |] ]
genLit (LitInteger i id) = return [ [je| `i` |] ] -- fixme, convert to bytes and JSBN int?

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

-- make a signed 32 bit int from this unsigned one, lower 32 bits
toSigned :: Integer -> Integer
toSigned i | testBit i 31 = complement (0x7FFFFFFF `xor` (i.&.0x7FFFFFFF))
           | otherwise    = i.&.0xFFFFFFFF

genSingleLit :: Literal -> G JExpr
genSingleLit l = do
  es <- genLit l
  case es of
    [e] -> return e
    _   -> error "genSingleLit: expected single-variable literal"

genCon :: DataCon -> [JExpr] -> C
genCon con args
  | isUnboxedTupleCon con = return $ -- unboxed tuple returned in registers
      mconcat (zipWith (\reg e -> [j| `reg` = `e`; |]) (enumFrom R1) args) <>
      [j| return `Stack`[`Sp`]; |]
  | otherwise = do
      di <- makeIdent
      alloc <- allocCon di con args
      return [j| `alloc`;
                 `R1` = `di`;
                 return `Stack`[`Sp`];
               |]

allocCon :: Ident -> DataCon -> [JExpr] -> C
allocCon to con xs
  | isBoolTy (dataConType con) || isUnboxableCon con = do
      return $ decl to <> [j| `to` = `allocUnboxedCon con xs`; |]
  | null xs = do
      i <- jsId (dataConWorkId con)
      return $ decl to <> [j| `to` = `i`; |]
  | otherwise = do
      e <- enterDataCon con
      cs <- use gsCgSettings
      return $ allocDynamic cs True to e xs

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
     return ( mconcat ra <> afterLoad <> [j| return `ii`.f; |])
  | otherwise   = do
     ei <- jsEntryId i
     return (mconcat ra <> afterLoad <> [j| return `ei`; |])
  where
    ra = reverse $ zipWith (\r e -> [j| `r` = `e` |]) (enumFrom R2) args


-- load arguments and jump to fun directly (not going through trampoline)
jumpTo' :: JExpr -> [JExpr] -> JStat
jumpTo' fun args = mconcat ra <> [j| return `fun`(); |]
  where
      ra = zipWith (\r e -> [j| `r` = `e` |]) (enumFrom R2) args

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
selectApply :: Bool     ->    -- ^ true for fast apply, false for stack apply
               ([StgArg], [JExpr]) ->    -- ^ arguments
               G (JExpr,Bool) -- ^ the function to call, true if specialized path
selectApply fast (args, as) = do
  case specApply fast (length args) (length as) of
    Just e  -> return (e, True)
    Nothing -> return (jsv $ "h$ap_gen" <> fastSuff, False)
  where
    fastSuff | fast      = "_fast"
             | otherwise = ""

-- insert delimiters around block so linker can extract this efficiently
-- abuses PPostStat to insert a comment!
{-
delimitBlock :: Module -> [Id] -> JStat -> C
delimitBlock m i s = do
  b <- block
  return $ PPostStat True (start b) emptye <> s <> PPostStat True (end b) emptye
  where
    idStr i = istr <$> jsIdI i
    block  = L.intercalate "," <$> mapM idStr i
    emptye = iex (StrI "")
    start block = T.unpack Linker.startMarker ++ block ++ ">"
    end   block = T.unpack Linker.endMarker ++ block ++ ">"
-}

-- this hack doesn't work anymore
comment :: String -> JStat
comment xs =  mempty -- True ("// " ++ xs) (iex $ TxtI "")


typeComment :: Id -> C
typeComment i = do
  tc <- csAssertRts <$> use gsCgSettings
  if not tc
    then return mempty
    else do
      si <- sh i
      sit <- sh (idType i)
      return (comment $ si ++ " :: " ++ sit)
        where
          sh x = map (\x -> if x == '\n' then ' ' else x) <$> showPpr' x

-- fixme: what if the call returns a thunk?
genPrimCall :: PrimCall -> [StgArg] -> Type -> C
genPrimCall (PrimCall lbl _) args t =
  parseFFIPattern False False False ("h$" ++ unpackFS lbl) t tgt args <> return [j| return `Stack`[`Sp`]; |]
  where
    tgt = map toJExpr . take (typeSize t) $ enumFrom R1

genForeignCall0 :: ForeignCall -> Type -> [JExpr] -> [StgArg] -> G (JStat, Bool)
genForeignCall0 (CCall (CCallSpec (StaticTarget clbl mpkg isFunPtr) JavaScriptCallConv safe)) t tgt args
  = (,async) <$> parseFFIPattern catchExcep async True (unpackFS clbl) t tgt args
  where
    catchExcep = playSafe safe || playInterruptible safe
    async      = playInterruptible safe
genForeignCall0 (CCall (CCallSpec (StaticTarget clbl mpkg isFunPtr) conv safe)) t tgt args
  = (,False) <$> parseFFIPattern False False False lbl t tgt args
    where
      cl = unpackFS clbl
      lbl | wrapperPrefix `L.isPrefixOf` cl =
              ("h$" ++ (drop 2 $ dropWhile isDigit $ drop (length wrapperPrefix) cl))
          | otherwise = "h$" ++ cl
      wrapperPrefix = "ghczuwrapperZC"
genForeignCall0 (CCall (CCallSpec DynamicTarget conv safe)) t tgt args = return (mempty,False) -- fixme panic "unsupported foreign call"
genForeignCall0 _ _ _ _ = error "genForeignCall0: unsupported foreign call"

{-
-- fixme: what if the call returns a thunk?
-- fixme: deal with safety and calling conventions
-}
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

parseFFIPatternA :: Bool  -- ^ async (only valid with javascript calling conv)
                 -> Bool  -- ^ javascript calling conv
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
               return `Stack`[`Sp`];
             }
           |]
     where nrst = typeSize t
           copyResult d = mconcat $ zipWith
              (\r i -> [j| `r` = `d`[`i`]; |]) (enumFrom R1) [0..nrst-1]
parseFFIPatternA False javascriptCc pat t es as =
  parseFFIPattern' Nothing javascriptCc pat t es as
parseFFIPatternA _ _ _ _ _ _ = error "parseFFIPattern: non-JavaScript pattern must be synchronous"

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
         cs <- use gsCgSettings
         return $ traceCall cs as <> mconcat stats <> ApplStat f' (concat as++[cb])
      | (ts@(_:_)) <- tgt = do
         (stats, as) <- unzip <$> mapM genFFIArg args
         (statR, (t:ts')) <- return (mempty, ts)
         cs <- use gsCgSettings
         return $ traceCall cs as
                <> mconcat stats
                <> [j| `t` = `ApplExpr f' (concat as)`; |]
                <> copyResult ts'
                <> statR
      | otherwise = do
         (stats, as) <- unzip <$> mapM genFFIArg args
         cs <- use gsCgSettings
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
