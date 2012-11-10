{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

{-
  Main generator module
-}

module Gen2.Generator (generate) where

import StgCmmClosure
import ClosureInfo hiding (ClosureInfo)
import Outputable hiding ((<>))
import FastString
import DynFlags
import Encoding
import UniqSet
import Literal
import DataCon
import CoreSyn
import TcType
import Unique
import StgSyn
import PrimOp
import Module
import TyCon
import Util
import Type
import Name
import Id

import Data.Char (ord)
import Data.Bits ((.|.), shiftL)
import Data.ByteString (ByteString)
import Data.Serialize
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Maybe (isJust, fromMaybe)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.List (partition, intercalate, sort, find)
import qualified Data.List as L
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text as T
import Text.PrettyPrint.Leijen.Text hiding ((<>), pretty)
import Language.Javascript.JMacro

import Gen2.Floater
import Gen2.Utils
import Gen2.Prim
import Gen2.Rts
import Gen2.RtsTypes
import Gen2.StgAst
import Gen2.Debug
import Gen2.RtsAlloc
import Gen2.RtsApply
import Gen2.RtsSettings
import Gen2.Printer
import qualified Gen2.Linker as Linker

import qualified Gen2.Optimizer as O

import Data.Generics.Aliases
import Data.Generics.Schemes

type StgPgm     = [(StgBinding, [(Id, [Id])])]
type StaticRefs = [Id]

data GenerateSettings = GenerateSettings
       { gsIncludeRts     :: Bool -- include the rts in the generated file
       , gsRunMain        :: Bool -- immediately run the main IO action
       , gsTraceFunctions :: Bool -- trace all function calls (not just trampoline)
       , gsNodeModule     :: Bool -- export our global variables to make a node module
       }

-- fixme remove this hack to run main and use the settings
generate :: StgPgm -> Module -> (ByteString, ByteString)  -- module,metadata
generate s m = (result, deps)
  where
    prr doc = let doc' = doc <> exportGlobals doc
              in (displayT . renderPretty 0.8 150 . pretty . jsSaturate Nothing $ doc') <> TL.pack "\n"
    p1      = pass1 s
    js      = pass2 m p1
    deps    = runPut . put . genMetaData m $ p1

    result  = BL.toStrict $ TL.encodeUtf8 ( dumpAst <> prr ( {- addDebug $ -} rts <> js ) <> runHack)

    -- fixme remove
    dumpAst = TL.pack (intercalate "\n\n" (map ((\x -> "/*\n"++x++" */\n").showIndent.fst) s))
    runHack =  TL.pack "run_init_static(); \n debugger;\n function main() { try { " <> prr (runMainIO m) <> TL.pack "\n} catch(e) { log(e.stack); debugger; log('exception, r1: ' + r1); dumpStack(stack,sp+5); dh(); throw e; } }\nif(typeof($) === 'undefined') { main(); }\ndebugger;\n"


{-
srts :: [[(Id, [Id])]] -> JStat
srts xss = mconcat $ map (\xs -> [j| srts = `mconcat $ map toSrt xs`; |]) xss
  where
    toSrt (id, ids) = istr (jsIdI id) .= map jsId ids
-}

-- collect globals and export them, to make our source a node module
exportGlobals :: JStat -> JStat
exportGlobals s = [j| if(typeof exports !== "undefined") { `exp s`; } |]
  where exp = mconcat . map exportVar . collectDecls . everywhere (mkT removeFunc)

collectDecls :: JStat -> [Ident]
collectDecls = map fromDecl . listify isDecl
    where
      isDecl (DeclStat {}) = True
      isDecl _             = False
      fromDecl (DeclStat i _) = i

exportVar :: Ident -> JStat
exportVar i = AssignStat (SelExpr [je|exports|] i) (ValExpr (JVar i))

removeFunc :: JVal -> JVal
removeFunc (JFunc {}) = JInt 0
removeFunc x          = x

-- | variable prefix for the nth block in module
modulePrefix :: Module -> Int -> String
modulePrefix m n = "$hs_" ++ (zEncodeString . moduleNameString . moduleName $ m) ++ "_id_" ++ show n

runMainIO :: Module -> JStat
runMainIO m = [j|
  runhs(runio(`main`), dumpRes);
 |]
    where
      main = JVar . StrI $ "$hs_" ++ (zEncodeString . moduleNameString . moduleName) m ++ "zimain"

-- | pass1 generates unsaturated blocks and dependency info
pass1 :: StgPgm
      -> [(JStat, [Id], [Id])] -- | for each function block: js code, defined ids, dependencies
pass1 ss = map generateBlock ss
  where
    generateBlock d@(decl,deps) =
      let ids = map fst deps
      in  (genToplevel d, ids, L.nub (concatMap snd deps))

-- | pass2 combines and saturates the AST and runs the transformations
pass2 :: Module -> [(JStat,[Id],[Id])] -> JStat
pass2 m = mconcat . zipWith p2 [1..]
  where
    p2 n (s,ids,_) = delimitBlock ids
                   . floatTop
                   . jsSaturate (Just $ modulePrefix m n)
                   $ s

genMetaData :: Module -> [(a, [Id], [Id])] -> Linker.Deps
genMetaData m p1 = Linker.Deps (M.fromList $ concatMap oneDep p1)
  where
    oneDep (_, symbs, deps) = map (symbDep deps) symbs
    symbDep deps symb = (idTxt symb, S.fromList $ map idFun deps)
    idTxt i = let (StrI xs) = jsIdI i in T.pack xs
    idFun i = Linker.Fun (T.pack "fixme_package") (T.pack "fixme_module") (idTxt i) -- fixme


genToplevel :: (StgBinding, [(Id, [Id])]) -> JStat
genToplevel (StgNonRec bndr rhs, srts) = genToplevelDecl bndr rhs (lookupStaticRefs bndr srts)
genToplevel (StgRec bs, srts)          = mconcat $ map (\(bndr, rhs) -> genToplevelDecl bndr rhs (lookupStaticRefs bndr srts)) bs

lookupStaticRefs :: Id -> [(Id, [Id])] -> StaticRefs
lookupStaticRefs i xs = fromMaybe [] (lookup i xs)

genToplevelDecl :: Id -> StgRhs -> StaticRefs -> JStat
genToplevelDecl i rhs srs = genToplevelConEntry i rhs <> genToplevelRhs i rhs srs

genToplevelConEntry :: Id -> StgRhs -> JStat
genToplevelConEntry i (StgRhsCon _cc con args)
    | i `elem` dataConImplicitIds con = genSetConInfo i con
genToplevelConEntry i (StgRhsClosure _cc _bi [] upd_flag _srt args (StgConApp dc cargs))
    | i `elem` dataConImplicitIds dc = genSetConInfo i dc
genToplevelConEntry _ _ = mempty


genToplevelRhs :: Id -> StgRhs -> StaticRefs -> JStat
genToplevelRhs i (StgRhsCon _cc con args) _srs =
    decl (jsIdI i) <>
    genConStatic (jsId i) con (concatMap genArg args) -- no static here?
genToplevelRhs i (StgRhsClosure _cc _bi [] Updatable _srt [] body) srs =
  let f = JFunc funArgs (preamble <> updateThunk <> genBody i [] body Updatable i)
  in  toplevel [j| `decl (jsEnIdI i)`;
                   `jsEnId i` = `f`;
                   `ClosureInfo (jsEnId i) [] (istr (jsIdI i))
                       (CILayoutFixed 2 []) CIThunk (CIStaticRefs srs)`;
                   `decl (jsIdI i)`;
                   `jsId i` = static_thunk(`jsEnId i`);
                 |]
genToplevelRhs i (StgRhsClosure _cc _bi [] upd_flag _srt args body) srs = -- genBody i body
  let f = JFunc funArgs (preamble <> genBody i args body upd_flag i)
  in  toplevel [j| `decl (jsEnIdI i)`;
                   `jsEnId i` = `f`;
                   `ClosureInfo (jsEnId i) (genArgInfo False $ map idType args) (istr (jsIdI i))
                       (CILayoutFixed 1 []) (genEntryType args) (CIStaticRefs srs)`;
                   `decl (jsIdI i)`;
                   `jsId i` = static_fun(`jsEnId i`);
                 |]


-- genRhs _ _ = panic "genRhs"

updateThunk :: JStat
updateThunk =
  [j| `Heap`[`R1`] = blackhole;
      `push [toJExpr R1, jsv "stg_upd_frame"]`;
    |]

loadLiveFun l = -- mempty -- mconcat $ zipWith (loadFunVar currentClosure) (map jsId l) [0..]
   mconcat $ zipWith loadLiveVar [(1::Int)..] l
     where
        loadLiveVar n v = decl' (jsIdI v) [je| `Heap`[`R1`+`n`] |]

genBody :: Id -> [Id] -> StgExpr -> UpdateFlag -> Id -> JStat
genBody topid args e upd i = loadArgs args <> b0
    where
      b0      = genExpr topid e
      resultV = iex (StrI "error_unused_variable") -- fixme do we need this for constructors?

loadArgs :: [Id] -> JStat
loadArgs args = mconcat $ zipWith loadArg args' (enumFrom R2)
   where
     loadArg a reg = decl a <> [j| `iex a` = `reg`; |]
     args' = concatMap genIdArgI args


-- generate code for expression, assign result to r
genExpr :: Id -> StgExpr -> JStat
genExpr top (StgApp f args)      = genApp False True f args
genExpr top (StgLit l)           = [j| `R1` = `genSingleLit l`;
                                       return `Stack`[`Sp`];
                                     |] -- fixme, multi-var lits
genExpr top (StgConApp con args) = genCon con (concatMap genArg args)
genExpr top (StgOpApp (StgFCallOp f g) args t) = panic "ffiop"
genExpr top (StgOpApp (StgPrimOp op) args t) = [j| var res;
                                                   `genPrimOp [res] op args t`;
                                                   `R1` = res;
                                                   return `Stack`[`Sp`];
                                                 |]
genExpr top (StgOpApp (StgPrimCallOp c) args t) = panic "primcall"
genExpr top (StgLam{}) = panic "StgLam"
genExpr top (StgCase e _live1 liveRhs b s at alts) = genCase top b e at alts liveRhs
genExpr top (StgLet b e) = genBind top b <> genExpr top e
genExpr top (StgLetNoEscape l1 l2 b e) = panic "stgletne" -- genLet b e -- fixme what to do with live?
genExpr top (StgSCC cc b1 b2 e) = genExpr top e
genExpr top (StgTick m n e) = genExpr top e

getId :: JExpr -> Ident
getId (ValExpr (JVar i)) = i
getId _                  = panic "getId: expression is not a variable"

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

-- fixme if static thing is a thunk it's never a con, check can go!
-- fixme: if we know some id has already been pattern matched, just return the stack, no check needed?
genApp :: Bool -> Bool -> Id -> [StgArg] -> JStat
genApp force mstackTop i a
    | isPrimitiveType (idType i)
          = [j| `R1` = `jsId i`; return `Stack`[`Sp`]; |] -- fixme: isPrimitiveType can have types bigger than r1
    | isStrictType (idType i)
          = r1 <> [j| return `Stack`[`Sp`]; |]
    | n == 0 && isBoolTy (idType i) -- simple bool tagging: remove one indirection
          = r1 <> [j| if(`R1` < 2) { return `Stack`[`Sp`]; } else { return `Heap`[`R1`]; } |]
    | idArity i == 0 && n == 0 && not (might_be_a_function (idType i)) && not (isLocalId i) -- (not hasFree || not (isLocalId i))
          =  [j| var x = `Heap`[`jsId i`];
                 var t = x.t;
                 if(t === `Thunk`) {
                   return x;
                 }
                 if(t === `Con`) {
                   `R1` = `jsId i`;
                   return `stackTop`; // stack[sp];
                 }
                 `R1` = `Heap`[`jsId i`+1];  // must be an ind
                 return `stackTop`; // stack[sp];
                |]
    | idArity i == 0 && n == 0 && not (might_be_a_function (idType i))
          = {- [j| var x = heap[`jsId i`];
                var t = x.t;
                switch(t) {
                  case `Con`: `R1` = `jsId i`; return stack[sp];
                  case `Ind`: `R1` = heap[`jsId i`+1]; return stack[sp];
                  default: `R1` = `jsId i`; return x;
                }
             |] -}
             [j| var x = `Heap`[`jsId i`];
                 var t = x.t;
                 if(t === `Thunk`) {
                   `R1` = `jsId i`;
                   return x;
                 }
                 if(t === `Con`) {
                   `R1` = `jsId i`;
                   return `stackTop`;
                 }
                 `R1` = `Heap`[`jsId i`+1];  // must be an ind
                 return `stackTop`;
                |]
    | n == 0         = r1 <> jumpTo' (jsVar "stg_ap_0_fast") (concatMap genArg a)
    | idArity i == n = r1 <> jumpTo (jsEntryId i) (concatMap genArg a)
    | idArity i <  n && idArity i > 0 = let (reg,over) = splitAt (idArity i) a
                       in  pushCont over <> jumpToI (jsId i) (concatMap genArg reg) -- fixme specialized cont
    | otherwise      = r1 <> jumpToFast a
  where
    stackTop = [je| `Stack`[`Sp`] |] -- fixme, use known val? fromMaybe [je| stack[sp]; |] mstackTop
    r1 = [j| `R1` = `jsId i`; |]
--    fr1 = if hasFree then r1 else mempty
    ji = jsId i
    n = length a
    b = map genArg a

pushCont :: [StgArg] -> JStat
pushCont as = push $ concatMap genArg as ++ [ app (length as) ]
  where
    app 1 = jsVar "stg_ap_1"
    app 2 = jsVar "stg_ap_2"
    app 3 = jsVar "stg_ap_3"
    app 4 = jsVar "stg_ap_4"
    app _ = [je| unhandled_generic_app() |] -- error "unhandled generic app"

-- jem xs = [j| result = `xs`; |]

genBind :: Id -> StgBinding -> JStat
genBind top bndr
 | (StgNonRec b r) <- bndr = assign b r <> allocCls [(b,r)]
 | (StgRec bs)     <- bndr = mconcat (map (uncurry assign) bs) <> allocCls bs
 where
   assign :: Id -> StgRhs -> JStat
   assign b r = genEntry top b r -- [j| `jsId b` = `` |]

-- generate the entry function for a local closure
genEntry :: Id -> Id -> StgRhs -> JStat
genEntry top i (StgRhsCon _cc con args) = mempty -- panic "local data entry" -- mempty ??
genEntry top i (StgRhsClosure _cc _bi live Updatable _str [] (StgApp fun args)) =
{-  let (apfun, pushfun)
          | length args == 0 = (jsv "stg_ap_0_fast", [])
          | otherwise        = let ap = jsv ("stg_ap_" ++ show (length args))
                               in (ap,[ap]) -}
  let f = JFunc funArgs $ preamble
                   <> loadLiveFun live
                   <> genUpdFrame Updatable
                   <> genApp False True fun args
{-
                   <> push (concatMap genArg as ++ pushfun)
                   <> [j| r1 = `head (genArg a1)`; return `apfun`(); |]
-}
-- fixme args correct here?
  in  toplevel [j| `decl (jsEntryIdI i)`;
                   `jsEntryId i` = `f`;
                    `ClosureInfo (jsEntryId i) (genArgInfo True []) (showPpr' i)
                         (fixedLayout $ map (typeVt.idType) live) (genEntryType []) CINoStatic`;
                |]
--                    `setObjInfo (jsEntryId i) $ genClosureInfo (showPpr' i) live [] <> "gai" .= genArgInfo True []`;
genEntry top i (StgRhsClosure _cc _bi live upd_flag _srt args body) =
  let f = JFunc funArgs (preamble <> loadLiveFun live <> genUpdFrame upd_flag <> genBody top args body upd_flag i)
  in  toplevel [j| `decl (jsEntryIdI i)`;
                   `jsEntryId i` = `f`;
                   `ClosureInfo (jsEntryId i) (genArgInfo True $ map idType args) (showPpr' i)
                          (fixedLayout $ map (typeVt.idType) live) (genEntryType args) CINoStatic`;
                |]

genEntryType :: [Id] -> CIType
genEntryType []   = CIThunk
genEntryType args = CIFun (length $ concat args') nvoid
  where
     args' = map genIdArg args
     nvoid = length $ takeWhile null (reverse args')
-- genClosureLayout :: [Id] -> [Id] -> (CI

--                   `setObjInfo (jsEntryId i) $ genClosureInfo (showPpr' i) live args <> "gai" .= genArgInfo True (map idType args)`;

{-
genClosureInfo :: String -> [Id] -> [Id] -> JObj
genClosureInfo name live [] =
     "i" .= genFunInfo name live <>
     genGcInfo (map idType live) <>
     "t" .= Thunk <>
     "a" .= ji 0
genClosureInfo name live args =
     "i" .= genFunInfo name live <>
     genGcInfo (map idType live) <>
     "t" .= Fun <>
     "a" .= genArityTagId args
-}

-- arity & 0xff = number of arguments
-- arity >> 8   = number of trailing void arguments
{-
genArityTag :: [StgArg] -> CIType
genArityTag args = length (concat args') + (nvoid `shiftL` 8)
    where
      args' = map genArg args
      nvoid = length $ takeWhile null (reverse args')

genArityTagId :: [Id] -> CIType
genArityTagId args = length (concat args') + (nvoid `shiftL` 8)
    where
      args' = map genIdArg args
      nvoid = length $ takeWhile null (reverse args')
-}
genSetConInfo :: Id -> DataCon -> JStat
genSetConInfo i d = [j| `decl (jsDcEntryIdI i)`;
                        `ei`     = `mkDataEntry`;
                        `ClosureInfo ei [R1] (showPpr' d) (fixedLayout $ map typeVt fields)
                            (CICon $ dataConTag d) CINoStatic`;
                      |]
    where
      ei     = jsDcEntryId i
      fields = trd4 (dataConSig d)
{-
           info = "i"   .= genConInfo d <>
             genGcInfo (trd4 (dataConSig d)) <>
             "t"   .= Con <>
             "a"   .= dataConTag d <>
             "gai" .= [ji 1];
-}
-- info table for the arguments that are heap pointers when this function is to be calledb
-- cl == True means that the current closure in r1 is a heap object
genArgInfo :: Bool -> [Type] -> [StgReg]
genArgInfo cl args = r1 <> map numReg (tbl 2 (map typeVt args))
    where
      r1 = if cl then [R1] else []
      tbl n [] = []
      tbl n (t:ts)
          | isPtr t   = n : tbl (n + varSize t) ts
          | otherwise = tbl (n + varSize t) ts

mkDataEntry :: JExpr
mkDataEntry = ValExpr $ JFunc funArgs [j| `preamble`; return `Stack`[`Sp`]; |]

-- generate the info table:
-- [size, descr, a0t, a1t, a2t]
{-
genConInfo :: DataCon -> JExpr
genConInfo c = ValExpr . JList $ [s, d] ++ map (toJExpr . typeVt) as
  where
    as = trd4 (dataConSig c)
    s  = toJExpr (argSize as + 1)
    d  = toJExpr (showPpr' c)
-}
trd4 (_,_,x,_) = x

genFunInfo :: String -> [Id] -> JExpr
genFunInfo name as = ValExpr . JList $ [s, jstr name] ++ map (toJExpr . typeVt . idType) as
  where
    s = toJExpr (argSize (map idType as) + 1)

-- fixme need first var for r1 = closure?
genGcInfo :: [Type] -> JObj
genGcInfo ts = gcInfo size (ptrOffsets 0 (map typeVt ts))
    where
      size = argSize ts + 1


conEntry :: DataCon -> Ident
conEntry = StrI . (\x -> "$hs_" ++ x ++ "_e") . zEncodeString . showPpr'

argSize :: [Type] -> Int
argSize = sum . map (varSize . typeVt)

genUpdFrame :: UpdateFlag -> JStat
genUpdFrame Updatable = updateThunk -- push [toJExpr R1,[je|stg_upd_frame|]]
genUpdFrame _         = mempty

-- allocate local closures
allocCls :: [(Id, StgRhs)] -> JStat
allocCls xs = mconcat stat <> allocDynAll True dyn
  where
    (stat, dyn) = splitEithers (map toCl xs)
    -- left = static, right = dynamic
    toCl :: (Id, StgRhs) -> Either JStat (Ident,JExpr,[JExpr])
    toCl (i, StgRhsCon _cc con []) = (Left $ allocCon (jsIdI i) con [])
    toCl (i, StgRhsCon _cc con ar) = (Right ((jsIdI i) :: Ident, enterDataCon con, concatMap genArg ar))
--    toCl (i, StgRhsClosure _cc _bi live Updatable _srt _args _body) =
--        Right (jsIdI i, updateEntry live, map jsId live)
    toCl (i, StgRhsClosure _cc _bi live upd_fag _srt _args _body) =
        Right (jsIdI i, jsEntryId i, map jsId live)

-- bind result of case to bnd, final result to r
genCase :: Id -> Id -> StgExpr -> AltType -> [StgAlt] -> StgLiveVars -> JStat
genCase top bnd (StgApp i []) at@(PrimAlt tc) alts l = decl (jsIdI bnd) <> [j| `jsId bnd` = `jsId i`; |] <> genInlinePrimCase top bnd (tyConVt tc) at alts

-- genCase r bnd (StgApp i []) at alts l = genForce r bnd (jsId i) at alts l
-- genCase bnd (StgApp i xs) (PrimAlt tc) alts l = decl (jsIdI bnd) <> [j| `jsId bnd` = `jsId i`; |] <> 

genCase top bnd (StgApp i xs) at alts l =
  genRet top bnd at alts l {- <> push [jsId bnd] -} <> genApp True False i xs -- genApp (jsId bnd) i xs

genCase top bnd (StgOpApp (StgPrimOp p) args t) at alts l =
    case genPrim p [jsId bnd] (concatMap genArg args) of
      PrimInline s -> decl (jsIdI bnd) <> s <> genInlinePrimCase top bnd (typeVt t) at alts
      PRPrimCall s -> [j| primcall_not_handled(); |]

genCase _ _ x _ _ _ = [j| unhandled_gencase_format = `show x` |]


-- simple inline prim case, no return function needed
genInlinePrimCase :: Id -> Id -> VarType -> AltType -> [StgAlt] -> JStat
genInlinePrimCase top bnd tc _ [(DEFAULT, bs, used, e)] = genExpr top e
genInlinePrimCase top bnd tc (AlgAlt dtc) alts
    | isBoolTy (mkTyConTy dtc) = mkSwitch [je| `jsId bnd` |] (map (mkPrimBranch top tc) alts)
    | otherwise                = mkSwitch [je| `Heap`[`jsId bnd`].a |] (map (mkPrimBranch top tc) alts)
genInlinePrimCase top bnd tc (PrimAlt ptc) alts
    | isMatchable tc    = mkSwitch (jsId bnd) (map (mkPrimBranch top tc) alts)
    | otherwise         = mkIfElse [jsId bnd] (map (mkPrimIfBranch top tc) alts)
genInlinePrimCase _ _ _ _ alt = [j| unhandled_primcase_format = `show alt`; |]


genForce :: Id -> Id -> JExpr -> AltType -> [StgAlt] -> StgLiveVars -> JStat
genForce top bnd e at alts l = identBoth
  (\i -> toplevel [j| `decl i`; `genRet top bnd at alts l`; |])
  (\i -> [j| `R1` = `e`;
             var c = _heap[`R1`];
             `push [iex i]`;
             if(c.t === `Thunk`) {
               return c;
             } else {
               return `iex i`;
             }
          |]
  )


needForce :: AltType -> Bool
needForce PolyAlt  = True
needForce AlgAlt{} = True
needForce _        = False

genRet :: Id -> Id -> AltType -> [StgAlt] -> StgLiveVars -> JStat
genRet top e at as l = withIdent $ \ret -> pushRetArgs free (iex ret) <> f ret
  where
    f r    = toplevel (decl r)
           <> toplevel [j| `iex r` = `fun`;
                           `ClosureInfo (iex r) (genArgInfo isBoxedAlt []) (istr r)
                                  (fixedLayout $ map (typeVt.idType) free) (CIFun 0 0) (CIStaticParent top)`;
                         |]
    free   = uniqSetToList l


    isBoxedAlt = case at of
                   PrimAlt {} -> False
                   _          -> True
    fun    = ValExpr $ JFunc funArgs [j| `preamble`;
                                         `decl (jsIdI e)`;
                                         `jsId e` = `R1`;
                                         `loadRetArgs free`;
                                         `genAlts top e at as`;
                                       |]


-- push/pop args in reverse order: first arg has lowest offset (corresponds with gc info)
pushRetArgs :: [Id] -> JExpr -> JStat
pushRetArgs free fun = push $ (concatMap genIdArg (reverse free) ++ [fun])

loadRetArgs :: [Id] -> JStat
loadRetArgs free = popSkipI 1 ids
    where
       ids = concatMap genIdArgI (reverse free)

genAlts :: Id -> Id -> AltType -> [StgAlt] -> JStat
genAlts top e PolyAlt [(_, _, _, expr)] = panic "polyalt"
genAlts top e (PrimAlt tc) [(_, bs, use, expr)] = loadParams (jsId e) bs use <> genExpr top expr
-- fixme: for 2-value arguments use more regs
genAlts top e (PrimAlt tc) alts = mkSwitch [je| `Heap`[`R1`] |] (map (mkPrimBranch top (tyConVt tc)) alts)
-- genAlts r e (PrimAlt tc) alts = mkSwitch [je| heap[r1] |] (map (mkPrimBranch r e) alts)
genAlts top e (UbxTupAlt n) [(_, bs, use, expr)] = loadUbxTup bs n <> genExpr top expr
--genAlts r e (AlgAlt tc) [alt] = mkSwitch [snd (mkAlgBranch r e alt)
genAlts top e (AlgAlt tc) [alt] = snd (mkAlgBranch top e alt)
genAlts top e (AlgAlt tc) alts
  | isBoolTy (idType e) = mkSwitch [je| `jsId e` |] (map (mkAlgBranch top e) alts)
  | otherwise           = mkSwitch [je| `Heap`[`(jsId e)`].a |] (map (mkAlgBranch top e) alts)
genAlts top e a l = panic $ "unhandled case variant: " ++ showPpr' a ++ " (" ++ show (length l) ++ ")"

-- fixme remove heap alloc? ubx tup in regs
loadUbxTup :: [Id] -> Int -> JStat
loadUbxTup bs n = mconcat $ zipWith loadArg bs [(1::Int)..]
    where
      loadArg b n = decl (jsIdI b) <> [j| `jsId b` = `Heap`[`R1`+`n`]; |]
{-
mkSwitch :: JExpr -> [(JExpr,JStat)] -> JStat
mkSwitch e cases = SwitchStat e (map addBreak (init cases)) lastc
    where
      addBreak (c,s) = (c, [j| `s`; break; |])
      lastc = snd (last cases)
-}

-- switch for pattern matching on constructors or prims
mkSwitch :: JExpr -> [(Maybe JExpr, JStat)] -> JStat
mkSwitch e cases
    | [(Just c1,s1)] <- n, [(_,s2)] <- d = checkIsCon e <> IfStat [je| `e` === `c1` |] s1 s2
    | [(Just c1,s1),(_,s2)] <- n, null d = checkIsCon e <> IfStat [je| `e` === `c1` |] s1 s2
    | null d        = checkIsCon e <> SwitchStat e (map addBreak (init n)) (snd $ last n)
    | [(_,d0)] <- d = checkIsCon e <> SwitchStat e (map addBreak n) d0
    | otherwise     = panic "multiple default cases"
    where
      addBreak (Just c,s) = (c, s) -- [j| `s`; break |]) -- fixme: rename, does not add break anymore
      addBreak _          = panic "mkSwitch"
      (n,d) = partition (isJust.fst) cases

checkIsCon :: JExpr -> JStat
checkIsCon e = assertRts [je| `e` !== undefined |] "unexpected undefined, expected datacon or prim"

-- mempty -- traceErrorCond [je| `e` === undefined |] "expected data constructor!"


-- if/else for pattern matching on things that js cannot switch on
mkIfElse :: [JExpr] -> [(Maybe [JExpr], JStat)] -> JStat
mkIfElse e s = go (reverse $ sort s)
    where
      go [(_, s)] = s -- only one 'nothing' allowed
      go ((Just e0, s):xs) =
          [j| if( `mkEq e e0` ) { `s` } else { `go xs` } |]

mkEq :: [JExpr] -> [JExpr] -> JExpr
mkEq es1 es2
  | length es1 == length es2 = foldl1 and (zipWith eq es1 es2)
  | otherwise                = panic "mkEq: incompatible expressions"
    where
      and e1 e2 = [je| `e1` && `e2`  |]
      eq  e1 e2 = [je| `e1` === `e2` |]


mkAlgBranch :: Id -> Id -> StgAlt -> (Maybe JExpr, JStat)
mkAlgBranch top d (a,bs,use,expr) = (caseCond a, b)
  where
    b = loadParams (jsId d) bs use <> genExpr top expr


{-
    c | DataAlt da <- a = Just [je| `dataConTag da` |]
      | LitAlt  l  <- a = Just (genSingleLit l)
      | DEFAULT    <- a = Nothing -- panic "default branch"
-}

-- single-var prim
{-
mkPrimBranch :: JExpr -> JExpr -> StgAlt -> (Maybe JExpr, JStat)
mkPrimBranch tgt d (a,bs,use,expr) = (caseCond a, genExpr tgt expr)
-}
mkPrimBranch :: Id -> VarType -> StgAlt -> (Maybe JExpr, JStat)
mkPrimBranch top vt (DEFAULT, bs, us, e) = (Nothing, genExpr top e)
mkPrimBranch top vt (cond,    bs, us, e) = (caseCond cond, genExpr top e)

-- possibly multi-var prim
mkPrimIfBranch :: Id -> VarType -> StgAlt -> (Maybe [JExpr], JStat)
mkPrimIfBranch top vt (DEFAULT, bs, us, e) = (Nothing, genExpr top e)
mkPrimIfBranch top vt (cond,    bs, us, e) = (ifCond cond, genExpr top e)

dummyRet = [je| dummyRet |]


ifCond :: AltCon -> Maybe [JExpr]
ifCond (DataAlt da)
    | isBoolTy (mkTyConTy (dataConTyCon da)) = Just [[je| `dataConTag da - 1` |]]
    | otherwise                              = Just [[je| `dataConTag da` |]]
ifCond (LitAlt l)   = Just (genLit l)
ifCond DEFAULT      = Nothing

caseCond :: AltCon -> Maybe JExpr
caseCond (DataAlt da)
    | isBoolTy (mkTyConTy (dataConTyCon da)) = Just [je| `dataConTag da - 1` |]
    | otherwise                              = Just [je| `dataConTag da` |]
caseCond (LitAlt l)   = Just (genSingleLit l)
caseCond DEFAULT      = Nothing

fourth (_,_,_,x) = x

-- load parameters from constructor
loadParams :: JExpr -> [Id] -> [Bool] -> JStat
loadParams from args use = mconcat $ zipWith3 load args use [(0::Int)..]
  where
    load _ False _ = mempty
    load a True  n = loadConVar from (jsIdI a) n

-- load a variable into a local js var after pattern matching on the constructor
loadConVar :: JExpr -> Ident -> Int -> JStat
loadConVar from to n = [j| `decl to`; `iex to` = `Heap`[`from`+`n+1`] |]

genPrimOp :: [JExpr] -> PrimOp -> [StgArg] -> Type -> JStat
genPrimOp rs op args t = case genPrim op rs (concatMap genArg args) of
                               PrimInline s -> s
                               PRPrimCall s -> [j| error_msg = "unsupported primcall" |]

genArg :: StgArg -> [JExpr]
genArg (StgLitArg l) = genLit l -- fixme 64 bit lits are 2 vars
genArg a@(StgVarArg i)
    | isVoid r     = []
    | isMultiVar r = panic "genArg: multi-arg not yet supported"
    | otherwise    = [jsId i]
   where
     r = typeVt . stgArgType $ a

genIdArg :: Id -> [JExpr]
genIdArg i
    | isVoid r     = []
    | isMultiVar r = panic "genIdArg: multi-arg not yet supported"
    | otherwise    = [jsId i]
    where
      r = typeVt . idType $ i

genIdArgI :: Id -> [Ident]
genIdArgI i
    | isVoid r     = []
    | isMultiVar r = panic "genIdArg: multi-arg not yet supported"
    | otherwise    = [jsIdI i]
    where
      r = typeVt . idType $ i


r2d :: Rational -> Double
r2d = realToFrac

genLit :: Literal -> [JExpr]
genLit (MachChar c)      = [ [je| `ord c` |] ]
genLit (MachStr  str)    = [ [je| decodeString(`unpackFS str`) |] ]
genLit MachNullAddr      = [ [je| null |] ] -- is this right?
genLit (MachInt i)       = [ [je| `i` |] ]
genLit (MachInt64 i)     = [ [je| `i` |] , [je| `i` |] ] -- fixme
genLit (MachWord w)      = [ [je| 0 |] ] -- fixme
genLit (MachWord64 w)    = [ [je| 0 |] ] -- fixme
genLit (MachFloat r)     = [ [je| `r2d r` |] ]
genLit (MachDouble r)    = [ [je| `r2d r` |] ]
genLit (MachLabel name size fod) = [ [je| 0 |] ] -- fixme
genLit (LitInteger i id) = [ [je| `i` |] ] -- fixme, convert to bytes and google int

genSingleLit :: Literal -> JExpr
genSingleLit l
    | [lit] <- genLit l = lit
    | otherwise         = panic "genSingleLit: expected single-variable literal"

genCon :: DataCon -> [JExpr] -> JStat
genCon con args 
  | isUnboxedTupleCon con = -- unboxed tuple returned in registers
      mconcat (zipWith (\reg e -> [j| `reg` = `e`; |]) (enumFrom R1) args) <>
      [j| return `Stack`[`Sp`]; |]
  | otherwise = withIdent $ \di ->
      [j| `decl di`;
          `allocCon di con args`;
          `R1` = `iex di`;
           return `Stack`[`Sp`];
        |]

-- entry function of the worker
enterDataCon :: DataCon -> JExpr
enterDataCon d = jsDcEntryId (dataConWorkId d)

allocCon :: Ident -> DataCon -> [JExpr] -> JStat
allocCon to con [] = [j| `JVar to` = `jsId (dataConWorkId con)`; |]
allocCon to con xs = allocDynamic False to (enterDataCon con) xs

nullaryConClosure tag = ValExpr (JVar . StrI $ "data_static_0_" ++ show tag ++ "_c")

-- allocConStatic cl f n = decl' cl [je| static_con(`f`,`n`) |]
allocConStatic :: JExpr -> DataCon -> [JExpr] -> JStat
-- allocConStatic to tag [] = [j| `to` = `nullaryConClosure tag`; |]
allocConStatic to con [] = [j| `to` = static_con0(`enterDataCon con`) |]
allocConStatic to con xs = [j| `to` = alloc_static(`length xs+1`);
                               initStatic.push( \ { init_closure(`to`,`enterDataCon con`, `JList xs`); } );
                             |]

genConStatic :: JExpr -> DataCon -> [JExpr] -> JStat
genConStatic d con args = allocConStatic d con args

-- load arguments and jump to fun
jumpTo :: JExpr -> [JExpr] -> JStat
jumpTo fun args = mconcat ra <> [j| return `fun`; |]
  where
      ra = zipWith (\r e -> [j| `r` = `e` |]) (enumFrom R2) regargs
      regargs = args

jumpToI :: JExpr -> [JExpr] -> JStat
jumpToI funIdx args = mconcat ra <> enter' [je| `Heap`[`funIdx`] |]
  where
      ra = zipWith (\r e -> [j| `r` = `e` |]) (enumFrom R2) args

-- load arguments and jump to fun directly (not going through trampoline)
jumpTo' :: JExpr -> [JExpr] -> JStat
jumpTo' fun args = mconcat ra <> [j| return `fun`(); |]
  where
      ra = zipWith (\r e -> [j| `r` = `e` |]) (enumFrom R2) args

jumpToFast :: [StgArg] -> JStat
jumpToFast as | spec      = mconcat ra <> [j| return `fun`(); |]
              | otherwise = mconcat ra <> [j| return `fun`(`tag`); |]
    where
      ra    = zipWith (\r e -> [j| `r` = `e` |]) (enumFrom R2) regs
      (fun,spec) = selectApply True as
      regs  = concatMap genArg as
      nargs = length as
      -- our tag is the bitmap of registers that contain pointers
      tag  = ptrTag $ ptrOffsets 0 (map argVt as)

-- find a specialized application path if there is one
selectApply :: Bool     -> -- ^ true for fast apply, false for stack apply
               [StgArg] -> -- ^ arguments
              (JExpr,Bool) -- ^ the function to call, true if specialized path
selectApply fast args =
    case find (\(_,n',v',p) -> (n',v',ptrTag p) == (n,nvoid,ptrs)) fixedApply of
      Nothing           -> (jsv $ "stg_ap_" ++ show n ++ vsuff ++ suff, False) -- fixme check overflow for many-ary functions
      Just (n, _, _, _) -> (jsv $ "stg_ap_" ++ n ++ suff, True)
    where
      suff | fast      = "_fast"
           | otherwise = ""
      vsuff | nvoid == 0 = ""
            | otherwise  = '_' : replicate nvoid 'v'
      args' = map genArg args
      n     = length (concat args')
      nvoid = length (takeWhile null $ reverse args')
      ptrs  = ptrTag $ ptrOffsets 0 (map argVt args)

-- jmacro hacks:

-- insert a toplevel statement (by labeling it toplevel, see Floater)
toplevel :: JStat -> JStat
toplevel = LabelStat "toplevel"

-- insert delimiters around block so linker can extract this efficiently
-- abuses PPostStat to insert a comment!
delimitBlock :: [Id] -> JStat -> JStat
delimitBlock i s = PPostStat True start emptye <> s <> PPostStat True end emptye
  where
    idStr i = let (StrI xs) = jsIdI i in xs
    block  = L.intercalate "," (map idStr i)
    emptye = iex (StrI "")
    start = T.unpack Linker.startMarker ++ block ++ ">"
    end   = T.unpack Linker.endMarker ++ block ++ ">"