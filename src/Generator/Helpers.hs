{-# LANGUAGE CPP, TypeFamilies #-}
module Generator.Helpers where

import Id as Stg (Id)
import Name (NamedThing (getName, getOccName), nameModule, isExternalName)
import OccName (occNameString)
import FastString (unpackFS)
import Panic (panic)
import Encoding (zEncodeString)

import Module as Stg (Module, moduleName, moduleNameString)
import StgSyn as Stg
import qualified Literal as Stg
import qualified Javascript.Language as Js
import qualified RTS.Objects as RTS
#ifdef MIN_VERSION_monads_tf
import Control.Monad.State (State, runState)
import Control.Monad.State.Class (MonadState(..))
#endif
import Control.Applicative ((<$>))
import qualified Data.Map as M
import qualified Data.Set as S (Set, member, fromList)
import Javascript.Language (Javascript, Expression)
import Data.Bits (Bits(..))

data GenState = GenState {
    idMap     :: M.Map Stg.Id Js.Id
  , unusedIds :: [Js.Id]
  }

#ifdef MIN_VERSION_monads_tf
type Gen = State GenState
runGen :: Gen a -> GenState -> (a, GenState)
runGen = runState
#else
newtype Gen a = Gen { runGen :: GenState -> (a, GenState) }

instance Monad Gen where
    return a = Gen $ \s -> (a, s)
    m >>= k  = Gen $ \s ->
        let ~(a, s') = runGen m s in
        runGen (k a) s'
    fail str = Gen $ \_ -> error str

get :: Gen GenState
get = Gen $ \s -> (s, s)

put :: GenState -> Gen ()
put s = Gen $ \_ -> ((), s)

instance Functor Gen where
    fmap f m = Gen $ \s ->
        (\ ~(a, s') -> (f a, s')) $ runGen m s
#endif

newGenState :: GenState
newGenState = GenState M.empty safeIds

-- | Convert Haskell literal to Javascript object
-- this function should really go into PrimOp module...
stgLiteralToJs :: Javascript js => Stg.Literal -> Gen (Expression js)
stgLiteralToJs (Stg.MachChar c) = return $ Js.string [c] -- Char#
stgLiteralToJs (Stg.MachStr s) = return $ Js.string (unpackFS s ++ "\0") -- Addr#
stgLiteralToJs (Stg.MachInt i) = return $ Js.int i -- Int#
stgLiteralToJs (Stg.MachInt64 i)
    = return $ Js.nativeMethodCall (Js.var "goog.math.Long") "fromBits" [
        Js.int (i .&. 0xFFFFFFFF),
        Js.int (i `shiftR` 32 .&. 0xFFFFFFFF)]
stgLiteralToJs (Stg.MachWord i) -- Word#
  | i >= 2^(31 :: Int) = return $ Js.int (i - 2^(32 :: Int) + 1)
  | otherwise = return $ Js.int i
stgLiteralToJs (Stg.MachWord64 i)
  | i >= 2^(63 :: Int) = do
    let c = i - 2^(64 :: Int) + 1
    return $ Js.nativeMethodCall (Js.var "goog.math.Long") "fromBits" [
        Js.int (c .&. 0xFFFFFFFF),
        Js.int (c `shiftR` 32 .&. 0xFFFFFFFF)]
  | otherwise = return $ Js.nativeMethodCall (Js.var "goog.math.Long") "fromBits" [
        Js.int (i .&. 0xFFFFFFFF),
        Js.int (i `shiftR` 32 .&. 0xFFFFFFFF)]
stgLiteralToJs (Stg.MachFloat i) = return $ Js.float i -- Float#
stgLiteralToJs (Stg.MachDouble i) = return $ Js.float i -- Doable#
stgLiteralToJs (Stg.MachNullAddr) = return $ Js.null -- Addr#
stgLiteralToJs (Stg.MachLabel {}) = return $ Js.nativeMethodCall RTS.root "alert" [Js.string "Unsupported literal: MachLabel"]
stgLiteralToJs _ = error "Unknown literal type"

stgArgToJs :: Javascript js => Stg.StgArg -> Gen (Expression js)
stgArgToJs (Stg.StgVarArg id) = Js.var <$> stgIdToJs id
stgArgToJs (Stg.StgLitArg l) = stgLiteralToJs l
stgArgToJs (Stg.StgTypeArg _) = panic "Compiler bug: StgTypeArg in expression"

jsBoolToHs :: Javascript js => Expression js -> Expression js
jsBoolToHs ex = Js.ternary ex RTS.true RTS.false

isExternalId :: Stg.Id -> Bool
isExternalId = isExternalName . getName

stgIdToJsExternalName :: Stg.Id -> Js.Id
stgIdToJsExternalName id = (("$$"++mod++"_")++) . zEncodeString . occNameString $ getOccName id
  where mod = zEncodeString . moduleNameString . moduleName . nameModule . getName $ id

stgIdToJs :: Stg.Id -> Gen Js.Id
stgIdToJs id
  | isExternalId id = return $ stgIdToJsExternalName id
  | otherwise = stgIdToJsId id

addTopLevelIds :: Module -> [Stg.Id] -> Gen ()
addTopLevelIds mod ids = do
    state <- get
    put state { idMap = (idMap state) `M.union` M.fromList newIds }
  where
    newIds = map newId (zip ids safeIds)
    newId (id, s) = (id, "$$" ++ modName ++ "$" ++ s)
    modName = zEncodeString . moduleNameString . moduleName $ mod

stgIdToJsId :: Stg.Id -> Gen Js.Id
stgIdToJsId id = do
    state <- get
    let m = idMap state
    case (M.lookup id m, unusedIds state) of
        (Just r, _)     -> return r
        (Nothing, x:xs) -> do
            put $ state {idMap = M.insert id x m, unusedIds=xs}
            return x
        (Nothing, [])   -> error "Ran out of ids (should never happen)"

stgBindingToList :: StgBinding -> [(Stg.Id, StgRhs)]
stgBindingToList (StgNonRec id rhs) = [(id, rhs)]
stgBindingToList (StgRec bs) = bs

stgArgsToJs :: Javascript js => [Stg.StgArg] -> Gen (Expression js)
stgArgsToJs args = Js.list <$> mapM stgArgToJs args

reservedWords :: S.Set String
reservedWords = S.fromList ["abstract","assert","boolean","break","byte","case","catch","char","class","const","continue","debugger","decimal","default","delete","do","double","else","ensure","enum","event","export","extends","false","final","finally","float","for","function","get","goto","if","implements","import","in","instanceof","int","interface","internal","invariant","let","long","namespace","native","new","null","package","private","protected","public","require","return","sbyte","set","short","static","super","switch","synchronized","this","throw","throws","transient","true","try","typeof","uint","ulong","use","ushort","var","void","volatile","while","with","yield"]

safeIds :: [String]
safeIds = filter (not . flip S.member reservedWords) (map intToIdNoInitialDigit [0..])

intToIdNoInitialDigit :: Int -> String
intToIdNoInitialDigit n = c : intToBase62 q
  where (q, r) = quotRem n 52
        c = chooseChar52 r
        chooseChar52 n = chars52 !! n
        chars52 = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

intToBase62 :: Int -> String
intToBase62 n = go n ""
  where go n cs
          | n == 0 = cs
          | otherwise = go q (c : cs)
          where (q, r) = quotRem n 62
                c = chooseChar62 r
                chooseChar62 n = chars62 !! n
                chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

