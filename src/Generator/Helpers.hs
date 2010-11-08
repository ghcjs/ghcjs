{-# LANGUAGE TypeFamilies #-}
module Generator.Helpers where

import Id as Stg (Id)
import Name (NamedThing (getName, getOccName), nameModule, isExternalName)
import OccName (occNameString)
import Unique (Uniquable (getUnique), getKey)
import FastString (unpackFS)
import Panic (panic)
import Encoding (zEncodeString)

import Module as Stg (Module, moduleName, moduleNameString)
import StgSyn as Stg
import qualified Literal as Stg
import Javascript.Language as Js
import qualified RTS.Objects as RTS

-- | Convert Haskell literal to Javascript object
-- this function should really go into PrimOp module...
stgLiteralToJs :: Javascript js => Stg.Literal -> Expression js
stgLiteralToJs (Stg.MachChar c) = Js.string [c] -- Char#
stgLiteralToJs (Stg.MachStr s) = Js.string (unpackFS s ++ "\0") -- Addr#
stgLiteralToJs (Stg.MachInt i) = Js.int i -- Int#
stgLiteralToJs (Stg.MachInt64 _) = Js.nativeMethodCall RTS.root "alert" [Js.string "Unsupported literal: Int64"]
stgLiteralToJs (Stg.MachWord i) -- Word#
  | i > 2^(31 :: Int) - 1 = Js.int (i - 2^(32 :: Int) + 1)
  | otherwise = Js.int i
stgLiteralToJs (Stg.MachWord64 _) = Js.nativeMethodCall RTS.root "alert" [Js.string "Unsupported literal: Int64"]
stgLiteralToJs (Stg.MachFloat i) = Js.float i -- Float#
stgLiteralToJs (Stg.MachDouble i) = Js.float i -- Doable#
stgLiteralToJs (Stg.MachNullAddr) = Js.null -- Addr#
stgLiteralToJs (Stg.MachLabel {}) = Js.nativeMethodCall RTS.root "alert" [Js.string "Unsupported literal: MachLabel"]

stgArgToJs :: Javascript js => Stg.StgArg -> Expression js
stgArgToJs (Stg.StgVarArg id) = stgIdToJs id
stgArgToJs (Stg.StgLitArg l) = stgLiteralToJs l
stgArgToJs (Stg.StgTypeArg _) = panic "Compiler bug: StgTypeArg in expression"

jsBoolToHs :: Javascript js => Expression js -> Expression js
jsBoolToHs ex = Js.ternary ex RTS.true RTS.false

moduleName :: Module -> String
moduleName = moduleNameString . Stg.moduleName

isExternalId :: Stg.Id -> Bool
isExternalId = isExternalName . getName

stgModuleToJs :: Javascript js => Module -> Expression js
stgModuleToJs mod = RTS.modules # (zEncodeString . moduleNameString . Stg.moduleName $ mod)
  where (#) = Js.property

stgIdToJsProperyName :: Stg.Id -> String
stgIdToJsProperyName = ("hs_"++) . zEncodeString . occNameString . getOccName

stgIdToJs :: Javascript js => Stg.Id -> Expression js
stgIdToJs id
  | isExternalId id = Js.property (stgModuleToJs . nameModule . getName $ id) (stgIdToJsProperyName id)
  | otherwise = Js.var . stgIdToJsId $ id

stgIdToJsId :: Stg.Id -> Js.Id
stgIdToJsId id = name ++ key
  where name = ("hs_"++) . zEncodeString . occNameString . getOccName $ id
        key = intToBase62 . getKey . getUnique $ id

stgBindingToList :: StgBinding -> [(Stg.Id, StgRhs)]
stgBindingToList (StgNonRec id rhs) = [(id, rhs)]
stgBindingToList (StgRec bs) = bs

stgArgsToJs :: Javascript js => [Stg.StgArg] -> Expression js
stgArgsToJs = Js.list . map stgArgToJs

intToBase62 :: Int -> String
intToBase62 n = go n ""
  where go n cs
          | n == 0 = cs
	  | otherwise = go q (c : cs)
          where (q, r) = quotRem n 62
                c = chooseChar62 r
                chooseChar62 n = chars62 !! n
                chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

