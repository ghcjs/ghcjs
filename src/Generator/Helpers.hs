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

haskellRoot :: Javascript js => Expression js
haskellRoot = Js.var "$hs"

moduleName :: Module -> String
moduleName = moduleNameString . Stg.moduleName

isExternalId :: Stg.Id -> Bool
isExternalId = isExternalName . getName

stgModuleToJs :: Javascript js => Module -> Expression js
stgModuleToJs mod = haskellRoot $. "modules" $. (zEncodeString . moduleNameString . Stg.moduleName $ mod)
  where ($.) = Js.property

stgIdToJs :: Javascript js => Stg.Id -> Expression js
stgIdToJs id
  | isExternalId id = Js.property (stgModuleToJs . nameModule . getName $ id) nameStr
  | otherwise = Js.var . stgIdToJsId $ id
  where nameStr = ("hs_"++) . zEncodeString . occNameString . getOccName $ id

stgIdToJsId :: Stg.Id -> Js.Id
stgIdToJsId id = name ++ key
  where name = ("hs_"++) . zEncodeString . occNameString . getOccName $ id
        key = intToBase62 . getKey . getUnique $ id

stgIdToJsDecl :: Javascript js => Stg.Id -> Expression js -> js
stgIdToJsDecl id expr
  | isExternalId id = Js.assign (stgIdToJs id) expr
  | otherwise = Js.declare (stgIdToJsId id) expr

stgIdToJsDeclareMethodCallResult :: Javascript js => Stg.Id -> Expression js -> Js.Id -> [Expression js] -> js
stgIdToJsDeclareMethodCallResult id
  | isExternalId id = Js.assignMethodCallResult (stgIdToJs id)
  | otherwise = Js.declareMethodCallResult (stgIdToJsId id)

stgIdToJsDeclareFunctionCallResult :: Javascript js => Stg.Id -> Expression js -> [Expression js] -> js
stgIdToJsDeclareFunctionCallResult id
  | isExternalId id = Js.assignFunctionCallResult (stgIdToJs id)
  | otherwise = Js.declareFunctionCallResult (stgIdToJsId id)

stgBindingToList :: StgBinding -> [(Stg.Id, StgRhs)]
stgBindingToList (StgNonRec id rhs) = [(id, rhs)]
stgBindingToList (StgRec bs) = bs

stgArgsToJs :: Javascript js => [Stg.StgArg] -> Expression js
stgArgsToJs = Js.list . map stgArgToJs

stgArgToJs :: Javascript js => Stg.StgArg -> Expression js
stgArgToJs (Stg.StgVarArg id) = stgIdToJs id
stgArgToJs (Stg.StgLitArg l) = stgLiteralToJs l
stgArgToJs (Stg.StgTypeArg _) = panic "Compiler bug: StgTypeArg in expression"

stgLiteralToJs :: Javascript js => Stg.Literal -> Expression js
stgLiteralToJs (Stg.MachChar c) = Js.string [c]
stgLiteralToJs (Stg.MachStr s) = Js.string (unpackFS s ++ "\0")
stgLiteralToJs (Stg.MachInt i) = Js.int i
stgLiteralToJs (Stg.MachInt64 i) = Js.int i
stgLiteralToJs (Stg.MachWord i) = Js.int i
stgLiteralToJs (Stg.MachWord64 i) = Js.int i
stgLiteralToJs (Stg.MachFloat i) = Js.float i
stgLiteralToJs (Stg.MachDouble i) = Js.float i
stgLiteralToJs (Stg.MachNullAddr) = Js.null
stgLiteralToJs (Stg.MachLabel {}) = Js.unsafeStringToExpression "$hs.alert ('Unsupported literal: MachLabel')"

intToBase62 :: Int -> String
intToBase62 n = go n ""
  where go n cs
          | n == 0 = cs
	  | otherwise = go q (c : cs)
          where (q, r) = quotRem n 62
                c = chooseChar62 r
                chooseChar62 n = chars62 !! n
                chars62 = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

