module Generator.Helpers where

import Id as Stg (Id, isExportedId)
import Name (NamedThing (getName, getOccName), nameModule)
import OccName (occNameString)
import Unique (Uniquable (getUnique), getKey)
import FastString (unpackFS)
import Panic (panic)
import Encoding (zEncodeString)

import Module (Module, moduleName, moduleNameString, moduleNameSlashes)
import StgSyn as Stg
import qualified Literal as Stg
import qualified Javascript.Language as Js

haskellRoot :: Js.Expression
haskellRoot = Js.var "$hs"

modulePath :: Module -> String
modulePath = moduleNameSlashes . moduleName

stgModuleToJs :: Module -> Js.Expression
stgModuleToJs mod = haskellRoot $. "modules" $. (zEncodeString . moduleNameString . moduleName $ mod)
  where ($.) = Js.property

stgIdToJs :: Stg.Id -> Js.Expression
stgIdToJs id
  | isExportedId id = Js.property (stgModuleToJs . nameModule . getName $ id) name
  | otherwise = Js.var . stgIdToJsId $ id
  where name = zEncodeString . occNameString . getOccName $ id

stgIdToJsId :: Stg.Id -> Js.Id
stgIdToJsId id = name ++ key
  where name = zEncodeString . occNameString . getOccName $ id
        key = intToBase62 . getKey . getUnique $ id

stgIdToJsDecl :: Stg.Id -> Js.Expression -> Js.Program
stgIdToJsDecl id expr
  | isExportedId id = Js.assign (stgIdToJs id) expr
  | otherwise = Js.declare (stgIdToJsId id) expr

stgIdToJsDeclareMethodCallResult :: Stg.Id -> Js.Expression -> Js.Id -> [Js.Expression] -> Js.Program
stgIdToJsDeclareMethodCallResult id
  | isExportedId id = Js.assignMethodCallResult (stgIdToJs id)
  | otherwise = Js.declareMethodCallResult (stgIdToJsId id)

stgIdToJsDeclareFunctionCallResult :: Stg.Id -> Js.Expression -> [Js.Expression] -> Js.Program
stgIdToJsDeclareFunctionCallResult id
  | isExportedId id = Js.assignFunctionCallResult (stgIdToJs id)
  | otherwise = Js.declareFunctionCallResult (stgIdToJsId id)

stgBindingToList :: StgBinding -> [(Id, StgRhs)]
stgBindingToList (StgNonRec id rhs) = [(id, rhs)]
stgBindingToList (StgRec bs) = bs

stgArgsToJs :: [Stg.StgArg] -> Js.Expression
stgArgsToJs = Js.list . map stgArgToJs

stgArgToJs :: Stg.StgArg -> Js.Expression
stgArgToJs (Stg.StgVarArg id) = stgIdToJs id
stgArgToJs (Stg.StgLitArg l) = stgLiteralToJs l
stgArgToJs (Stg.StgTypeArg _) = panic "Compiler bug: StgTypeArg in expression"

stgLiteralToJs :: Stg.Literal -> Js.Expression
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

