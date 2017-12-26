{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
{-
  A base bundle is used for incremental linking. it contains information about
  the symbols that have already been linked. These symbols are not included
  again in the incrementally linked program.

  The base contains a CompactorState for consistent renaming of private names
  and packed initialization of info tables and static closures.
-}
module Gen2.Base where

import qualified Gen2.Object          as Object

import           Compiler.JMacro

import           Control.Lens
import           Control.Monad

import           Data.Array
import qualified Data.Binary          as DB
import qualified Data.Binary.Get      as DB
import qualified Data.Binary.Put      as DB
import           Data.HashMap.Strict  (HashMap)
import qualified Data.HashMap.Strict  as HM
import qualified Data.HashSet         as HS
import qualified Data.Map             as M
import           Data.Monoid
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T

newLocals :: [Ident]
newLocals = filter (not . isKeyword) $ map (TxtI . T.pack) $ (map (:[]) chars0) ++ concatMap mkIdents [1..]
  where
    mkIdents n = [c0:cs | c0 <- chars0, cs <- replicateM n chars]
    chars0 = ['a'..'z']++['A'..'Z']
    chars = chars0++['0'..'9']
    isKeyword (TxtI i) = i `HS.member` kwSet
    kwSet = HS.fromList keywords
    keywords = [ "break", "case", "catch", "continue", "debugger"
               , "default", "delete", "do", "else", "finally", "for"
               , "function", "if", "in", "instanceof", "new", "return"
               , "switch", "this", "throw", "try", "typeof", "var", "void"
               , "while", "with"
               , "class", "enum", "export", "extends", "import", "super", "const"
               , "implements", "interface", "let", "package", "private", "protected"
               , "public", "static", "yield"
               , "null", "true", "false"
               ]

renamedVars :: [Ident]
renamedVars = map (\(TxtI xs) -> TxtI ("h$$"<>xs)) newLocals

data CompactorState =
  CompactorState { _identSupply   :: [Ident]               -- ^ ident supply for new names
                 , _nameMap       :: !(HashMap Text Ident) -- ^ renaming mapping for internal names
                 , _entries       :: !(HashMap Text Int)   -- ^ entry functions (these get listed in the metadata init array)
                 , _numEntries    :: !Int
                 , _statics       :: !(HashMap Text Int)   -- ^ mapping of global closure -> index in current block, for static initialisation
                 , _numStatics    :: !Int                  -- ^ number of static entries
                 , _labels        :: !(HashMap Text Int)   -- ^ non-Haskell JS labels
                 , _numLabels     :: !Int                  -- ^ number of labels
                 , _parentEntries :: !(HashMap Text Int)   -- ^ entry functions we're not linking, offset where parent gets [0..n], grantparent [n+1..k] etc
                 , _parentStatics :: !(HashMap Text Int)   -- ^ objects we're not linking in base bundle
                 , _parentLabels  :: !(HashMap Text Int)   -- ^ non-Haskell JS labels in parent
                 } deriving (Show)

makeLenses ''CompactorState

emptyCompactorState :: CompactorState
emptyCompactorState = CompactorState renamedVars HM.empty HM.empty 0 HM.empty 0 HM.empty 0 HM.empty HM.empty HM.empty

showBase :: Base -> String
showBase b = unlines
  [ "Base:"
  , "  packages: " ++ show (basePkgs b)
  , "  number of units: " ++ show (S.size $ baseUnits b)
  , "  renaming table size: " ++
    show (baseCompactorState b ^. nameMap . to HM.size)
  ]

data Base = Base { baseCompactorState :: CompactorState
                 , basePkgs           :: [Object.Package]
                 , baseUnits          :: Set (Object.Package, Text, Int)
                 }

emptyBase :: Base
emptyBase = Base emptyCompactorState [] S.empty

putBase :: Base -> DB.Put
putBase (Base cs packages funs) = do
  DB.putByteString "GHCJSBASE"
  DB.putLazyByteString Object.versionTag
  putCs cs
  putList DB.put packages
  putList putPkg pkgs
  putList DB.put mods
  putList putFun (S.toList funs)
  where
    pi :: Int -> DB.Put
    pi = DB.putWord32le . fromIntegral
    uniq :: Ord a => [a] -> [a]
    uniq  = S.toList . S.fromList
    pkgs  = uniq (map (\(x,_,_) -> x) $ S.toList funs)
    pkgsM = M.fromList (zip pkgs [(0::Int)..])
    mods  = uniq (map (\(_,x,_) -> x) $ S.toList funs)
    modsM = M.fromList (zip mods [(0::Int)..])
    putList f xs = pi (length xs) >> mapM_ f xs
    -- serialise the compactor state
    putCs (CompactorState [] _ _ _ _ _ _ _ _ _ _) = error "putBase: putCs exhausted renamer symbol names"
    putCs (CompactorState (ns:_) nm es _ ss _ ls _ pes pss pls) = do
      DB.put ns
      DB.put (HM.toList nm)
      DB.put (HM.toList es)
      DB.put (HM.toList ss)
      DB.put (HM.toList ls)
      DB.put (HM.toList pes)
      DB.put (HM.toList pss)
      DB.put (HM.toList pls)
    putPkg (Object.Package k) = DB.put k
    -- fixme group things first
    putFun (p,m,s) = pi (pkgsM M.! p) >> pi (modsM M.! m) >> DB.put s

getBase :: FilePath -> DB.Get Base
getBase file = getBase'
  where
    gi :: DB.Get Int
    gi = fromIntegral <$> DB.getWord32le
    getList f = DB.getWord32le >>= \n -> replicateM (fromIntegral n) f
    getFun ps ms = (,,) <$> ((ps!) <$> gi) <*> ((ms!) <$> gi) <*> DB.get
    la xs = listArray (0, length xs - 1) xs
    getPkg = Object.Package <$> DB.get
    getCs = do
      n   <- DB.get
      nm  <- HM.fromList <$> DB.get
      es  <- HM.fromList <$> DB.get
      ss  <- HM.fromList <$> DB.get
      ls  <- HM.fromList <$> DB.get
      pes <- HM.fromList <$> DB.get
      pss <- HM.fromList <$> DB.get
      pls <- HM.fromList <$> DB.get
      return (CompactorState (dropWhile (/=n) renamedVars) nm es (HM.size es) ss (HM.size ss) ls (HM.size ls) pes pss pls)
    getBase' = do
      hdr <- DB.getByteString 9
      when (hdr /= "GHCJSBASE") (error $ "getBase: invalid base file: " <> file)
      vt  <- DB.getLazyByteString (fromIntegral Object.versionTagLength)
      when (vt /= Object.versionTag) (error $ "getBase: incorrect version: " <> file)
      cs <- makeCompactorParent <$> getCs
      linkedPackages <- getList DB.get
      pkgs <- la <$> getList getPkg
      mods <- la <$> getList DB.get
      funs <- getList (getFun pkgs mods)
      return (Base cs linkedPackages $ S.fromList funs)

-- | make a base state from a CompactorState: empty the current symbols sets, move everything to
--   the parent
makeCompactorParent :: CompactorState -> CompactorState
makeCompactorParent (CompactorState is nm es nes ss nss ls nls pes pss pls) =
  CompactorState is nm HM.empty 0 HM.empty 0 HM.empty 0
     (HM.union (fmap (+nes) pes) es)
     (HM.union (fmap (+nss) pss) ss)
     (HM.union (fmap (+nls) pls) ls)

instance DB.Binary Base where
  get = getBase "<unknown file>"
  put = putBase
