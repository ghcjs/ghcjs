{-# LANGUAGE OverloadedStrings
  #-}
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
import Prelude

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
import           Data.Set             (Set)
import qualified Data.Set             as S
import           Data.Text            (Text)
import qualified Data.Text            as T
import           Data.ByteString      (ByteString)

import           Panic

newLocals :: [Ident]
newLocals = filter (not . isKeyword) $
            map (TxtI . T.pack) $
            (map (:[]) chars0) ++ concatMap mkIdents [1..]
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
               , "class", "enum", "export", "extends", "import", "super"
               , "const"
               , "implements", "interface", "let", "package", "private"
               , "protected"
               , "public", "static", "yield"
               , "null", "true", "false"
               ]

renamedVars :: [Ident]
renamedVars = map (\(TxtI xs) -> TxtI ("h$$"<>xs)) newLocals

data CompactorState = CompactorState
  { _identSupply   :: [Ident]               -- ^ ident supply for new names
  , _nameMap       :: !(HashMap Text Ident) -- ^ renaming mapping for internal names
  , _entries       :: !(HashMap Text Int)   -- ^ entry functions (these get listed in the metadata init array)
  , _numEntries    :: !Int
  , _statics       :: !(HashMap Text Int)   -- ^ mapping of global closure -> index in current block, for static initialisation
  , _numStatics    :: !Int                  -- ^ number of static entries
  , _labels        :: !(HashMap Text Int)   -- ^ non-Haskell JS labels
  , _numLabels     :: !Int                  -- ^ number of labels
  , _parentEntries :: !(HashMap Text Int)   -- ^ entry functions we're not linking, offset where parent gets [0..n], grandparent [n+1..k] etc
  , _parentStatics :: !(HashMap Text Int)   -- ^ objects we're not linking in base bundle
  , _parentLabels  :: !(HashMap Text Int)   -- ^ non-Haskell JS labels in parent
  , _stringTable   :: !StringTable
  } deriving (Show)

data StringTable = StringTable
  { stTableIdents :: !(Array Int Text)
  , stOffsets     :: !(HashMap ByteString (Int, Int))  -- ^ content of the table
  , stIdents      :: !(HashMap Text       (Either Int Int))  -- ^ identifiers in the table
  } deriving (Show)

instance DB.Binary StringTable where
  put (StringTable tids offs idents) = do
    DB.put tids
    DB.put (HM.toList offs)
    DB.put (HM.toList idents)
  get = StringTable <$> DB.get
                    <*> fmap HM.fromList DB.get
                    <*> fmap HM.fromList DB.get

emptyStringTable :: StringTable
emptyStringTable = StringTable (listArray (0,-1) [])
                               HM.empty
                               HM.empty
{-
makeLenses ''CompactorState
-}

entries :: Lens' CompactorState (HashMap Text Int)
entries
  f_abikn
  (CompactorState x1_abiko
                  x2_abikp
                  x3_abikq
                  x4_abikr
                  x5_abiks
                  x6_abikt
                  x7_abiku
                  x8_abikv
                  x9_abikw
                  x10_abikx
                  x11_abiky
                  x12_abikz)
  = (fmap
       (\ y1_abikA
          -> (((((((((((CompactorState x1_abiko) x2_abikp) y1_abikA)
                       x4_abikr)
                      x5_abiks)
                     x6_abikt)
                    x7_abiku)
                   x8_abikv)
                  x9_abikw)
                 x10_abikx)
                x11_abiky)
               x12_abikz))
      (f_abikn x3_abikq)
{-# INLINE entries #-}
identSupply :: Lens' CompactorState [Ident]
identSupply
  f_abikB
  (CompactorState x1_abikC
                  x2_abikD
                  x3_abikE
                  x4_abikF
                  x5_abikG
                  x6_abikH
                  x7_abikI
                  x8_abikJ
                  x9_abikK
                  x10_abikL
                  x11_abikM
                  x12_abikN)
  = (fmap
       (\ y1_abikO
          -> (((((((((((CompactorState y1_abikO) x2_abikD) x3_abikE)
                       x4_abikF)
                      x5_abikG)
                     x6_abikH)
                    x7_abikI)
                   x8_abikJ)
                  x9_abikK)
                 x10_abikL)
                x11_abikM)
               x12_abikN))
      (f_abikB x1_abikC)
{-# INLINE identSupply #-}
labels :: Lens' CompactorState (HashMap Text Int)
labels
  f_abikP
  (CompactorState x1_abikQ
                  x2_abikR
                  x3_abikS
                  x4_abikT
                  x5_abikU
                  x6_abikV
                  x7_abikW
                  x8_abikX
                  x9_abikY
                  x10_abikZ
                  x11_abil0
                  x12_abil1)
  = (fmap
       (\ y1_abil2
          -> (((((((((((CompactorState x1_abikQ) x2_abikR) x3_abikS)
                       x4_abikT)
                      x5_abikU)
                     x6_abikV)
                    y1_abil2)
                   x8_abikX)
                  x9_abikY)
                 x10_abikZ)
                x11_abil0)
               x12_abil1))
      (f_abikP x7_abikW)
{-# INLINE labels #-}
nameMap :: Lens' CompactorState (HashMap Text Ident)
nameMap
  f_abil3
  (CompactorState x1_abil4
                  x2_abil5
                  x3_abil6
                  x4_abil7
                  x5_abil8
                  x6_abil9
                  x7_abila
                  x8_abilb
                  x9_abilc
                  x10_abild
                  x11_abile
                  x12_abilf)
  = (fmap
       (\ y1_abilg
          -> (((((((((((CompactorState x1_abil4) y1_abilg) x3_abil6)
                       x4_abil7)
                      x5_abil8)
                     x6_abil9)
                    x7_abila)
                   x8_abilb)
                  x9_abilc)
                 x10_abild)
                x11_abile)
               x12_abilf))
      (f_abil3 x2_abil5)
{-# INLINE nameMap #-}
numEntries :: Lens' CompactorState Int
numEntries
  f_abilh
  (CompactorState x1_abili
                  x2_abilj
                  x3_abilk
                  x4_abill
                  x5_abilm
                  x6_abiln
                  x7_abilo
                  x8_abilp
                  x9_abilq
                  x10_abilr
                  x11_abils
                  x12_abilt)
  = (fmap
       (\ y1_abilu
          -> (((((((((((CompactorState x1_abili) x2_abilj) x3_abilk)
                       y1_abilu)
                      x5_abilm)
                     x6_abiln)
                    x7_abilo)
                   x8_abilp)
                  x9_abilq)
                 x10_abilr)
                x11_abils)
               x12_abilt))
      (f_abilh x4_abill)
{-# INLINE numEntries #-}
numLabels :: Lens' CompactorState Int
numLabels
  f_abilv
  (CompactorState x1_abilw
                  x2_abilx
                  x3_abily
                  x4_abilz
                  x5_abilA
                  x6_abilB
                  x7_abilC
                  x8_abilD
                  x9_abilE
                  x10_abilF
                  x11_abilG
                  x12_abilH)
  = (fmap
       (\ y1_abilI
          -> (((((((((((CompactorState x1_abilw) x2_abilx) x3_abily)
                       x4_abilz)
                      x5_abilA)
                     x6_abilB)
                    x7_abilC)
                   y1_abilI)
                  x9_abilE)
                 x10_abilF)
                x11_abilG)
               x12_abilH))
      (f_abilv x8_abilD)
{-# INLINE numLabels #-}
numStatics :: Lens' CompactorState Int
numStatics
  f_abilJ
  (CompactorState x1_abilK
                  x2_abilL
                  x3_abilM
                  x4_abilN
                  x5_abilO
                  x6_abilP
                  x7_abilQ
                  x8_abilR
                  x9_abilS
                  x10_abilT
                  x11_abilU
                  x12_abilV)
  = (fmap
       (\ y1_abilW
          -> (((((((((((CompactorState x1_abilK) x2_abilL) x3_abilM)
                       x4_abilN)
                      x5_abilO)
                     y1_abilW)
                    x7_abilQ)
                   x8_abilR)
                  x9_abilS)
                 x10_abilT)
                x11_abilU)
               x12_abilV))
      (f_abilJ x6_abilP)
{-# INLINE numStatics #-}
parentEntries :: Lens' CompactorState (HashMap Text Int)
parentEntries
  f_abilX
  (CompactorState x1_abilY
                  x2_abilZ
                  x3_abim0
                  x4_abim1
                  x5_abim2
                  x6_abim3
                  x7_abim4
                  x8_abim5
                  x9_abim6
                  x10_abim7
                  x11_abim8
                  x12_abim9)
  = (fmap
       (\ y1_abima
          -> (((((((((((CompactorState x1_abilY) x2_abilZ) x3_abim0)
                       x4_abim1)
                      x5_abim2)
                     x6_abim3)
                    x7_abim4)
                   x8_abim5)
                  y1_abima)
                 x10_abim7)
                x11_abim8)
               x12_abim9))
      (f_abilX x9_abim6)
{-# INLINE parentEntries #-}
parentLabels :: Lens' CompactorState (HashMap Text Int)
parentLabels
  f_abimb
  (CompactorState x1_abimc
                  x2_abimd
                  x3_abime
                  x4_abimf
                  x5_abimg
                  x6_abimh
                  x7_abimi
                  x8_abimk
                  x9_abiml
                  x10_abimm
                  x11_abimn
                  x12_abimo)
  = (fmap
       (\ y1_abimp
          -> (((((((((((CompactorState x1_abimc) x2_abimd) x3_abime)
                       x4_abimf)
                      x5_abimg)
                     x6_abimh)
                    x7_abimi)
                   x8_abimk)
                  x9_abiml)
                 x10_abimm)
                y1_abimp)
               x12_abimo))
      (f_abimb x11_abimn)
{-# INLINE parentLabels #-}
parentStatics :: Lens' CompactorState (HashMap Text Int)
parentStatics
  f_abimr
  (CompactorState x1_abims
                  x2_abimt
                  x3_abimu
                  x4_abimv
                  x5_abimw
                  x6_abimx
                  x7_abimy
                  x8_abimz
                  x9_abimA
                  x10_abimB
                  x11_abimC
                  x12_abimD)
  = (fmap
       (\ y1_abimE
          -> (((((((((((CompactorState x1_abims) x2_abimt) x3_abimu)
                       x4_abimv)
                      x5_abimw)
                     x6_abimx)
                    x7_abimy)
                   x8_abimz)
                  x9_abimA)
                 y1_abimE)
                x11_abimC)
               x12_abimD))
      (f_abimr x10_abimB)
{-# INLINE parentStatics #-}
statics :: Lens' CompactorState (HashMap Text Int)
statics
  f_abimG
  (CompactorState x1_abimH
                  x2_abimI
                  x3_abimJ
                  x4_abimK
                  x5_abimL
                  x6_abimM
                  x7_abimN
                  x8_abimO
                  x9_abimP
                  x10_abimQ
                  x11_abimR
                  x12_abimS)
  = (fmap
       (\ y1_abimT
          -> (((((((((((CompactorState x1_abimH) x2_abimI) x3_abimJ)
                       x4_abimK)
                      y1_abimT)
                     x6_abimM)
                    x7_abimN)
                   x8_abimO)
                  x9_abimP)
                 x10_abimQ)
                x11_abimR)
               x12_abimS))
      (f_abimG x5_abimL)
{-# INLINE statics #-}
stringTable :: Lens' CompactorState StringTable
stringTable
  f_abimU
  (CompactorState x1_abimV
                  x2_abimW
                  x3_abimX
                  x4_abimY
                  x5_abimZ
                  x6_abin0
                  x7_abin1
                  x8_abin2
                  x9_abin3
                  x10_abin4
                  x11_abin5
                  x12_abin6)
  = (fmap
       (\ y1_abin7
          -> (((((((((((CompactorState x1_abimV) x2_abimW) x3_abimX)
                       x4_abimY)
                      x5_abimZ)
                     x6_abin0)
                    x7_abin1)
                   x8_abin2)
                  x9_abin3)
                 x10_abin4)
                x11_abin5)
               y1_abin7))
      (f_abimU x12_abin6)
{-# INLINE stringTable #-}



--- end makeLenses

emptyCompactorState :: CompactorState
emptyCompactorState = CompactorState renamedVars
                                     HM.empty
                                     HM.empty
                                     0
                                     HM.empty
                                     0
                                     HM.empty
                                     0
                                     HM.empty
                                     HM.empty
                                     HM.empty
                                     emptyStringTable

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
    putCs (CompactorState [] _ _ _ _ _ _ _ _ _ _ _) =
      panic "putBase: putCs exhausted renamer symbol names"
    putCs (CompactorState (ns:_) nm es _ ss _ ls _ pes pss pls sts) = do
      DB.put ns
      DB.put (HM.toList nm)
      DB.put (HM.toList es)
      DB.put (HM.toList ss)
      DB.put (HM.toList ls)
      DB.put (HM.toList pes)
      DB.put (HM.toList pss)
      DB.put (HM.toList pls)
      DB.put sts
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
      sts <- DB.get
      return (CompactorState (dropWhile (/=n) renamedVars)
                             nm
                             es
                             (HM.size es)
                             ss
                             (HM.size ss)
                             ls
                             (HM.size ls)
                             pes
                             pss
                             pls
                             sts)
    getBase' = do
      hdr <- DB.getByteString 9
      when (hdr /= "GHCJSBASE")
           (panic $ "getBase: invalid base file: " <> file)
      vt  <- DB.getLazyByteString (fromIntegral Object.versionTagLength)
      when (vt /= Object.versionTag)
           (panic $ "getBase: incorrect version: " <> file)
      cs <- makeCompactorParent <$> getCs
      linkedPackages <- getList DB.get
      pkgs <- la <$> getList getPkg
      mods <- la <$> getList DB.get
      funs <- getList (getFun pkgs mods)
      return (Base cs linkedPackages $ S.fromList funs)

-- | make a base state from a CompactorState: empty the current symbols sets,
--   move everything to the parent
makeCompactorParent :: CompactorState -> CompactorState
makeCompactorParent (CompactorState is nm es nes ss nss ls nls pes pss pls sts)
  = CompactorState is
                   nm
                   HM.empty 0
                   HM.empty 0
                   HM.empty 0
                   (HM.union (fmap (+nes) pes) es)
                   (HM.union (fmap (+nss) pss) ss)
                   (HM.union (fmap (+nls) pls) ls)
                   sts

instance DB.Binary Base where
  get = getBase "<unknown file>"
  put = putBase
