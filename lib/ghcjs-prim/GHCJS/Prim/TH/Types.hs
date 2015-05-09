{-# OPTIONS_GHC -O0 #-}
{-# LANGUAGE CPP, DeriveGeneric, DeriveDataTypeable, LambdaCase, MagicHash, StandaloneDeriving #-}

{- |
     Communication between the compiler (GHCJS) and runtime (on node.js) for
     Template Haskell
 -}

module GHCJS.Prim.TH.Types ( Message(..)
                           , THResultType(..)
                           ) where

import           Control.Applicative

import           Data.Binary
import           Data.ByteString (ByteString)
import           Data.Data
import           Data.Word

import           GHC.Generics
import           GHC.Exts

import           GHCJS.Prim.TH.Serialized

import qualified Language.Haskell.TH        as TH
import qualified Language.Haskell.TH.Syntax as TH

data THResultType = THExp | THPat | THType | THDec | THAnnWrapper
  deriving (Enum, Show, Data, Generic)

data Message
  -- | compiler to node requests
  = RunTH THResultType ByteString (Maybe TH.Loc)
  | FinishTH
  -- | node to compiler responses
  | RunTH'            ByteString -- ^ serialized result
  | FinishTH'
  -- | node to compiler requests
  | NewName           String
  | Report            Bool String
  | LookupName        Bool String
  | Reify             TH.Name
  | ReifyInstances    TH.Name [TH.Type]
  | ReifyRoles        TH.Name
  | ReifyAnnotations  TH.AnnLookup
  | ReifyModule       TH.Module
  | AddDependentFile  FilePath
  | AddTopDecls       [TH.Dec]
  -- | compiler to node responses
  | NewName'          TH.Name
  | Report'
  | LookupName'       (Maybe TH.Name)
  | Reify'            TH.Info
  | ReifyInstances'   [TH.Dec]
  | ReifyRoles'       [TH.Role]
  | ReifyAnnotations' [ByteString]
  | ReifyModule'      TH.ModuleInfo
  | AddDependentFile'
  | AddTopDecls'
  | QFail'
  | QCompilerException' Int String -- ^ exception id and result of showing the exception
  -- | error recovery
  | StartRecover
  | EndRecover Bool            -- ^ true for recovery action taken
  | StartRecover'
  | EndRecover'
  -- | exit with error status
  | QFail              String  -- ^ monadic fail called
  | QUserException     String  -- ^ exception in user code
  | QCompilerException Int     -- ^ exception originated on compiler side
  deriving (Data, Generic)

instance Binary THResultType
instance Binary Message

#if MIN_VERSION_template_haskell(2,11,0)
#error "unsupported template-haskell version"
#elif MIN_VERSION_template_haskell(2,9,0)

#if !MIN_VERSION_template_haskell(2,10,0)
deriving instance Generic TH.Pred

deriving instance Generic TH.Loc
deriving instance Generic TH.Name
deriving instance Generic TH.ModName
deriving instance Generic TH.PkgName
deriving instance Generic TH.NameSpace
deriving instance Generic TH.Module
deriving instance Generic TH.Info
deriving instance Generic TH.Type
deriving instance Generic TH.TyLit
deriving instance Generic TH.TyVarBndr
deriving instance Generic TH.Role
deriving instance Generic TH.Lit
deriving instance Generic TH.Range
deriving instance Generic TH.Stmt
deriving instance Generic TH.Pat
deriving instance Generic TH.Exp
deriving instance Generic TH.Dec
deriving instance Generic TH.Guard
deriving instance Generic TH.Body
deriving instance Generic TH.Match
deriving instance Generic TH.Fixity
deriving instance Generic TH.TySynEqn
deriving instance Generic TH.FamFlavour
deriving instance Generic TH.FunDep
deriving instance Generic TH.AnnTarget
deriving instance Generic TH.RuleBndr
deriving instance Generic TH.Phases
deriving instance Generic TH.RuleMatch
deriving instance Generic TH.Inline
deriving instance Generic TH.Pragma
deriving instance Generic TH.Safety
deriving instance Generic TH.Callconv
deriving instance Generic TH.Foreign
deriving instance Generic TH.Strict
deriving instance Generic TH.FixityDirection
deriving instance Generic TH.OccName
deriving instance Generic TH.Con
deriving instance Generic TH.AnnLookup
deriving instance Generic TH.ModuleInfo
deriving instance Generic TH.Clause
#endif

#if !MIN_VERSION_template_haskell(2,10,0)
instance Binary TH.Pred
#endif

instance Binary TH.Loc
instance Binary TH.Name
instance Binary TH.ModName
#if MIN_VERSION_template_haskell(2,10,0)
instance Binary TH.NameFlavour
#else
instance Binary TH.NameFlavour where
  put TH.NameS             = putWord8 1
  put (TH.NameQ mn)        = putWord8 2 >> put mn
  put (TH.NameU i)         = putWord8 3 >> put (I# i)
  put (TH.NameL i)         = putWord8 4 >> put (I# i)
  put (TH.NameG ns pkg mn) = putWord8 5 >> put ns >> put pkg >> put mn
  get = getWord8 >>= \case
                        1 -> return TH.NameS
                        2 -> TH.NameQ <$> get
                        3 -> (\(I# i) -> TH.NameU i) <$> get
                        4 -> (\(I# i) -> TH.NameL i) <$> get
                        5 -> TH.NameG <$> get <*> get <*> get
                        _ -> error "get Name: invalid tag"
#endif
instance Binary TH.PkgName
instance Binary TH.NameSpace
instance Binary TH.Module
instance Binary TH.Info
instance Binary TH.Type
instance Binary TH.TyLit
instance Binary TH.TyVarBndr
instance Binary TH.Role
instance Binary TH.Lit
instance Binary TH.Range
instance Binary TH.Stmt
instance Binary TH.Pat
instance Binary TH.Exp
instance Binary TH.Dec
instance Binary TH.Guard
instance Binary TH.Body
instance Binary TH.Match
instance Binary TH.Fixity
instance Binary TH.TySynEqn
instance Binary TH.FamFlavour
instance Binary TH.FunDep
instance Binary TH.AnnTarget
instance Binary TH.RuleBndr
instance Binary TH.Phases
instance Binary TH.RuleMatch
instance Binary TH.Inline
instance Binary TH.Pragma
instance Binary TH.Safety
instance Binary TH.Callconv
instance Binary TH.Foreign
instance Binary TH.Strict
instance Binary TH.FixityDirection
instance Binary TH.OccName
instance Binary TH.Con
instance Binary TH.AnnLookup
instance Binary TH.ModuleInfo
instance Binary TH.Clause

#else
#error "unsupported template-haskell version"
#endif

