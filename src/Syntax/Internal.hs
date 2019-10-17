{-# LANGUAGE DeriveFunctor #-}

-- | Type-checked syntax.

module Syntax.Internal where

import Data.List (stripPrefix)
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Text.Read (readMaybe)

import qualified ParMod.Abs as C

type CName  = C.Name
type CQName = C.QName

type Nat   = Int
type AName = Nat
type QName = [AName]

-- * Bound variables

data Index = Index
  { iBlock :: Nat
  , iEntry :: Nat
  } deriving (Show)

data VarName = VarName
  { varIndex :: Index
  , varCName :: CName
  } deriving (Show)

-- * Qualified Symbols

data DefName = DefName
  { defKind  :: Kind
  , defIndex :: QName
  , defCName :: CQName
  } deriving (Show)

-- | Definition types
data Kind = Con | Dat | Fun
  deriving (Show)

-- * Module Names

data ModName = ModName
  { modIndex :: QName
  , modCName :: CQName
  } deriving (Show)

-- * Terms and types

data Expr
  = EVar VarName [Expr]
  | EDef DefName [Expr]
  | EPi Type (Abs Type)
  | Univ Nat
  deriving (Show)

type Type = Expr

data Abs a
  = Abs C.Name a
  | NoAbs a
  deriving (Show)

-- Should telescope be a list (or array?)
data Telescope
  = EmptyTel
  | ExtendTel Expr (Abs Telescope)
  deriving (Show)

-- * CaseTrees

data CaseTree = CaseTree
  deriving (Show)

-- * Signature

newtype FunDef  = FunDef
    { funCaseTree :: CaseTree
    } deriving (Show)

newtype DataDef = DataDef
    { dataCons :: [AName]
      -- ^ Constructors, in original order.
      --   Just a simple name, as they need to be in the same module as the data type.
    } deriving (Show)
newtype ConDef = ConDef
    { conData :: AName
      -- ^ Data type this constructor belongs to.
      --   Just a simple name, as constructor is in the same module as its data type.
    } deriving (Show)

-- -- | @f@ can be 'Maybe' (if possibly undefined) or 'Identity' (if definitely defined).
data Defn
  = Function FunDef
  | Datatype DataDef
  | Constructor ConDef
  deriving (Show)

data Definition = Definition
  { defName :: CName
  , defType :: Expr
  , theDef  :: Defn
  } deriving (Show)

data Signature = Signature
  { sigSize  :: Nat
  , sigNames :: Map CName AName
  , theSig   :: IntMap Definition
  } deriving (Show)

-- * Modules

data ModSig = ModSig
  { msigSize  :: Nat
  , msigNames :: Map CName AName
  , theMSig   :: IntMap ModuleDefinition
  } deriving (Show)

type ModuleDefinition = Module' ModDef
data ModDef
  = ModDef ModuleContent
  | ModAlias ModuleAlias
  deriving (Show)

data Module' a = Module
  { modName :: CName
  , modTel  :: Telescope
  , modDef  :: a
  } deriving (Show, Functor)

data ModuleAlias = ModuleAlias
  { maMod  :: ModName
  , maArgs :: [Expr]
  } deriving (Show)

type Module = Module' ModuleContent
data ModuleContent = ModuleContent
  { modMSig :: ModSig
  , modSig  :: Signature
  } deriving (Show)


-- * Contexts

-- The contexts is everything above the current POV.
-- * the current module and its parents.
-- * a local context

data Context = Context
  { cxtModules :: [Module] -- ^ Child first, parent last.
  , cxtTel     :: Telescope -- ^ Local variables.
  } deriving (Show)

emptyContext :: Context
emptyContext = Context [] EmptyTel

-- * Instances

instance Semigroup Telescope where
instance Monoid Telescope where
  mempty = EmptyTel

instance Semigroup Signature where
instance Monoid Signature where
  mempty = Signature 0 mempty mempty

instance Semigroup ModSig where
instance Monoid ModSig where
  mempty = ModSig 0 mempty mempty

instance Semigroup ModuleContent where
  ModuleContent m s <> ModuleContent m' s' = ModuleContent (m <> m') (s <> s')

instance Monoid ModuleContent where
  mempty = ModuleContent mempty mempty
  mappend = (<>)

-- | Parse Set, Set0, Set1
isUniv :: C.QName -> Maybe Nat
isUniv (C.QName (C.Name x)) = isNatOrEmpty =<< stripPrefix "Set" x
isUniv _ = Nothing

isNatOrEmpty :: String -> Maybe Nat
isNatOrEmpty "" = return 0
isNatOrEmpty s  = readMaybe s
