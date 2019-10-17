{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}  -- For type equality
{-# LANGUAGE TemplateHaskell #-}


module TypeChecker where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe

import Data.Bifunctor
import Data.Foldable (Foldable, foldMap)
import Data.Function
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set

import Lens.Micro
import Lens.Micro.Mtl
import Lens.Micro.TH

import Text.PrettyPrint

import qualified ParMod.Abs as C
import Syntax.Internal

data TypeError = GenError String
  deriving (Show)

type TCM = ReaderT Context (Except TypeError)
type DCM = StateT ModuleContent TCM

checkProgram :: C.Program -> Either TypeError Module
checkProgram (C.Prg x bs ds) = runExcept . (`runReaderT` emptyContext) $ do
  tel <- checkBindings bs
  pushModule (Module x tel mempty) $ do
    mc <- checkDecls ds `execStateT` mempty
    return $ Module x tel mc

checkBindings :: [C.Bind] -> TCM Telescope
checkBindings []     = return $ mempty
checkBindings (b:bs) = nyi

checkDecls :: [C.Decl] -> DCM ()
checkDecls = mapM_ checkDecl

checkDecl :: C.Decl -> DCM ()
checkDecl = \case
  C.DData x e cs -> do
    t   <- withCurrentModuleContent $ do Univ <$> do isUniverse =<< checkExpr e
    -- allocate AName for data type
    y   <- newData x t
    mapM_ (checkConstructors y) cs

checkConstructors :: AName -> C.TSig -> DCM ()
checkConstructors d (C.TSig xs e) = nyi

checkExpr :: C.Expr -> TCM Expr
checkExpr = \case
  C.EId x
    | Just n <- isUniv x -> return $ Univ n
  _ -> nyi

isUniverse :: Expr -> TCM Nat
isUniverse = \case
  Univ n -> return n
  e -> notAUniverseError e

-- * TCM ops

nyi :: a
nyi = error "Not yet implemented"

-- nyi :: MonadError TypeError m => m a
-- nyi = throwError $ TypeError $ "Not yet implemented"

pushModule :: Module -> TCM a -> TCM a
pushModule m = local $ \ (Context ms EmptyTel) -> Context (m:ms) EmptyTel

withCurrentModuleContent :: TCM a -> DCM a
withCurrentModuleContent m = do
  mc <- get
  lift $ local (\ (Context (m:ms) tel) -> Context ((const mc <$> m) : ms) tel) m

newData :: CName -> Expr -> DCM AName
newData x t = do
  st <- get
  let Signature n xs sig = modSig st
  -- TODO check duplicate definition
  let y = succ n
  let def = Definition x t $ Datatype $ DataDef []
  put $ st { modSig = Signature y (Map.insert x y xs) (IntMap.insert y def sig) }
  return y

-- * Errors

notAUniverseError :: MonadError TypeError m => Expr -> m a
notAUniverseError = nyi

-- * Lens tools

lensHead :: Lens' [a] a
lensHead f (a:as) = (:as) <$> f a

infix 4 %==

-- | Modify a part of the state monadically.
(%==) :: MonadState o m => Lens' o i -> (i -> m i) -> m ()
l %== f = put =<< l f =<< get

-- * Generic utilities

nub' :: Ord a => [a] -> [a]
nub' = Set.toList . Set.fromList

firstSuccess :: Monad m => [m (Maybe a)] -> m (Maybe a)
firstSuccess = \case
  []   -> return Nothing
  m:ms -> m >>= \case
    Nothing  -> firstSuccess ms
    r@Just{} -> return r

-- | Convert between error monads.

maybeToEither :: Maybe a -> Either () a
maybeToEither = \case
  Nothing -> Left ()
  Just a  -> Right a

-- | Adding to a <=1 collection may fail.

addMaybe :: a -> Maybe a -> Maybe (Maybe a)
addMaybe a Nothing = Just (Just a)
addMaybe a Just{}  = Nothing

-- | Partial semigroup.

class UnionMaybe a where
  unionMaybe :: a -> a -> Maybe a

instance UnionMaybe (Maybe a) where
  unionMaybe = \case
    Nothing -> Just
    Just a  -> addMaybe a
