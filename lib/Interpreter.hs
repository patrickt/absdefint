{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Interpreter where

import           Syntax

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             (Map)
import qualified Data.Map             as M

-- | Errors
data Err e = DivideByZero
           | NotFound e
           | BadArgs Bin (Exp e) (Exp e)
           | BadApp (Exp e) (Exp e)
           deriving (Show, Eq)

-- | ID represents locations in a 'Store'.
type ID = Int

-- | Environments map names to IDs.
type Env e = Map e ID

-- | Stores map IDs to values.
type Store e = Map ID (Exp e)

-- | A monad for definitional interpreters that track bindings in an 'Env' and values in a 'Store'.
newtype Interp e a = Interp { runInterp :: ReaderT (Env e) (ExceptT (Err e) (State (Store e))) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (Store e)
           , MonadError (Err e)
           , MonadReader (Env e)
           )

type MonadInterp m e = ( MonadState (Store e) m
                       , MonadError (Err e) m
                       , MonadReader (Env e) m)

-- | This is the first interpreter. It is invoked in an open-recursive style with 'fix'
-- (the Y combinator) to provide open interpretation.
eval1 :: (Show a, Ord a, MonadInterp m a)
      => (Exp a -> m (Exp a))
      -> Exp a
      -> m (Exp a)
eval1 go e = case e of
  -- Numbers and lambdas evaluate to themselves
  Num x -> return (Num x)
  Lam n b -> return (Lam n b)
  -- Variables call out to 'find' to perform lookups
  Var n -> ask >>= find n >>= go
  -- Straightforward if application
  If cond onTrue onFalse -> do
    v <- go cond
    let z = isZero v
    go (if z then onTrue else onFalse)
  -- Eval arguments then call out to delta
  Op o left right -> do
    l <- go left
    r <- go right
    Num <$> delta o l r
  -- Let-binding
  Let n v a -> do
    pos <- alloc n
    let augment = M.insert n pos
    -- Evaluate this recursively with a local environment (letrec)
    local augment $ do
      arg <- go v
      -- Insert the result of evaluating the argument into the symbol table
      modify (M.insert pos arg)
      -- eval the body
      go a
  -- Application
  App f a -> do
    fun <- go f
    case fun of
      Lam n b -> do
        pos <- alloc n
        let augment = M.insert n pos
        local augment $ do
          arg <- go a
          modify (M.insert pos arg)
          go b
      other -> go (App other a)

-- | Performs variable lookups.
find :: (Show a, Ord a, MonadInterp m a) => a -> (Env a) -> m (Exp a)
find n e = do
  st <- get
  case M.lookup n e >>= flip M.lookup st of
    Nothing -> throwError (NotFound n)
    Just f  -> return f

-- | Performs arithmetic.
delta :: (MonadInterp m a, Show a) => Bin -> Exp a -> Exp a -> m Int
delta Div _ (Num 0)      = throwError DivideByZero
delta op (Num a) (Num b) = pure $ case op of
  Add -> a + b
  Sub -> a - b
  Mul -> a * b
  Div -> a `div` b
delta op a b = throwError (BadArgs op a b)

-- | Computes the next available location in the store.
alloc :: MonadInterp m e => a -> m Int
alloc _ = M.size <$> get

-- | Executes an interpreter; equivalent to ADI's runm
execInterp :: Ord e => Interp e a -> (Either (Err e) a, Store e)
execInterp = flip runState mempty . runExceptT . flip runReaderT mempty . runInterp

eval :: (Show a, Ord a) => Exp a -> (Either (Err a) (Exp a), Store a)
eval = execInterp . fix eval1
