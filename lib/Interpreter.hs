{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

module Interpreter where

import           Syntax

import           Bound
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             (Map)
import           Debug.Trace
import qualified Data.Map             as M

-- | Errors
data Err e = DivideByZero
           | NotFound Name
           | BadArgs Bin (Exp e) (Exp e)
           | BadApp (Exp e) (Exp e)
           deriving (Show, Eq)

-- | ID represents locations in a 'Store'.
type ID = Int

-- | Environments map names to IDs.
type Env = Map Name ID

-- | Stores map IDs to values.
type Store e = Map ID (Exp e)

-- | A monad for definitional interpreters that track bindings in an 'Env' and values in a 'Store'.
newtype Interp e a = Interp { runInterp :: ReaderT Env (ExceptT (Err e) (State (Store e))) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState (Store e)
           , MonadError (Err e)
           , MonadReader Env
           )

type MonadInterp m e = ( MonadState (Store e) m
                       , MonadError (Err e) m
                       , MonadReader Env m)

-- | This is the first interpreter. It is invoked in an open-recursive style with 'fix'
-- (the Y combinator) to provide open interpretation.
eval1 :: (MonadInterp m Name)
      => (Exp Name -> m (Exp Name))
      -> Exp Name
      -> m (Exp Name)
eval1 go e = case e of
  -- Numbers and lambdas evaluate to themselves
  Num x -> return (Num x)
  Lam b -> return (Lam b)
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
    -- Evaluate this recursively with a local environment
    local augment $ do
      -- Insert the result into the symbol table
      modify (M.insert pos v)
      go a
  -- Application
  App f a -> do
    fun <- go f
    case fun of
      Lam b -> do
        arg <- go a
        pos <- alloc (show arg)
        modify (M.insert pos arg)
        go (instantiate1 arg b) -- instantiate1 performs variable substitution
      other -> go (App other a)

-- | Performs variable lookups.
find :: (Show a, MonadInterp m a) => Name -> Env -> m (Exp a)
find n e = do
  st <- get
  en <- ask
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
alloc :: MonadInterp m e => Name -> m Int
alloc _ = M.size <$> get

-- | Executes an interpreter; equivalent to ADI's runm
execInterp :: Interp e a -> (Either (Err e) a, Store e)
execInterp = flip runState mempty . runExceptT . flip runReaderT mempty . runInterp

eval :: Exp Name -> (Either (Err Name) (Exp Name), Store Name)
eval = execInterp . fix eval1
