{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Interpreter where

import           Syntax

import           Bound
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Map             (Map)
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

-- | Executes an interpreter; equivalent to ADI's runm
execInterp :: Interp e a -> (Either (Err e) a, Store e)
execInterp = flip runState mempty . runExceptT . flip runReaderT mempty . runInterp

-- | This is the first interpreter. It is invoked in an open-recursive style with 'fix'
-- (the Y combinator) to provide open interpretation.
eval1 :: (Exp Name -> Interp Name (Exp Name)) -> Exp Name -> Interp Name (Exp Name)
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
  Rec n a -> do
    pos <- alloc n
    let augment = M.insert n pos
    -- Evaluate this recursively with a local environment
    res <- local augment (go a)
    -- Insert the result into the symbol table
    modify (M.insert pos (fmap show res))
    return res
  -- Application
  App f a -> do
    arg <- go a
    fun <- go f
    case fun of
      Lam b -> go (instantiate1 arg b) -- instantiate1 performs variable substitution
      other -> throwError (BadApp f other)

-- | Performs variable lookups.
find :: Name -> Env -> Interp e (Exp e)
find n e = do
  st <- get
  case M.lookup n e >>= flip M.lookup st of
    Nothing -> throwError (NotFound n)
    Just f  -> return f

-- | Performs arithmetic.
delta :: Show a => Bin -> Exp a -> Exp a -> Interp a Int
delta Div _ (Num 0)      = throwError DivideByZero
delta op (Num a) (Num b) = pure $ case op of
  Add -> a + b
  Sub -> a - b
  Mul -> a * b
  Div -> a `div` b
delta op a b = throwError (BadArgs op a b)

-- | Computes the next available location in the store.
alloc :: Name -> Interp e Int
alloc _ = M.size <$> get
