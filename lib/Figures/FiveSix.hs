{-# LANGUAGE LambdaCase #-}

module Figures.FiveSix
  ( neval )
  where

import Prelude hiding (lookup)

import Common hiding (Exp (..), ID, Store, Env)
import Control.Applicative
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import Control.Monad.Freer.NonDet
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

-- TODO for idiomatic work: use base functor for Exp,
-- Union for N + Exp, and Store and Env in terms of ID
-- TODO test me

data Exp
  = Var Name             -- | Variables
  | Num Int              -- | Literals
  | If Exp Exp Exp       -- | Conditions
  | App Exp Exp          -- | Function application
  | Rec Name Exp         -- | Let-binding (recursive)
  | Lam Name Exp         -- | Lambdas
  | Op Bin Exp Exp       -- | Arithmetic
  | Closure Exp Env      -- | Datum + scope
  | N
    deriving (Eq, Show, Ord)

type Store = Map Name (Set Exp)

type Env = Set Name

type AltMonad = '[ Reader Env
                 , State Store
                 , Error Failure
                 , NonDet
                 ]

ẟ :: (Member (Error Failure) effs, Member NonDet effs) => Bin -> Exp -> Exp -> Eff effs Exp
ẟ Div _ (Num 0) = divideByZero
ẟ Div _ _ = divideByZero <|> pure N
ẟ _ _ _   = pure N

alloc :: Applicative f => Name -> f Name
alloc = pure

find :: ( Member (State Store) effs
        , Member NonDet effs
        , Member (Error Failure) effs
        ) => Name -> Eff effs Exp
find n = do
  σ <- get @Store
  case Map.lookup n σ of
    Nothing   -> notFound n
    Just vals -> msum (fmap pure (Set.toList vals))

ext :: Member (State Store) effs => Name -> Exp -> Eff effs ()
ext n e = modify (Map.insertWith (<>) n (Set.singleton e))

isZero :: Member NonDet effs => Exp -> Eff effs Bool
isZero N = mplus (pure True) (pure False)
isZero n = pure (n == (Num 0))

neval1 :: Members AltMonad effs
       => Evaluator Exp effs
       -> Evaluator Exp effs
neval1 recur = \case
  N ->
    pure N
  n@Num{} ->
    pure n
  (Var n) -> do
    ρ <- ask
    if Set.member n ρ
      then find n
      else notFound n
  (If cond l r) -> do
    v <- recur cond
    z <- isZero v
    recur (if z then r else l)
  Op bin l' r' -> do
    l <- recur l'
    r <- recur r'
    ẟ bin l r
  Rec f e -> do
    a <- alloc f
    let ρ' = Set.insert a
    v <- local ρ' (recur e)
    v <$ ext a v
  Lam x e ->
    Closure (Lam x e) <$> ask
  App l r -> do
    Closure (Lam x e) ρ <- recur l
    v <- recur r
    a <- alloc x
    ext a v
    local (const (Set.insert a ρ)) (recur e)
  Closure e _ ->
    pure e

neval :: Exp -> [Either Failure Exp]
neval = run
      . makeChoiceA
      . runError
      . evalState mempty
      . runReader mempty
      . go
  where go = fix (neval1 @AltMonad)
