{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Figures.One
  ( eval
  , eval1
  )
  where

import Prelude hiding (lookup)

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Data.Function
import Data.Map (lookup, insert)

import Figures.Two

type EvalMonad = '[Reader Env, State Store, Error Failure]

eval1 :: Members EvalMonad effs
      => Evaluator effs
      -> Evaluator effs
eval1 recur = \case
  Num i ->
    pure (Num i)
  Var n -> do
    ρ <- ask
    lookup n ρ & maybe (notFound n) find
  If cond l r -> do
    v <- recur cond
    let z = isZero v
    recur (if z then r else l)
  Op bin l' r' -> do
    l <- recur l'
    r <- recur r'
    ẟ bin l r
  Rec f e -> do
    a <- alloc f
    let ρ' = insert f a
    v <- local ρ' (recur e)
    v <$ ext a v
  Lam x e ->
    Closure (Lam x e) <$> ask
  App l r -> do
    Closure (Lam x e) ρ <- recur l
    v <- recur r
    a <- alloc x
    ext a v
    local (const (insert x a ρ)) (recur e)
  Closure e _ ->
    pure e

eval :: Exp -> Either Failure Exp
eval = run
       . runError
       . evalState mempty
       . runReader mempty
       . go
  where go = fix (eval1 @EvalMonad)
