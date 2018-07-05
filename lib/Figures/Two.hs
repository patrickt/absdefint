{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Figures.Two
    ( Bin (..)
    , Exp (..)
    , Failure (..)
    , Evaluator
    , Env
    , ID
    , Name
    , Store
    , ext
    , find
    , isZero
    , subexps
    , notFound
    , alloc
    , divideByZero
    , ẟ
    ) where

import Prelude hiding (lookup)

import Data.Map (Map, lookup, insert)
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

type Name = String

type ID = Int

type Env = Map Name ID

type Store = Map ID Exp

data Bin = Add | Sub | Mul | Div deriving (Eq, Show, Ord)

data Exp
  = Var Name             -- | Variables
  | Num Int              -- | Literals
  | If Exp Exp Exp       -- | Conditions
  | App Exp Exp          -- | Function application
  | Rec Name Exp         -- | Let-binding (recursive)
  | Lam Name Exp         -- | Lambdas
  | Op Bin Exp Exp       -- | Arithmetic
  | Closure Exp Env      -- | Datum + scope
    deriving (Eq, Show, Ord)

makeBaseFunctor ''Exp

subexps :: Exp -> [Exp]
subexps = para (foldMap (uncurry (:)))

instance Num Exp where
  fromInteger = Num . fromInteger
  (+) = Op Add
  (-) = Op Sub
  (*) = Op Mul

  negate = Op Sub 0
  abs = error "abs over Exp not defined"
  signum = error "signum over Exp not defined"

-- N.B. 'Closure' is not defined as a syntax type in ADI, but this is
-- the cleanest place to put it, considering that in the paper they
-- paper over it with a specious little cons. I see y'all.

type Evaluator effs = Exp -> Eff effs Exp

isZero :: Exp -> Bool
isZero = (== (Num 0))

data Failure
  = NotFound Name
  | InvalidID ID
  | TypeError Exp
  | DivideByZero
    deriving (Eq, Show)

ẟ :: Member (Error Failure) effs => Bin -> Exp -> Exp -> Eff effs Exp
ẟ op n' m' = do
  n <- asNumber n'
  m <- asNumber m'
  case op of
    Add -> pure (Num (n + m))
    Sub -> pure (Num (n - m))
    Mul -> pure (Num (n * m))
    Div -> if m == 0 then divideByZero else pure (Num (div n m))

find :: ( Member (State Store) effs
        , Member (Error Failure) effs
        )
     => ID
     -> Eff effs Exp
find i = fmap (lookup i) (get @Store)  >>= maybe (invalidID i) pure

ext :: Member (State Store) effs
    => ID -> Exp -> Eff effs ()
ext i = modify . insert i

alloc :: Member (State Store) effs => Name -> Eff effs ID
alloc _ = length <$> get @Store

-- Smart constructors; not in the original paper

asNumber :: Member (Error Failure) effs => Exp -> Eff effs Int
asNumber (Num i) = pure i
asNumber e = throwError (TypeError e)

notFound :: Member (Error Failure) effs => Name -> Eff effs a
notFound = throwError . NotFound

invalidID :: Member (Error Failure) effs => ID -> Eff effs a
invalidID = throwError . InvalidID

divideByZero :: Member (Error Failure) effs => Eff effs a
divideByZero = throwError DivideByZero
