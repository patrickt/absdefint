{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

module Common
  ( Bin (..)
  , Exp (..)
  , Failure (..)
  , Evaluator
  , Env
  , ID
  , Name
  , Store
  , asNumber
  , divideByZero
  , invalidID
  , notFound
  , subexps

  ) where

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Map (Map)

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

-- N.B. 'Closure' is not defined as a syntax type in ADI, but this is
-- the cleanest place to put it, considering that in the paper they
-- paper over it with a specious little cons. I see y'all.

data Failure
  = NotFound Name
  | InvalidID ID
  | TypeError Exp
  | DivideByZero
    deriving (Eq, Show)

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


-- TODO change the order around
type Evaluator exp effs = exp -> Eff effs exp

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
