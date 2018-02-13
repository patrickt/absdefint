{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}

module Syntax
    ( Bin (..)
    , Exp (..)
    , Name
    , lam
    , isZero
    ) where

import           Control.Monad (ap)
import           Data.Deriving
import           Data.Functor.Classes

type Name = String

data Bin = Add | Sub | Mul | Div deriving (Eq, Show, Ord)

data Exp a
  = Var a                      -- | Variables
  | Num Int                    -- | Literals
  | If (Exp a) (Exp a) (Exp a) -- | Conditions
  | App (Exp a) (Exp a)        -- | Function application
  | Let a (Exp a) (Exp a)      -- | Let-binding (recursive)
  | Lam a (Exp a)              -- | Lambdas
  | Op Bin (Exp a) (Exp a)     -- | Arithmetic
    deriving (Functor, Foldable, Traversable)

-- Gives us Eq, Show, and Ord
deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveShow1 ''Exp

instance Eq a   => Eq   (Exp a) where (==) = eq1
instance Ord a  => Ord  (Exp a) where compare = compare1
instance Show a => Show (Exp a) where showsPrec = showsPrec1

instance Applicative Exp where
  pure  = Var
  (<*>) = ap

instance Monad Exp where
  Var a >>= f     = f a
  Num n >>= _     = Num n
  If a b c >>= f  = If (a >>= f) (b >>= f) (c >>= f)
  App a b >>= f   = App (a >>= f) (b >>= f)
  Let n a v >>= f = f n >>= (\x -> Let x (a >>= f) (v >>= f))
  Lam n e >>= f   = f n >>= (\x -> Lam x (e >>= f))
  Op a b c >>= f  = Op a (b >>= f) (c >>= f)

-- Smart constructor for lambdas
lam :: Eq a => a -> Exp a -> Exp a
lam = Lam

-- Conditional tester
isZero :: Exp a -> Bool
isZero (Num 0) = True
isZero _       = False
