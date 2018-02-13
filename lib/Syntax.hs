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

import           Bound
import           Data.Deriving
import           Data.Functor.Classes

type Name = String

data Bin = Add | Sub | Mul | Div deriving (Eq, Show, Ord)

data Exp a
  = Var a                      -- | Variables
  | Num Int                    -- | Literals
  | If (Exp a) (Exp a) (Exp a) -- | Conditions
  | App (Exp a) (Exp a)        -- | Function application
  | Rec Name (Exp a)           -- | Let-binding (wrong?)
  | Lam (Scope () Exp a)       -- | Lambdas
  | Op Bin (Exp a) (Exp a)     -- | Arithmetic
    deriving (Functor, Foldable, Traversable)

-- Gives us Eq, Show, and Ord
deriveEq1   ''Exp
deriveOrd1  ''Exp
deriveShow1 ''Exp

instance Eq a   => Eq   (Exp a) where (==) = eq1
instance Ord a  => Ord  (Exp a) where compare = compare1
instance Show a => Show (Exp a) where showsPrec = showsPrec1

-- Gives us the straightforward definitions of Applicative and Monad
makeBound ''Exp

-- Smart constructor for lambdas
lam :: Eq a => a -> Exp a -> Exp a
lam v b = Lam (abstract1 v b)

-- Conditional tester
isZero :: Exp a -> Bool
isZero (Num 0) = True
isZero _       = False
