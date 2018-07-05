module Figures.Two
    ( module Common
    , ext
    , find
    , isZero
    , alloc
    , ẟ
    ) where

import Prelude hiding (lookup)

import Common
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Data.Map (lookup, insert)

ẟ :: Member (Error Failure) effs => Bin -> Exp -> Exp -> Eff effs Exp
ẟ op n' m' = do
  n <- asNumber n'
  m <- asNumber m'
  case op of
    Add -> pure (Num (n + m))
    Sub -> pure (Num (n - m))
    Mul -> pure (Num (n * m))
    Div -> if m == 0 then divideByZero else pure (Num (div n m))

isZero :: Exp -> Bool
isZero = (== (Num 0))

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
