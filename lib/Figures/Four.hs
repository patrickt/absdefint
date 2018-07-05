module Figures.Four
  ( evalDead
  ) where

import Figures.One
import Figures.Two

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Data.Function (fix)
import Data.Set (Set, delete, fromList)

type DeadMonad = '[ Reader Env
                  , State Store
                  , Error Failure
                  , State (Set Exp)
                  ]

evalDead1 :: Members DeadMonad effs
          => (Evaluator effs -> Evaluator effs)
          -> Evaluator effs
          -> Evaluator effs
evalDead1 base recur e = do
  modify (delete e)
  (base recur) e

evalDead :: Exp -> (Either Failure Exp, Set Exp)
evalDead e = run
             . runState (fromList (subexps e))
             . runError
             . evalState mempty
             . runReader mempty
             . go $ e
  where go = fix (evalDead1 @DeadMonad eval1)
