module Figures.Seven
  ( ceval1
  , ceval
  ) where

import Figures.FiveSix

import Common hiding (Exp (..), ID, Store, Env)
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Control.Monad.Freer.Reader
import Control.Monad.Freer.NonDet
import Data.Function
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Config = Config Exp Env Store
  deriving (Show, Eq, Ord)

type Cache = Map Config (Set (Exp, Store))

type CacheMonad = '[ Reader Env
                   , State Store
                   , Error Failure
                   , NonDet
                   , Reader Cache
                   , State Cache
                   ]

ceval1 :: Members CacheMonad effs
       => (Evaluator Exp effs -> Evaluator Exp effs)
       -> Evaluator Exp effs
       -> Evaluator Exp effs
ceval1 base recur e = do
  ρ <- ask @Env
  σ <- get @Store
  let ς = Config e ρ σ
  outcache <- get @Cache
  let res = Map.lookup ς outcache
  case res of
    Just cached -> do
      let go (v, s) = v <$ put s
      msum (fmap go (Set.toList cached))
    Nothing -> do
      incache <- ask @Cache
      let pair = fromMaybe Set.empty (Map.lookup ς incache)
      modify @Cache (Map.insert ς pair)
      v <- (base recur) e
      σ' <- get @Store
      let new = (v, σ')
      modify @Cache (Map.adjust (Set.insert new) ς)
      pure v

fixCache :: Members CacheMonad effs
         => Evaluator Exp effs
         -> Evaluator Exp effs
fixCache eval e = do
  ρ <- ask @Env
  σ <- get @Store
  let ς = Config e ρ σ
  oracle <- converge mempty $ \cache -> do
    put @Cache mempty
    put @Store σ
    void $ local (Map.union cache) (eval e)
    get @Cache

  let process (v, s) = v <$ put @Store s
  let result = Map.findWithDefault (Map.fromList []) ς oracle
  msum (fmap process (Set.toList result))

converge :: (Monad m, Eq point) => point -> (point -> m point) -> m point
converge p recur = loop p where
  loop x = do
    x' <- recur x
    (if x == x' then pure else loop) x

ceval :: Exp -> [Either Failure Exp]
ceval = run
      . evalState mempty
      . runReader mempty
      . makeChoiceA
      . runError
      . evalState @Store mempty
      . runReader @Env mempty
      . go
  where go = fixCache (fix (ceval1 @CacheMonad neval1))
