module Figures.Three
    ( evalTrace
    , Log (..)
    ) where

import Figures.One
import Figures.Two

import Control.Monad.Freer
import Control.Monad.Freer.Error
import Control.Monad.Freer.Reader
import Control.Monad.Freer.State
import Control.Monad.Freer.Writer
import Data.Function

data Log = Log Exp Env Store
  deriving (Show, Eq)

-- Trace-collecting semantics
type TraceMonad = '[ Reader Env
                   , State Store
                   , Error Failure
                   , Writer [Log]
                   ]

evalTrace1 :: Members TraceMonad effs
           => (Evaluator effs -> Evaluator effs)
           -> Evaluator effs
           -> Evaluator effs
evalTrace1 base recur e = do
  traced <- Log e <$> ask <*> get
  tell [traced]
  (base recur) e

evalTrace :: Exp -> (Either Failure Exp, [Log])
evalTrace = run
          . runWriter
          . runError
          . evalState mempty
          . runReader mempty
          . go
  where go = fix (evalTrace1 @TraceMonad eval1)
