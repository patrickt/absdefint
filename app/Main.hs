{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TemplateHaskell            #-}

module Main where

import           Bound
import           Control.Monad        (ap)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Deriving
import           Data.Functor.Classes
import           Data.Map             (Map)
import qualified Data.Map             as M
-- import           Data.Monoid

type Name = String

data Bin = Add | Sub | Mul | Div deriving (Eq, Show, Ord)

data Exp a
  = Var a
  | Num Int
  | If (Exp a) (Exp a) (Exp a)
  | App (Exp a) (Exp a)
  | Rec Name (Exp a)
  | Lam (Scope () Exp a)
  | Op Bin (Exp a) (Exp a)
    deriving (Functor, Foldable, Traversable)

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
  Rec n a >>= f   = Rec n (a >>= f)
  Lam e >>= f     = Lam (e >>>= f)
  Op a b c >>= f  = Op a (b >>= f) (c >>= f)

lam :: Eq a => a -> Exp a -> Exp a
lam v b = Lam (abstract1 v b)

isZero :: Exp a -> Bool
isZero (Num 0) = True
isZero _       = False

data Err = DivideByZero
         | NotFound Name
         | BadArgs Bin String String
         | BadApp String String
           deriving Show

type ID = Int

type Env = Map Name ID

type Store e = Map ID (Exp e)

newtype Interp e a = Interp { runInterp :: ReaderT Env (ExceptT Err (State (Store e))) a }
  deriving (Functor, Applicative, Monad, MonadState (Store e), MonadError Err, MonadReader Env)

execInterp :: Interp e a -> (Either Err a, Store e)
execInterp = flip runState mempty . runExceptT . flip runReaderT mempty . runInterp

find :: Name -> Env -> Interp e (Exp e)
find n e = do
  st <- get
  case M.lookup n e >>= flip M.lookup st of
    Nothing -> throwError (NotFound n)
    Just f  -> return f

delta :: Show a => Bin -> Exp a -> Exp a -> Interp e Int
delta Div _ (Num 0)      = throwError DivideByZero
delta op (Num a) (Num b) = pure $ case op of
  Add -> a + b
  Sub -> a - b
  Mul -> a * b
  Div -> a `div` b
delta op a b = throwError (BadArgs op (show a) (show b))

eval1 :: (Exp Name -> Interp Name (Exp Name)) -> Exp Name -> Interp Name (Exp Name)
eval1 go e = case e of
  Num x -> return (Num x)
  Var n -> ask >>= find n >>= go
  If cond onTrue onFalse -> do
    v <- go cond
    let z = isZero v
    go (if z then onTrue else onFalse)
  Op o left right -> do
    l <- go left
    r <- go right
    Num <$> delta o l r
  Rec n a -> do
    pos <- alloc n
    let augment = M.insert n pos
    res <- local augment (go a)
    modify (M.insert pos (fmap show res))
    return res
  App f a -> do
    arg <- go a
    fun <- go f
    case fun of
      Lam b -> go (instantiate1 arg b)
      other -> throwError (BadApp (show f) (show other))
  Lam b -> return $ Lam b


terms :: Exp Name
terms = App (lam "x" (Var "x")) (Num 1)

main :: IO ()
main = do
  print (terms :: Exp Name)
  print (execInterp ((fix eval1) terms))




-- data Result a = Result Env a
--   deriving (Show, Functor)

-- extract :: Result a -> a
-- extract (Result _ a) = a

-- result :: MonadReader Env m => a -> m (Result a)
-- result x = Result <$> ask <*> pure x

alloc :: Name -> Interp e Int
alloc _ = M.size <$> get

-- eval :: (Exp -> Interp (Result Exp)) -> Exp -> Interp (Result Exp)
-- eval go e = case e of
--   Num n -> result (Num n)
--   Var v -> ask >>= find v >>= result
--   If condE trueE falseE -> do
--     v <- go condE
--     let z = isZero (extract v)
--     go (if z then trueE else falseE)
--   Op o leftE rightE -> do
--     v0 <- go leftE
--     v1 <- go rightE
--     delta o (extract v0) (extract v1) >>= result
--   Rec f e' -> do
--     a <- alloc f
--     v <- local (M.insert f a) (go e')
--     modify (M.insert a (extract v))
--     return v
-- --   Lam x e' -> do
-- --     p <- ask
-- --     return _cons
-- --  App _f _e' -> do

--     -- v1 <- go e'
--     -- a <- alloc f
--     -- modify (M.insert f (extract v1))
--     -- local (M.insert f a) (go _other)
