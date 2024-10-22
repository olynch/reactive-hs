{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module MyLib where

import Control.Monad.State (MonadState (get), State, evalState, modify)
import Control.Monad.Trans.Class
import Data.Kind (Type)

newtype V = MkV Int
  deriving (Eq, Show)

type ℝ = Double

data Tp = TR | TB

data Tm :: Tp -> Type where
  Var :: V -> Tm a
  Const :: ℝ -> Tm TR
  Add :: Tm TR -> Tm TR -> Tm TR
  Sub :: Tm TR -> Tm TR -> Tm TR
  If :: Tm TB -> Tm a -> Tm a -> Tm aa
  TTrue :: Tm TB
  TFalse :: Tm TB
  Eq :: Tm a -> Tm a -> Tm TB
  Let :: V -> Tm a -> Tm b -> Tm b

instance Show (Tm a) where
  show (Var (MkV i)) = "#" ++ show i
  show (Const x) = show x
  show (Add t1 t2) = "(" ++ show t1 ++ " + " ++ show t2 ++ ")"
  show (Sub t1 t2) = "(" ++ show t1 ++ " - " ++ show t2 ++ ")"
  show (If cond t1 t2) = "if " ++ show cond ++ " then " ++ show t1 ++ " else " ++ show t2
  show TTrue = "true"
  show TFalse = "false"
  show (Eq t1 t2) = "(" ++ show t1 ++ " == " ++ show t2 ++ ")"
  show (Let v t body) = "let " ++ show (Var v) ++ " = " ++ show t ++ " in " ++ show body

newtype Staged m a = MkStaged {callCC :: forall s. (a -> m (Tm s)) -> m (Tm s)}
  deriving (Functor)

instance (Applicative m) => Applicative (Staged m) where
  pure x = MkStaged $ \f -> f x
  (MkStaged sf) <*> (MkStaged sx) = MkStaged $ \k -> sf (\f -> sx (k . f))

instance (Monad m) => Monad (Staged m) where
  (MkStaged sx) >>= f = MkStaged $ \k -> sx (\x -> callCC (f x) k)

instance MonadTrans Staged where
  lift mx = MkStaged $ \k -> mx >>= k

class (Monad m) => MonadFresh m where
  fresh :: m V

instance MonadFresh (State Int) where
  fresh = do
    i <- get
    modify (+ 1)
    return (MkV i)

newtype Gen a = MkGen (Staged (State Int) a)
  deriving (Functor, Applicative, Monad)

let_ :: Tm a -> Gen (Tm a)
let_ t = MkGen $ MkStaged $ \k -> do
  v <- fresh
  body <- k (Var v)
  return (Let v t body)

produce :: Gen (Tm a) -> Tm a
produce (MkGen (MkStaged f)) = evalState (f pure) 0
