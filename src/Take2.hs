{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Take2 where

import Data.Kind (Type)

infix 3 .<-

class (Monad m) => Staged m where
  data Val m :: Type -> Type
  data Var m :: Type -> Type

  var :: Val m a -> m (Var m a)
  val :: Var m a -> Val m a

class (Staged m) => MutableVars m where
  (.<-) :: Var m a -> Val m a -> m ()

class (Staged m) => StaticArrays m arr where
  alloc :: Int -> Val m a -> m (Val m (arr m a))
  len :: Val m (arr m a) -> m Int -- we have access to the length of the array at staging-time
  setIndex :: Int -> Var m (arr m a) -> Val m a -> m () -- we can statically bounds check

class (Staged m) => Category m ob hom | ob -> hom, hom -> ob where
  dom :: Val m hom -> ob -- we can look up dom/codom at *staging time*
  codom :: Val m hom -> ob
  compose :: Val m hom -> Val m hom -> Maybe (Val m hom)
  id :: Val m ob -> Val m hom

prog1 :: (MutableVars m, Num (Val m Double)) => m (Val m Double)
prog1 = do
  x <- var 2
  x .<- val x + 1
  return $ val x
