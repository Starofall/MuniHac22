{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE RankNTypes #-}

module LovecraftLetter.Operational where

import Control.Monad(ap, (>=>))

data Program m a where
  Done :: a -> Program m a
  (:>>=) :: m a -> (a -> Program m b) -> Program m b

deriving instance Functor (Program m)

perform :: m a -> Program m a
perform action = action :>>= Done

instance Applicative (Program m) where
  pure = Done
  (<*>) = ap

instance Monad (Program m) where
  return = Done
  Done x >>= k = k x
  (x :>>= k1) >>= k2 = x :>>= (k1 >=> k2)

interpret :: Monad m => (forall x. i x -> m x) -> Program i a -> m a
interpret f = go
  where
    go (Done x) = return x
    go (action :>>= k) = f action >>= go . k
