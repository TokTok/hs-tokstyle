{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE Safe          #-}
module Result where

import Control.DeepSeq(NFData)
import Control.Applicative(Applicative(..))


data Result a
  = Success a
  | Failure String
  deriving (Read, Show, Eq, Functor)

instance NFData a => NFData (Result a)


instance Applicative Result where
  pure = Success

  Success f   <*> x = fmap f x
  Failure msg <*> _ = Failure msg


instance Monad Result where
  return = pure
  fail = Failure

  Success x   >>= f = f x
  Failure msg >>= _ = Failure msg
