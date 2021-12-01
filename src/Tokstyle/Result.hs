{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE Safe          #-}
{-# LANGUAGE StrictData    #-}
module Tokstyle.Result where

import           Control.DeepSeq (NFData)
import           GHC.Generics    (Generic)


data Result a
  = Success a
  | Failure String
  deriving (Read, Show, Eq, Functor, Generic)

instance NFData a => NFData (Result a)


instance Applicative Result where
  pure = Success

  Success f   <*> x = fmap f x
  Failure msg <*> _ = Failure msg


instance Monad Result where
  return = pure

  Success x   >>= f = f x
  Failure msg >>= _ = Failure msg


instance MonadFail Result where
  fail = Failure
