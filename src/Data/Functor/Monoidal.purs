module Data.Functor.Monoidal where

import Prelude

import Data.Tuple (Tuple)

class Semigroupal f where
  fproduct :: ∀ a b. f a -> f b -> f (Tuple a b)

class Semigroupal f <= Monoidal f where
  funit :: f Unit
