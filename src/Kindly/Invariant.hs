{-# OPTIONS_GHC -Wno-orphans #-}
module Kindly.Invariant where

--------------------------------------------------------------------------------

import Control.Category
import Data.Monoid (Endo (..))
import Kindly.Class
import Kindly.Iso

--------------------------------------------------------------------------------

instance Functor Endo where
  type Dom Endo = (<->)
  type Cod Endo = (->)

  map :: (a <-> b) -> Endo a -> Endo b
  map Iso {..} (Endo f) = Endo (fwd . f . bwd)

invmap :: (FunctorOf (<->) (->) f) => (a -> b) -> (b -> a) -> f a -> f b
invmap f g = map (Iso f g)
