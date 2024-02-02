{-# OPTIONS_GHC -Wno-orphans #-}
-- |
module Kindly.Contravariant where

--------------------------------------------------------------------------------

import Control.Category
import Data.Functor.Contravariant (Op (..), Predicate (..))
import Data.Functor.Contravariant qualified as Hask
import Kindly.Class

--------------------------------------------------------------------------------

newtype FromContra f a = FromContra {getContra :: f a}
  deriving newtype (Hask.Contravariant)

instance (Hask.Contravariant f) => Functor (FromContra f) where
  type Dom (FromContra f) = Op
  type Cod (FromContra f) = (->)

  map :: Dom (FromContra f) a b -> Cod (FromContra f) ((FromContra f) a) ((FromContra f) b)
  map = Hask.contramap . getOp

contramap :: (FunctorOf Op (->) f) => (a -> b) -> f b -> f a
contramap = map . Op

deriving via (FromContra Predicate) instance Functor Predicate
