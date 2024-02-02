{-# OPTIONS_GHC -Wno-orphans #-}
module Kindly.Profunctor where

--------------------------------------------------------------------------------

import Control.Category
import Control.Monad qualified as Hask
import Data.Functor.Contravariant (Op (..))
import Data.Profunctor qualified as Hask
import Kindly.Class

--------------------------------------------------------------------------------

newtype FromProfunctor f a b = FromProfunctor (f a b)
  deriving newtype (Hask.Functor, Hask.Profunctor)

instance (Hask.Profunctor p, FunctorOf (->) (->) (p x)) => Functor (FromProfunctor p x) where
  type Dom (FromProfunctor p x) = (->)
  type Cod (FromProfunctor p x) = (->)

  map :: (a -> b) -> Cod (FromProfunctor p x) (FromProfunctor p x a) (FromProfunctor p x b)
  map f (FromProfunctor pxa) = FromProfunctor (map f pxa)

instance (Hask.Profunctor p) => Functor (FromProfunctor p) where
  type Dom (FromProfunctor p) = Op
  type Cod (FromProfunctor p) = (->) ~> (->)

  map :: Op a b -> ((->) ~> (->)) ((FromProfunctor p) a) ((FromProfunctor p) b)
  map (Op f) = Nat (\(FromProfunctor pax) -> FromProfunctor (Hask.lmap f pax))

lmap :: (FunctorOf Op ((->) ~> (->)) p) => (a -> b) -> p b x -> p a x
lmap = runNat . map . Op

rmap :: (FunctorOf (->) (->) (f x)) => (a -> b) -> f x a -> f x b
rmap = map

dimap :: (FunctorOf Op ((->) ~> (->)) p, forall x. FunctorOf (->) (->) (p x)) => (a -> b) -> (c -> d) -> p b c -> p a d
dimap f g = lmap f . rmap g

deriving via (FromProfunctor (->)) instance Functor (->)
