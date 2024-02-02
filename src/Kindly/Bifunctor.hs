{-# OPTIONS_GHC -Wno-orphans #-}

module Kindly.Bifunctor where

--------------------------------------------------------------------------------

import Control.Category
import Control.Monad qualified as Hask
import Data.Bifunctor qualified as Hask
import Data.Either (Either)
import Data.Functor.Const (Const)
import Data.Kind (Type)
import Data.Semigroup (Arg)
import GHC.Generics (K1)
import Kindly.Class (Functor (..), FunctorOf, Nat (Nat), runNat, type (~>))
import Kindly.Functor ()

--------------------------------------------------------------------------------

newtype FromBifunctor f a b = FromBifunctor (f a b)
  deriving newtype (Hask.Functor, Hask.Bifunctor)

instance (Hask.Bifunctor p, FunctorOf (->) (->) (p x)) => Functor (FromBifunctor p x) where
  type Dom (FromBifunctor p x) = (->)
  type Cod (FromBifunctor p x) = (->)

  map :: (a -> b) -> FromBifunctor p x a -> FromBifunctor p x b
  map f (FromBifunctor pab) = FromBifunctor (map f pab)

instance (Hask.Bifunctor p, forall x. FunctorOf (->) (->) (p x)) => Functor (FromBifunctor p) where
  type Dom (FromBifunctor p) = (->)
  type Cod (FromBifunctor p) = (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->)) (FromBifunctor p a) (FromBifunctor p b)
  map f = Nat (\(FromBifunctor pax) -> FromBifunctor (Hask.first f pax))

first :: (FunctorOf (->) ((->) ~> (->)) p) => (a -> b) -> p a x -> p b x
first = runNat . map

second :: (FunctorOf (->) (->) (p x)) => (a -> b) -> p x a -> p x b
second = map

bimap :: (FunctorOf (->) (->) (p a), FunctorOf (->) ((->) ~> (->)) p) => (a -> b) -> (c -> d) -> p a c -> p b d
bimap f g = first f . second g

--------------------------------------------------------------------------------

deriving via (FromBifunctor (,)) instance Functor (,)

deriving via (FromBifunctor ((,,) a)) instance Functor ((,,) a)

deriving via (FromBifunctor ((,,,) a b)) instance Functor ((,,,) a b)

deriving via (FromBifunctor ((,,,,) a b c)) instance Functor ((,,,,) a b c)

deriving via (FromBifunctor ((,,,,,) a b c d)) instance Functor ((,,,,,) a b c d)

deriving via (FromBifunctor ((,,,,,,) a b c d e)) instance Functor ((,,,,,,) a b c d e)

deriving via (FromBifunctor Either) instance Functor Either

deriving via (FromBifunctor Arg) instance Functor Arg

deriving via (FromBifunctor (Const :: Type -> Type -> Type)) instance Functor (Const :: Type -> Type -> Type)

deriving via (FromBifunctor (K1 i :: Type -> Type -> Type)) instance Functor (K1 i :: Type -> Type -> Type)
