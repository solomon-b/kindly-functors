{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Kindly.Bifunctor where

--------------------------------------------------------------------------------

import Control.Category
import Control.Monad qualified as Hask
import Data.Bifunctor qualified as Hask
import Data.Either (Either)
import Data.Function (flip)
import Data.Functor.Const (Const)
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import Data.Profunctor qualified as Hask
import Data.Semigroup (Arg)
import Data.These (These)
import GHC.Generics (K1)
import Kindly.Class
import Kindly.Functor ()

--------------------------------------------------------------------------------

type Bifunctor :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type) -> Constraint
type Bifunctor cat2 cat1 p = (MapArg2 cat2 cat1 p, forall x. MapArg1 cat1 (p x))

bimap :: forall cat2 cat1 p. (Bifunctor cat2 cat1 p) => forall a b a' b'. (a `cat2` a') -> (b `cat1` b') -> p a b -> p a' b'
bimap f g = map2 f . map1 g

lmap :: (Category cat1, Bifunctor cat2 cat1 p) => (a `cat2` b) -> p a x -> p b x
lmap = flip bimap id

rmap :: (Bifunctor cat2 cat1 p) => (a `cat1` b) -> p x a -> p x b
rmap = bimap id

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

--------------------------------------------------------------------------------
-- Covariant (Bi)Functor instances

deriving via (FromBifunctor (,)) instance Functor (,)

deriving via (FromBifunctor ((,,) a)) instance Functor ((,,) a)

deriving via (FromBifunctor ((,,,) a b)) instance Functor ((,,,) a b)

deriving via (FromBifunctor ((,,,,) a b c)) instance Functor ((,,,,) a b c)

deriving via (FromBifunctor ((,,,,,) a b c d)) instance Functor ((,,,,,) a b c d)

deriving via (FromBifunctor ((,,,,,,) a b c d e)) instance Functor ((,,,,,,) a b c d e)

deriving via (FromBifunctor Either) instance Functor Either

deriving via (FromBifunctor These) instance Functor These

deriving via (FromBifunctor Arg) instance Functor Arg

deriving via (FromBifunctor (Const :: Type -> Type -> Type)) instance Functor (Const :: Type -> Type -> Type)

deriving via (FromBifunctor (K1 i :: Type -> Type -> Type)) instance Functor (K1 i :: Type -> Type -> Type)

--------------------------------------------------------------------------------
-- Covariant MapArg2 instances

instance MapArg2 (->) (->) (,)

instance MapArg2 (->) (->) ((,,) a)

instance MapArg2 (->) (->) ((,,,) a b)

instance MapArg2 (->) (->) ((,,,,) a b c)

instance MapArg2 (->) (->) ((,,,,,) a b c d)

instance MapArg2 (->) (->) ((,,,,,,) a b c d e)

instance MapArg2 (->) (->) Either

-- instance MapArg2 (->) (->) These

instance MapArg2 (->) (->) Arg

instance MapArg2 (->) (->) (Const :: Type -> Type -> Type)

instance MapArg2 (->) (->) (K1 i :: Type -> Type -> Type)

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

--------------------------------------------------------------------------------
-- Profunctorial Functor instances

deriving via (FromProfunctor (->)) instance Functor (->)

-- TODO: Add remaining Profunctor instances

--------------------------------------------------------------------------------
-- Profunctorial MapArg2 instances

instance MapArg2 Op (->) (->)

-- TODO: Add remaining Profunctor instances
