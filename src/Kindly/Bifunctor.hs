{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Two Parameter Functors of arbitrary categories.
module Kindly.Bifunctor
  ( Bifunctor,
    bimap,
    lmap,
    rmap,
  )
where

--------------------------------------------------------------------------------

import Control.Category
import Data.Bifunctor qualified as Hask
import Data.Either (Either)
import Data.Function (flip)
import Data.Functor qualified as Hask
import Data.Functor.Const (Const)
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import Data.Profunctor qualified as Hask
import Data.Semigroup qualified as Semigroup
import Data.These (These)
import GHC.Generics (K1)
import Kindly.Class
import Kindly.Functor ()

--------------------------------------------------------------------------------

-- | A 'CategoricalFunctor' of kind @Type -> Type@ mapping from an
-- arbitrary category @cat1@ to a functor category @cat2 ~> (->)@.
type Bifunctor :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type) -> Constraint
type Bifunctor cat1 cat2 p = (MapArg2 cat1 cat2 p, forall x. MapArg1 cat2 (p x))

-- | Lift a morphism @cat1 a a'@ and a morphism @cat2 b b'@ into a
-- function @p a b -> p a' b'@.
bimap :: forall cat1 cat2 p. (Bifunctor cat1 cat2 p) => forall a b a' b'. (a `cat1` a') -> (b `cat2` b') -> p a b -> p a' b'
bimap f g = map2 f . map1 g

-- | Lift a morphism @cat1 a b@ into a function @p a x -> p b x@.
lmap :: (Category cat2, Bifunctor cat1 cat2 p) => (a `cat1` b) -> p a x -> p b x
lmap = flip bimap id

-- | Lift a morphism @cat2 a b@ into a function @p x a -> p x b@.
rmap :: (Bifunctor cat1 cat2 p) => (a `cat2` b) -> p x a -> p x b
rmap = bimap id

--------------------------------------------------------------------------------

newtype FromBifunctor f a b = FromBifunctor (f a b)
  deriving newtype (Hask.Functor, Hask.Bifunctor)

instance (Hask.Bifunctor p, FunctorOf (->) (->) (p x)) => CategoricalFunctor (FromBifunctor p x) where
  type Dom (FromBifunctor p x) = (->)
  type Cod (FromBifunctor p x) = (->)

  map :: (a -> b) -> FromBifunctor p x a -> FromBifunctor p x b
  map f (FromBifunctor pab) = FromBifunctor (map f pab)

instance (Hask.Bifunctor p, forall x. FunctorOf (->) (->) (p x)) => CategoricalFunctor (FromBifunctor p) where
  type Dom (FromBifunctor p) = (->)
  type Cod (FromBifunctor p) = (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->)) (FromBifunctor p a) (FromBifunctor p b)
  map f = Nat (\(FromBifunctor pax) -> FromBifunctor (Hask.first f pax))

--------------------------------------------------------------------------------
-- Covariant (Bi)Functor instances

deriving via (FromBifunctor (,)) instance CategoricalFunctor (,)

deriving via (FromBifunctor ((,,) a)) instance CategoricalFunctor ((,,) a)

deriving via (FromBifunctor ((,,,) a b)) instance CategoricalFunctor ((,,,) a b)

deriving via (FromBifunctor ((,,,,) a b c)) instance CategoricalFunctor ((,,,,) a b c)

deriving via (FromBifunctor ((,,,,,) a b c d)) instance CategoricalFunctor ((,,,,,) a b c d)

deriving via (FromBifunctor ((,,,,,,) a b c d e)) instance CategoricalFunctor ((,,,,,,) a b c d e)

deriving via (FromBifunctor Either) instance CategoricalFunctor Either

deriving via (FromBifunctor These) instance CategoricalFunctor These

deriving via (FromBifunctor Semigroup.Arg) instance CategoricalFunctor Semigroup.Arg

deriving via (FromBifunctor (Const :: Type -> Type -> Type)) instance CategoricalFunctor (Const :: Type -> Type -> Type)

deriving via (FromBifunctor (K1 i :: Type -> Type -> Type)) instance CategoricalFunctor (K1 i :: Type -> Type -> Type)

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

instance MapArg2 (->) (->) Semigroup.Arg

instance MapArg2 (->) (->) (Const :: Type -> Type -> Type)

instance MapArg2 (->) (->) (K1 i :: Type -> Type -> Type)

--------------------------------------------------------------------------------

newtype FromProfunctor f a b = FromProfunctor (f a b)
  deriving newtype (Hask.Functor, Hask.Profunctor)

instance (Hask.Profunctor p, FunctorOf (->) (->) (p x)) => CategoricalFunctor (FromProfunctor p x) where
  type Dom (FromProfunctor p x) = (->)
  type Cod (FromProfunctor p x) = (->)

  map :: (a -> b) -> Cod (FromProfunctor p x) (FromProfunctor p x a) (FromProfunctor p x b)
  map f (FromProfunctor pxa) = FromProfunctor (map f pxa)

instance (Hask.Profunctor p) => CategoricalFunctor (FromProfunctor p) where
  type Dom (FromProfunctor p) = Op
  type Cod (FromProfunctor p) = (->) ~> (->)

  map :: Op a b -> ((->) ~> (->)) ((FromProfunctor p) a) ((FromProfunctor p) b)
  map (Op f) = Nat (\(FromProfunctor pax) -> FromProfunctor (Hask.lmap f pax))

--------------------------------------------------------------------------------
-- Profunctorial Functor instances

deriving via (FromProfunctor (->)) instance CategoricalFunctor (->)

-- TODO: Add remaining Profunctor instances

--------------------------------------------------------------------------------
-- Profunctorial MapArg2 instances

instance MapArg2 Op (->) (->)

-- TODO: Add remaining Profunctor instances
