{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Work in progress. Provides an example that is fully Contravariant
-- and one that is Inv, Contra, and Co.
module Kindly.Trifunctor
  ( Trifunctor,
    trimap,
    MealyM (..),
  )
where

--------------------------------------------------------------------------------

import Control.Applicative qualified as Hask
import Control.Category
import Control.Monad qualified as Hask
import Control.Monad.Reader qualified as Hask
import Control.Monad.State qualified as Hask
import Data.Function (($))
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import Kindly.Bifunctor ()
import Kindly.Class
import Kindly.Functor
import Kindly.Iso

--------------------------------------------------------------------------------

type Trifunctor :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type -> Type) -> Constraint
type Trifunctor cat1 cat2 cat3 p = (MapArg3 cat3 cat2 cat1 p, forall x. MapArg2 cat2 cat1 (p x), forall x y. MapArg1 cat1 (p x y))

trimap :: forall cat1 cat2 cat3 p. (Trifunctor cat1 cat2 cat3 p) => forall a b c a' b' c'. (a `cat3` a') -> (b `cat2` b') -> (c `cat1` c') -> p a b c -> p a' b' c'
trimap f g h = map3 f . map2 @_ @cat1 g . map1 h

--------------------------------------------------------------------------------

instance Functor (,,) where
  type Dom (,,) = (->)
  type Cod (,,) = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,) a) ((,,) b)
  map f = Nat (Nat (\(x, y, z) -> (f x, y, z)))

instance Functor ((,,,) x) where
  type Dom ((,,,) x) = (->)
  type Cod ((,,,) x) = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,,) x a) ((,,,) x b)
  map f = Nat (Nat (\(a, b, c, d) -> (a, f b, c, d)))

instance Functor ((,,,,) x x') where
  type Dom ((,,,,) x x') = (->)
  type Cod ((,,,,) x x') = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,,,) x x' a) ((,,,,) x x' b)
  map f = Nat (Nat (\(a, b, c, d, e) -> (a, b, f c, d, e)))

instance Functor ((,,,,,) x x' x'') where
  type Dom ((,,,,,) x x' x'') = (->)
  type Cod ((,,,,,) x x' x'') = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,,,,) x x' x'' a) ((,,,,,) x x' x'' b)
  map f' = Nat (Nat (\(a, b, c, d, e, f) -> (a, b, c, f' d, e, f)))

instance Functor ((,,,,,,) x x' x'' x''') where
  type Dom ((,,,,,,) x x' x'' x''') = (->)
  type Cod ((,,,,,,) x x' x'' x''') = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,,,,,) x x' x'' x''' a) ((,,,,,,) x x' x'' x''' b)
  map f' = Nat (Nat (\(a, b, c, d, e, f, g) -> (a, b, c, d, f' e, f, g)))

--------------------------------------------------------------------------------

instance MapArg3 (->) (->) (->) (,,)

instance MapArg3 (->) (->) (->) ((,,,) x)

instance MapArg3 (->) (->) (->) ((,,,,) x x')

instance MapArg3 (->) (->) (->) ((,,,,,) x x' x'')

instance MapArg3 (->) (->) (->) ((,,,,,,) x x' x'' x''')

--------------------------------------------------------------------------------

newtype MealyM m s i o = MealyM {runMealyM :: s -> i -> m (o, s)}
  deriving
    (Hask.Functor, Hask.Applicative, Hask.Monad)
    via Hask.StateT s (Hask.ReaderT i m)

deriving via (FromFunctor (MealyM m s i)) instance (Hask.Functor m) => Functor (MealyM m s i)

instance (FunctorOf (->) (->) m) => Functor (MealyM m) where
  type Dom (MealyM m) = (<->)
  type Cod (MealyM m) = Nat Op ((->) ~> (->))

  map :: (a <-> b) -> Nat Op ((->) ~> (->)) (MealyM m a) (MealyM m b)
  map Iso {..} = Nat $ Nat $ \(MealyM mealy) -> MealyM $ \s i -> map (map fwd) $ mealy (bwd s) i

instance Functor (MealyM m s) where
  type Dom (MealyM m s) = Op
  type Cod (MealyM m s) = (->) ~> (->)

  map :: Op a b -> Nat (->) (->) (MealyM m s a) (MealyM m s b)
  map (Op f) = Nat $ \(MealyM mealy) -> MealyM $ \s -> mealy s . f

instance (Hask.Functor m) => MapArg1 (->) (MealyM m s i)

-- instance MapArg2 Op (->) (MealyM m s)

instance (FunctorOf (->) (->) m) => MapArg3 (<->) Op (->) (MealyM m)
