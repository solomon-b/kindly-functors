{-# OPTIONS_GHC -Wno-orphans #-}

-- | Work in progress. Provides an example that is fully Contravariant
-- and one that is Inv, Contra, and Co.
module Kindly.Trifunctor where

--------------------------------------------------------------------------------

import Control.Applicative qualified as Hask
import Control.Category
import Control.Monad qualified as Hask
import Control.Monad.Reader qualified as Hask
import Control.Monad.State qualified as Hask
import Data.Function (($))
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import Kindly.Class
import Kindly.Functor
import Kindly.Bifunctor ()
import Kindly.Iso

--------------------------------------------------------------------------------

type Trifunctor :: (Type -> Type -> Type -> Type) -> Constraint
type Trifunctor = FunctorOf (->) ((->) ~> (->) ~> (->))

instance Functor (,,) where
  type Dom (,,) = (->)
  type Cod (,,) = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,) a) ((,,) b)
  map f = Nat (Nat (\(x, y, z) -> (f x, y, z)))

tripleFirst :: (a -> b) -> (a, x, y) -> (b, x, y)
tripleFirst f = let (Nat (Nat f')) = map f in f'

tripleSecond :: (a -> b) -> (x, a, z) -> (x, b, z)
tripleSecond = runNat . map

tripleThird :: (a -> b) -> (x, y, a) -> (x, y, b)
tripleThird = map

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
