{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Work in progress. Provides an example that is fully Contravariant
-- and one that is Inv, Contra, and Co.
module Kindly.Trifunctor
  ( Trifunctor,
    trimap,
  )
where

--------------------------------------------------------------------------------

import Control.Category
import Data.Kind (Constraint, Type)
import Kindly.Bifunctor ()
import Kindly.Class

--------------------------------------------------------------------------------

type Trifunctor :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type -> Type) -> Constraint
type Trifunctor cat1 cat2 cat3 p = (MapArg3 cat3 cat2 cat1 p, forall x. MapArg2 cat2 cat1 (p x), forall x y. MapArg1 cat1 (p x y))

trimap :: forall cat1 cat2 cat3 p. (Trifunctor cat1 cat2 cat3 p) => forall a b c a' b' c'. (a `cat3` a') -> (b `cat2` b') -> (c `cat1` c') -> p a b c -> p a' b' c'
trimap f g h = map3 f . map2 @_ @cat1 g . map1 h

--------------------------------------------------------------------------------

instance CategoricalFunctor (,,) where
  type Dom (,,) = (->)
  type Cod (,,) = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,) a) ((,,) b)
  map f = Nat (Nat (\(x, y, z) -> (f x, y, z)))

instance CategoricalFunctor ((,,,) x) where
  type Dom ((,,,) x) = (->)
  type Cod ((,,,) x) = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,,) x a) ((,,,) x b)
  map f = Nat (Nat (\(a, b, c, d) -> (a, f b, c, d)))

instance CategoricalFunctor ((,,,,) x x') where
  type Dom ((,,,,) x x') = (->)
  type Cod ((,,,,) x x') = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,,,) x x' a) ((,,,,) x x' b)
  map f = Nat (Nat (\(a, b, c, d, e) -> (a, b, f c, d, e)))

instance CategoricalFunctor ((,,,,,) x x' x'') where
  type Dom ((,,,,,) x x' x'') = (->)
  type Cod ((,,,,,) x x' x'') = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,,,,) x x' x'' a) ((,,,,,) x x' x'' b)
  map f' = Nat (Nat (\(a, b, c, d, e, f) -> (a, b, c, f' d, e, f)))

instance CategoricalFunctor ((,,,,,,) x x' x'' x''') where
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
