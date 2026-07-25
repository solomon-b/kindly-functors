{-# LANGUAGE ImpredicativeTypes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Three Parameter Functors of arbitrary varience.
module Kindly.Trifunctor
  ( Trifunctor,
    trimap,
    trimapIso,
    Iso (..),
  )
where

--------------------------------------------------------------------------------

import Control.Category
import Data.Functor.Contravariant (Op)
import Data.Isomorphism (Iso (..))
import Data.Kind (Constraint, Type)
import Data.Profunctor (Forget (..))
import GHC.Generics (K1 (..))
import Kindly.Bifunctor ()
import Kindly.Class

--------------------------------------------------------------------------------

-- | A 'CategoricalFunctor' of kind @Type -> Type -> Type@ mapping from an
-- arbitrary category @cat1@ to a functor category @cat2 ~> cat3 ~> (->)@.
type Trifunctor :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type -> Type) -> Constraint
type Trifunctor cat1 cat2 cat3 p = (MapArg3 cat3 cat2 cat1 p, forall x. MapArg2 cat2 cat1 (p x), forall x y. MapArg1 cat1 (p x y))

-- | Lift a morphism @cat1 a a'@, a morphism @cat2 b b'@, and a morphism @cat3 c c'@ into a -- function @p a b c -> p a' b' c'@.
trimap :: forall cat1 cat2 cat3 p. (Trifunctor cat1 cat2 cat3 p) => forall a b c a' b' c'. (a `cat3` a') -> (b `cat2` b') -> (c `cat1` c') -> p a b c -> p a' b' c'
trimap f g h = map3 f . map2 @_ @cat1 g . map1 h

-- | Map a @('->')@ isomorphism through each position of a 'Trifunctor',
-- regardless of that position's variance. A trifunctor can always transport an
-- isomorphism in any argument, so 'liftIso' reflects each iso into that
-- position's category and drops whichever leg the category ignores. The isos
-- map the type arguments left-to-right: the first maps the first argument, the
-- second the second, the third the third.
--
-- 'trimapIso' is to 'trimap' what 'Kindly.Functor.mapIso' is to
-- 'Kindly.Functor.fmap'.
trimapIso ::
  (Trifunctor cat1 cat2 cat3 p, LiftIso cat1, LiftIso cat2, LiftIso cat3) =>
  Iso (->) a a' ->
  Iso (->) b b' ->
  Iso (->) c c' ->
  p a b c ->
  p a' b' c'
trimapIso i j k = trimap (liftIso i) (liftIso j) (liftIso k)

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

instance CategoricalFunctor (K1 :: Type -> Type -> Type -> Type) where
  type Dom K1 = (->)
  type Cod K1 = (->) ~> (->) ~> (->)

  map _ = Nat (Nat (\(K1 c) -> K1 c))

instance CategoricalFunctor (Forget :: Type -> Type -> Type -> Type) where
  type Dom Forget = (->)
  type Cod Forget = Op ~> (->) ~> (->)

  map f = Nat (Nat (\(Forget g) -> Forget (f . g)))

instance CategoricalFunctor ((,,,,,,) x x' x'' x''') where
  type Dom ((,,,,,,) x x' x'' x''') = (->)
  type Cod ((,,,,,,) x x' x'' x''') = (->) ~> (->) ~> (->)

  map :: (a -> b) -> ((->) ~> (->) ~> (->)) ((,,,,,,) x x' x'' x''' a) ((,,,,,,) x x' x'' x''' b)
  map f' = Nat (Nat (\(a, b, c, d, e, f, g) -> (a, b, c, d, f' e, f, g)))
