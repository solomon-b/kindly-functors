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

import Control.Applicative (WrappedArrow (..))
import Control.Arrow (Arrow, Kleisli (..), arr)
import Control.Category
import Data.Bifunctor qualified as Hask
import Data.Bifunctor.Biff (Biff (..))
import Data.Bifunctor.Clown (Clown (..))
import Data.Bifunctor.Flip (Flip (..))
import Data.Bifunctor.Joker (Joker (..))
import Data.Bifunctor.Product (Product (..))
import Data.Bifunctor.Sum (Sum (..))
import Data.Bifunctor.Tannen (Tannen (..))
import Data.Bifunctor.Wrapped (WrappedBifunctor (..))
import Data.Either (Either)
import Data.Function (flip)
import Data.Functor qualified as Hask
import Data.Functor.Const (Const)
import Data.Functor.Contravariant (Op (..))
import Data.Kind (Constraint, Type)
import Data.Profunctor qualified as Hask
import Data.Profunctor.Cayley qualified as Hask
import Data.Profunctor.Choice qualified as Hask
import Data.Profunctor.Closed qualified as Hask
import Data.Profunctor.Composition qualified as Hask
import Data.Profunctor.Mapping qualified as Hask
import Data.Profunctor.Strong qualified as Hask
import Data.Profunctor.Traversing qualified as Hask
import Data.Profunctor.Yoneda qualified as Hask
import Data.Semigroup qualified as Semigroup
import Data.Tagged (Tagged (..))
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

instance (forall x. FunctorOf (->) (->) (p x)) => CategoricalFunctor (Flip p :: Type -> Type -> Type) where
  type Dom (Flip p) = (->)
  type Cod (Flip p) = (->) ~> (->)

  map f = Nat (\(Flip pxa) -> Flip (map f pxa))

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Clown f :: Type -> Type -> Type) where
  type Dom (Clown f) = (->)
  type Cod (Clown f) = (->) ~> (->)

  map f = Nat (\(Clown fa) -> Clown (map f fa))

instance CategoricalFunctor (Joker g :: Type -> Type -> Type) where
  type Dom (Joker g) = (->)
  type Cod (Joker g) = (->) ~> (->)

  map _ = Nat (\(Joker gb) -> Joker gb)

instance (MapArg2 (->) (->) p, MapArg2 (->) (->) q) => CategoricalFunctor (Product p q) where
  type Dom (Product p q) = (->)
  type Cod (Product p q) = (->) ~> (->)

  map f = Nat (\(Pair pab qab) -> Pair (map2 f pab) (map2 f qab))

instance (MapArg2 (->) (->) p, MapArg2 (->) (->) q) => CategoricalFunctor (Sum p q) where
  type Dom (Sum p q) = (->)
  type Cod (Sum p q) = (->) ~> (->)

  map f =
    Nat
      ( \s -> case s of
          L2 pab -> L2 (map2 f pab)
          R2 qab -> R2 (map2 f qab)
      )

instance (FunctorOf (->) (->) f, MapArg2 (->) (->) p) => CategoricalFunctor (Tannen f p) where
  type Dom (Tannen f p) = (->)
  type Cod (Tannen f p) = (->) ~> (->)

  map f = Nat (\(Tannen fp) -> Tannen (map1 (map2 f) fp))

instance (MapArg2 (->) (->) p, FunctorOf (->) (->) f) => CategoricalFunctor (Biff p f g :: Type -> Type -> Type) where
  type Dom (Biff p f g) = (->)
  type Cod (Biff p f g) = (->) ~> (->)

  map f = Nat (\(Biff pfg) -> Biff (map2 (map1 f) pfg))

instance (MapArg2 (->) (->) p) => CategoricalFunctor (WrappedBifunctor p) where
  type Dom (WrappedBifunctor p) = (->)
  type Cod (WrappedBifunctor p) = (->) ~> (->)

  map f = Nat (\(WrapBifunctor pab) -> WrapBifunctor (map2 f pab))

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

instance CategoricalFunctor (Kleisli m) where
  type Dom (Kleisli m) = Op
  type Cod (Kleisli m) = (->) ~> (->)

  map (Op f) = Nat (\(Kleisli g) -> Kleisli (g . f))

instance CategoricalFunctor (Hask.Star f :: Type -> Type -> Type) where
  type Dom (Hask.Star f) = Op
  type Cod (Hask.Star f) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Star g) -> Hask.Star (g . f))

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Hask.Costar f :: Type -> Type -> Type) where
  type Dom (Hask.Costar f) = Op
  type Cod (Hask.Costar f) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Costar g) -> Hask.Costar (g . map f))

instance CategoricalFunctor (Hask.Forget r :: Type -> Type -> Type) where
  type Dom (Hask.Forget r) = Op
  type Cod (Hask.Forget r) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Forget g) -> Hask.Forget (g . f))

instance (Arrow p) => CategoricalFunctor (WrappedArrow p) where
  type Dom (WrappedArrow p) = Op
  type Cod (WrappedArrow p) = (->) ~> (->)

  map (Op f) = Nat (\(WrapArrow g) -> WrapArrow (g . arr f))

instance (MapArg2 Op (->) q) => CategoricalFunctor (Hask.Procompose p q :: Type -> Type -> Type) where
  type Dom (Hask.Procompose p q) = Op
  type Cod (Hask.Procompose p q) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Procompose pxc qdx) -> Hask.Procompose pxc (map2 (Op f) qdx))

instance (MapArg2 Op (->) q) => CategoricalFunctor (Hask.Rift p q :: Type -> Type -> Type) where
  type Dom (Hask.Rift p q) = Op
  type Cod (Hask.Rift p q) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Rift g) -> Hask.Rift (\p -> map2 (Op f) (g p)))

instance CategoricalFunctor (Hask.Yoneda p) where
  type Dom (Hask.Yoneda p) = Op
  type Cod (Hask.Yoneda p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Yoneda g) -> Hask.Yoneda (\l r -> g (f . l) r))

instance CategoricalFunctor (Hask.Coyoneda p) where
  type Dom (Hask.Coyoneda p) = Op
  type Cod (Hask.Coyoneda p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Coyoneda l r p) -> Hask.Coyoneda (l . f) r p)

instance (FunctorOf (->) (->) f, MapArg2 Op (->) p) => CategoricalFunctor (Hask.Cayley f p) where
  type Dom (Hask.Cayley f p) = Op
  type Cod (Hask.Cayley f p) = (->) ~> (->)

  map (Op g) = Nat (\(Hask.Cayley fp) -> Hask.Cayley (map (map2 (Op g)) fp))

instance (MapArg2 Op (->) p) => CategoricalFunctor (Hask.Tambara p) where
  type Dom (Hask.Tambara p) = Op
  type Cod (Hask.Tambara p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Tambara t) -> Hask.Tambara (map2 (Op (\(a, c) -> (f a, c))) t))

instance CategoricalFunctor (Hask.Pastro p) where
  type Dom (Hask.Pastro p) = Op
  type Cod (Hask.Pastro p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Pastro l m r) -> Hask.Pastro l m (r . f))

instance CategoricalFunctor (Hask.Cotambara q) where
  type Dom (Hask.Cotambara q) = Op
  type Cod (Hask.Cotambara q) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Cotambara n r) -> Hask.Cotambara n (Hask.lmap f r))

instance CategoricalFunctor (Hask.Copastro p) where
  type Dom (Hask.Copastro p) = Op
  type Cod (Hask.Copastro p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Copastro g) -> Hask.Copastro (\n -> Hask.lmap f (g n)))

instance (MapArg2 Op (->) p) => CategoricalFunctor (Hask.TambaraSum p) where
  type Dom (Hask.TambaraSum p) = Op
  type Cod (Hask.TambaraSum p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.TambaraSum t) -> Hask.TambaraSum (map2 (Op (map2 f)) t))

instance CategoricalFunctor (Hask.PastroSum p) where
  type Dom (Hask.PastroSum p) = Op
  type Cod (Hask.PastroSum p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.PastroSum l m r) -> Hask.PastroSum l m (r . f))

instance CategoricalFunctor (Hask.CotambaraSum q) where
  type Dom (Hask.CotambaraSum q) = Op
  type Cod (Hask.CotambaraSum q) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.CotambaraSum n r) -> Hask.CotambaraSum n (Hask.lmap f r))

instance CategoricalFunctor (Hask.CopastroSum p) where
  type Dom (Hask.CopastroSum p) = Op
  type Cod (Hask.CopastroSum p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.CopastroSum g) -> Hask.CopastroSum (\n -> Hask.lmap f (g n)))

instance (MapArg2 Op (->) p) => CategoricalFunctor (Hask.Closure p) where
  type Dom (Hask.Closure p) = Op
  type Cod (Hask.Closure p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Closure t) -> Hask.Closure (map2 (Op (f .)) t))

instance CategoricalFunctor (Hask.Environment p) where
  type Dom (Hask.Environment p) = Op
  type Cod (Hask.Environment p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.Environment l m r) -> Hask.Environment l m (r . f))

instance CategoricalFunctor (Hask.FreeTraversing p) where
  type Dom (Hask.FreeTraversing p) = Op
  type Cod (Hask.FreeTraversing p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.FreeTraversing l m r) -> Hask.FreeTraversing l m (r . f))

instance (MapArg2 Op (->) p) => CategoricalFunctor (Hask.CofreeTraversing p) where
  type Dom (Hask.CofreeTraversing p) = Op
  type Cod (Hask.CofreeTraversing p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.CofreeTraversing t) -> Hask.CofreeTraversing (map2 (Op (Hask.fmap f)) t))

instance CategoricalFunctor (Hask.FreeMapping p) where
  type Dom (Hask.FreeMapping p) = Op
  type Cod (Hask.FreeMapping p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.FreeMapping l m r) -> Hask.FreeMapping l m (r . f))

instance (MapArg2 Op (->) p) => CategoricalFunctor (Hask.CofreeMapping p) where
  type Dom (Hask.CofreeMapping p) = Op
  type Cod (Hask.CofreeMapping p) = (->) ~> (->)

  map (Op f) = Nat (\(Hask.CofreeMapping t) -> Hask.CofreeMapping (map2 (Op (Hask.fmap f)) t))

instance CategoricalFunctor (Tagged :: Type -> Type -> Type) where
  type Dom Tagged = Op
  type Cod Tagged = (->) ~> (->)

  map _ = Nat (\(Tagged b) -> Tagged b)

--------------------------------------------------------------------------------
-- Bifunctors into a non-(->) inner category

-- | t'Op' is covariant in its first (result) argument, with contravariant
-- partial applications: @'Bifunctor' (->) 'Op' 'Op'@ holds, which neither
-- 'Hask.Bifunctor' nor 'Hask.Profunctor' can express.
instance CategoricalFunctor Op where
  type Dom Op = (->)
  type Cod Op = Op ~> (->)

  map f = Nat (\(Op g) -> Op (f . g))
