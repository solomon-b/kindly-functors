{-# LANGUAGE CPP #-}

module Kindly.Class where

--------------------------------------------------------------------------------

import Control.Arrow (Kleisli (..))
import Control.Category
import Data.Functor.Contravariant (Op (..))
import Data.Isomorphism (Iso (..))
import Data.Kind (Constraint)
import Data.Profunctor (Star (..))
import Data.Semigroupoid (Semigroupoid (..))
#if MIN_VERSION_base(4,17,0)
-- On GHC 9.4+ (@base >= 4.17@) @~@ is an ordinary type operator rather than
-- built-in syntax, so under @NoImplicitPrelude@ it must be brought into scope.
-- Earlier GHCs still treat @~@ as built-in syntax and do not export it.
import Data.Type.Equality (type (~))
#endif
import GHC.Base (Monad, Type, pure)

--------------------------------------------------------------------------------

-- | A functor @f@ between categories @from@ and @to@ sends objects in
-- @Dom f@ to objects in @Cod f@ and morphisms in @Dom f@ to
-- morphisms in @Dom f@.
--
-- === Laws
--
-- [Identity]    @'map' 'id' == 'id'@
-- [Composition] @'map' (f . g) == 'map' f . 'map' g@
type CategoricalFunctor :: (from -> to) -> Constraint
class (Category (Dom f), Category (Cod f)) => CategoricalFunctor (f :: from -> to) where
  -- | @Dom f@ is the source category for the functor @f@.
  type Dom f :: from -> from -> Type

  -- | @Cod f@ is the target category for the functor @f@.
  type Cod f :: to -> to -> Type

  -- | Lift a function of type @Dom f a b@ into a function of type @Cod f (f a) (f b)@.
  map :: Dom f a b -> Cod f (f a) (f b)

type Cat i = i -> i -> Type

-- | A Natural Transformation betweeen two functors @f@ and @g@.
type Nat :: Cat s -> Cat t -> Cat (s -> t)
newtype Nat source target f g where
  Nat :: (forall x. target (f x) (g x)) -> Nat source target f g

runNat :: Nat source target f g -> (forall x. target (f x) (g x))
runNat (Nat f) = f

infixr 0 ~>

type (~>) c1 c2 = Nat c1 c2

instance (Semigroupoid c1, Semigroupoid c2) => Semigroupoid (Nat c1 c2) where
  o :: Nat c1 c2 j k1 -> Nat c1 c2 i j -> Nat c1 c2 i k1
  Nat c1 `o` Nat c2 = Nat (c1 `o` c2)

instance (Semigroupoid c1, Semigroupoid c2, Category c1, Category c2) => Category (c1 ~> c2) where
  id :: (c1 ~> c2) a a
  id = Nat id

  (.) = o

type FunctorOf :: Cat from -> Cat to -> (from -> to) -> Constraint
class (CategoricalFunctor f, dom ~ Dom f, cod ~ Cod f) => FunctorOf dom cod f

instance (CategoricalFunctor f, dom ~ Dom f, cod ~ Cod f) => FunctorOf dom cod f

--------------------------------------------------------------------------------
-- NOTE: These these classes go from right to left:

class (FunctorOf cat1 (->) p) => MapArg1 cat1 p | p -> cat1 where
  map1 :: (a `cat1` b) -> p a -> p b
  map1 = map

class (FunctorOf cat1 (cat2 ~> (->)) p, forall x. MapArg1 cat2 (p x)) => MapArg2 cat1 cat2 p | p -> cat2 cat2 where
  map2 :: (a `cat1` b) -> forall x. p a x -> p b x
  map2 f = runNat (map @_ @_ @p f)

class (FunctorOf cat1 (cat2 ~> cat3 ~> (->)) p, forall x. MapArg2 cat2 cat3 (p x)) => MapArg3 cat1 cat2 cat3 p | p -> cat1 cat2 cat3 where
  map3 :: (a `cat1` b) -> forall x y. p a x y -> p b x y
  map3 f = runNat (runNat (map @_ @_ @p f))

--------------------------------------------------------------------------------

-- | Every 'CategoricalFunctor' whose codomain is a (nested) functor category
-- ending in @(->)@ is a @MapArgN@ via the default methods. These blanket
-- instances mean a 'CategoricalFunctor' instance never needs a paired @MapArgN@
-- instance. The domain category @cat1@ is recovered from @Dom p@, so one
-- instance covers every variance (covariant @(->)@, contravariant 'Op',
-- invariant @'Data.Isomorphism.Iso' (->)@).
instance (CategoricalFunctor p, Cod p ~ (->), cat1 ~ Dom p) => MapArg1 cat1 p

instance
  (CategoricalFunctor p, Cod p ~ (cat2 ~> (->)), cat1 ~ Dom p, forall x. MapArg1 cat2 (p x)) =>
  MapArg2 cat1 cat2 p

instance
  (CategoricalFunctor p, Cod p ~ (cat2 ~> cat3 ~> (->)), cat1 ~ Dom p, forall x. MapArg2 cat2 cat3 (p x)) =>
  MapArg3 cat1 cat2 cat3 p

--------------------------------------------------------------------------------

-- | Lift a @('->')@ isomorphism into an arbitrary category @cat@. This is the
-- identity-on-objects functor from the @('->')@ core groupoid (embodied by
-- @'Iso' ('->')@) into @cat@. Objects stay put, and an isomorphism becomes a
-- @cat@ morphism that keeps whichever leg @cat@ can use and discards the other.
--
-- Every category admits this functor, which is what lets a 'CategoricalFunctor'
-- of /any/ variance map an isomorphism, whether its domain is @('->')@
-- (covariant), 'Op' (contravariant), or @'Iso' ('->')@ (invariant). See
-- 'Kindly.Functor.mapIso'.
--
-- 'liftIso' fixes its source to @'Iso' ('->')@, so @a@ and @b@ are 'Type' and
-- the kind is @'Cat' 'Type'@. Supporting rank-2 functors (whose objects are type
-- constructors) needs more than @PolyKinds@. There the universally available
-- isos are natural isomorphisms, @'Iso' ((->) '~>' (->))@, not @'Iso' ('->')@,
-- so the source groupoid has to be abstracted too, e.g. a second parameter
-- @LiftIso src cat@ carrying the core groupoid at that kind.
--
-- === Laws
--
-- [Identity]    @'liftIso' 'id' == 'id'@
-- [Composition] @'liftIso' (i '.' j) == 'liftIso' i '.' 'liftIso' j@
type LiftIso :: Cat Type -> Constraint
class (Category cat) => LiftIso cat where
  liftIso :: Iso (->) a b -> cat a b

-- | A covariant @('->')@ functor keeps the forward leg.
instance LiftIso (->) where
  liftIso :: Iso (->) a b -> a -> b
  liftIso = embed

-- | A contravariant 'Op' functor keeps the backward leg.
instance LiftIso Op where
  liftIso :: Iso (->) a b -> Op a b
  liftIso i = Op (project i)

-- | An invariant @'Iso' ('->')@ functor keeps both legs. The lift is the identity.
instance LiftIso (Iso (->)) where
  liftIso :: Iso (->) a b -> Iso (->) a b
  liftIso = id

-- | A @'Star' f@ Kleisli arrow keeps the forward leg, returning it in @f@ via
-- 'pure'. Needs @'Monad' f@, matching its @'Category' ('Star' f)@ instance.
-- @Star Maybe@ is the domain the library uses for filtering (@Filterable@)
-- functors.
instance (Monad f) => LiftIso (Star f) where
  liftIso :: Iso (->) a b -> Star f a b
  liftIso i = Star (pure . embed i)

-- | A @'Kleisli' m@ arrow is @Star@ by another name (base's copy of the same
-- @a -> m b@ type), so its lift is identical.
instance (Monad m) => LiftIso (Kleisli m) where
  liftIso :: Iso (->) a b -> Kleisli m a b
  liftIso i = Kleisli (pure . embed i)
