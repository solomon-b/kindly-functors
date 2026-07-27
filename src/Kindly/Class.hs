{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}

module Kindly.Class
  ( -- * Category-polymorphic functors
    CategoricalFunctor (..),
    Cat,
    FunctorOf,

    -- * Natural transformations
    Nat (..),
    runNat,
    type (~>),

    -- * One-, two-, and three-argument interfaces
    MapArg1 (..),
    MapArg2 (..),
    MapArg3 (..),

    -- * Lifting @(->)@ isomorphisms
    LiftIso (..),

    -- * Generic deriving
    GenericK,
    deriveGenericK,
  )
where

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
import GHC.Base (Functor (fmap), Monad, Type, pure)
import Generics.Kind
import Generics.Kind.TH (deriveGenericK)
import Prelude (Bool (..))

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
  --
  -- 'map' has a generic default. A datatype with a @kind-generics@ 'GenericK'
  -- instance (from 'deriveGenericK') gets a 'CategoricalFunctor' instance from an
  -- empty body that gives only 'Dom' and 'Cod'. The default reads each argument's
  -- variance off the field structure, so an instance of the wrong sign is a
  -- compile error rather than a wrong answer.
  --
  -- @
  -- data Pred a = Pred (a -> Bool)

  -- $(deriveGenericK ''Pred)
  --
  -- instance CategoricalFunctor Pred where
  --   type Dom Pred = Op
  --   type Cod Pred = (->)
  -- @
  --
  -- This covers covariant (@Dom = (->)@), contravariant (@Op@), and invariant
  -- (@Iso (->)@) single-argument functors, and bifunctors, profunctors, and
  -- trifunctors in any per-argument combination of those variances. It does not
  -- cover non-@(->)@ domains such as @Star Maybe@ (filtering), rank-2 functors,
  -- constructors carrying constraints or existentials, or a recursive field whose
  -- head has no base @Functor@.

  map :: Dom f a b -> Cod f (f a) (f b)
  default map :: (GMapFull (Dom f) (Cod f) f) => Dom f a b -> Cod f (f a) (f b)
  map = gmapFull

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

--------------------------------------------------------------------------------
-- Internals backing map's generic default (see the 'map' Haddock for the
-- user-facing story). None of the names below are exported. The default
-- dispatches on the instance's Dom and Cod through GMapFull, which routes to a
-- position interpreter over the RepK.

-- Covariant interpreter over a RepK, mirroring
-- @Generics.Kind.Derive.FunctorPosition@ (reproduced to avoid depending on
-- @kind-generics-deriving@, which pulls in @aeson@).
class GFunctorPos (f :: LoT k -> Type) (v :: TyVar k Type) (as :: LoT k) (bs :: LoT k) where
  gfmapp :: (Interpret ('Var v) as -> Interpret ('Var v) bs) -> f as -> f bs

instance GFunctorPos U1 v as bs where
  gfmapp _ U1 = U1

instance (GFunctorPos f v as bs) => GFunctorPos (M1 i c f) v as bs where
  gfmapp v (M1 x) = M1 (gfmapp @_ @f @v @as @bs v x)

instance (GFunctorPos f v as bs, GFunctorPos g v as bs) => GFunctorPos (f :+: g) v as bs where
  gfmapp v (L1 x) = L1 (gfmapp @_ @f @v @as @bs v x)
  gfmapp v (R1 x) = R1 (gfmapp @_ @g @v @as @bs v x)

instance (GFunctorPos f v as bs, GFunctorPos g v as bs) => GFunctorPos (f :*: g) v as bs where
  gfmapp v (x :*: y) = gfmapp @_ @f @v @as @bs v x :*: gfmapp @_ @g @v @as @bs v y

instance (GFunctorArgPos t v as bs (ContainsTyVar v t)) => GFunctorPos (Field t) v as bs where
  gfmapp v (Field x) = Field (gfmappf @_ @t @v @as @bs @(ContainsTyVar v t) v x)

class GFunctorArgPos (t :: Atom d Type) (v :: TyVar d Type) (as :: LoT d) (bs :: LoT d) (p :: Bool) where
  gfmappf :: (Interpret ('Var v) as -> Interpret ('Var v) bs) -> Interpret t as -> Interpret t bs

instance (Interpret t as ~ Interpret t bs) => GFunctorArgPos t v as bs 'False where
  gfmappf _ = id

instance
  ( Functor (Interpret f as),
    Interpret f as ~ Interpret f bs,
    GFunctorArgPos x v as bs (ContainsTyVar v x)
  ) =>
  GFunctorArgPos (f ':@: x) v as bs 'True
  where
  gfmappf f = fmap (gfmappf @_ @x @v @as @bs @(ContainsTyVar v x) f)

instance (w ~ v) => GFunctorArgPos ('Var w) v as bs 'True where
  gfmappf f = f

-- Contravariant interpreter. At a function field the domain is mapped by the
-- covariant interpreter and the codomain recurses contravariantly.
class GContraPos (f :: LoT k -> Type) (v :: TyVar k Type) (as :: LoT k) (bs :: LoT k) where
  gcontrap :: (Interpret ('Var v) bs -> Interpret ('Var v) as) -> f as -> f bs

instance GContraPos U1 v as bs where
  gcontrap _ U1 = U1

instance (GContraPos f v as bs) => GContraPos (M1 i c f) v as bs where
  gcontrap v (M1 x) = M1 (gcontrap @_ @f @v @as @bs v x)

instance (GContraPos f v as bs, GContraPos g v as bs) => GContraPos (f :+: g) v as bs where
  gcontrap v (L1 x) = L1 (gcontrap @_ @f @v @as @bs v x)
  gcontrap v (R1 x) = R1 (gcontrap @_ @g @v @as @bs v x)

instance (GContraPos f v as bs, GContraPos g v as bs) => GContraPos (f :*: g) v as bs where
  gcontrap v (x :*: y) = gcontrap @_ @f @v @as @bs v x :*: gcontrap @_ @g @v @as @bs v y

instance (GContraArgPos t v as bs (ContainsTyVar v t)) => GContraPos (Field t) v as bs where
  gcontrap v (Field x) = Field (gcontrapf @_ @t @v @as @bs @(ContainsTyVar v t) v x)

class GContraArgPos (t :: Atom d Type) (v :: TyVar d Type) (as :: LoT d) (bs :: LoT d) (p :: Bool) where
  gcontrapf :: (Interpret ('Var v) bs -> Interpret ('Var v) as) -> Interpret t as -> Interpret t bs

instance (Interpret t as ~ Interpret t bs) => GContraArgPos t v as bs 'False where
  gcontrapf _ = id

instance
  ( GFunctorArgPos dom v bs as (ContainsTyVar v dom),
    GContraArgPos cod v as bs (ContainsTyVar v cod)
  ) =>
  GContraArgPos (('Kon (->) ':@: dom) ':@: cod) v as bs 'True
  where
  gcontrapf k field =
    gcontrapf @_ @cod @v @as @bs @(ContainsTyVar v cod) k
      . field
      . gfmappf @_ @dom @v @bs @as @(ContainsTyVar v dom) k

-- Invariant interpreter. Threads both legs of an isomorphism, swapping them at
-- each function field.
class GInvPos (f :: LoT k -> Type) (v :: TyVar k Type) (as :: LoT k) (bs :: LoT k) where
  ginvp ::
    (Interpret ('Var v) as -> Interpret ('Var v) bs) ->
    (Interpret ('Var v) bs -> Interpret ('Var v) as) ->
    f as ->
    f bs

instance GInvPos U1 v as bs where
  ginvp _ _ U1 = U1

instance (GInvPos f v as bs) => GInvPos (M1 i c f) v as bs where
  ginvp fwd bwd (M1 x) = M1 (ginvp @_ @f @v @as @bs fwd bwd x)

instance (GInvPos f v as bs, GInvPos g v as bs) => GInvPos (f :+: g) v as bs where
  ginvp fwd bwd (L1 x) = L1 (ginvp @_ @f @v @as @bs fwd bwd x)
  ginvp fwd bwd (R1 x) = R1 (ginvp @_ @g @v @as @bs fwd bwd x)

instance (GInvPos f v as bs, GInvPos g v as bs) => GInvPos (f :*: g) v as bs where
  ginvp fwd bwd (x :*: y) = ginvp @_ @f @v @as @bs fwd bwd x :*: ginvp @_ @g @v @as @bs fwd bwd y

instance (GInvArgPos t v as bs (ContainsTyVar v t)) => GInvPos (Field t) v as bs where
  ginvp fwd bwd (Field x) = Field (ginvpf @_ @t @v @as @bs @(ContainsTyVar v t) fwd bwd x)

class GInvArgPos (t :: Atom d Type) (v :: TyVar d Type) (as :: LoT d) (bs :: LoT d) (p :: Bool) where
  ginvpf ::
    (Interpret ('Var v) as -> Interpret ('Var v) bs) ->
    (Interpret ('Var v) bs -> Interpret ('Var v) as) ->
    Interpret t as ->
    Interpret t bs

instance (Interpret t as ~ Interpret t bs) => GInvArgPos t v as bs 'False where
  ginvpf _ _ = id

instance (w ~ v) => GInvArgPos ('Var w) v as bs 'True where
  ginvpf fwd _ = fwd

instance
  ( GInvArgPos dom v bs as (ContainsTyVar v dom),
    GInvArgPos cod v as bs (ContainsTyVar v cod)
  ) =>
  GInvArgPos (('Kon (->) ':@: dom) ':@: cod) v as bs 'True
  where
  ginvpf fwd bwd field =
    ginvpf @_ @cod @v @as @bs @(ContainsTyVar v cod) fwd bwd
      . field
      . ginvpf @_ @dom @v @bs @as @(ContainsTyVar v dom) bwd fwd

instance
  {-# OVERLAPPABLE #-}
  ( Functor (Interpret f as),
    Interpret f as ~ Interpret f bs,
    GInvArgPos x v as bs (ContainsTyVar v x)
  ) =>
  GInvArgPos (f ':@: x) v as bs 'True
  where
  ginvpf fwd bwd = fmap (ginvpf @_ @x @v @as @bs @(ContainsTyVar v x) fwd bwd)

--------------------------------------------------------------------------------
-- The wrappers quantify the assignment internally so a plain
-- @G...K (RepK f)@ suffices as a constraint.

class GFunctorK (rep :: LoT (Type -> Type) -> Type) where
  gfmapK :: (a -> b) -> rep (LoT1 a) -> rep (LoT1 b)

instance (forall a b. GFunctorPos rep 'VZ (LoT1 a) (LoT1 b)) => GFunctorK rep where
  gfmapK :: forall a b. (a -> b) -> rep (LoT1 a) -> rep (LoT1 b)
  gfmapK = gfmapp @_ @rep @'VZ @(LoT1 a) @(LoT1 b)

class GContraK (rep :: LoT (Type -> Type) -> Type) where
  gcontraK :: (b -> a) -> rep (LoT1 a) -> rep (LoT1 b)

instance (forall a b. GContraPos rep 'VZ (LoT1 a) (LoT1 b)) => GContraK rep where
  gcontraK :: forall a b. (b -> a) -> rep (LoT1 a) -> rep (LoT1 b)
  gcontraK = gcontrap @_ @rep @'VZ @(LoT1 a) @(LoT1 b)

class GInvK (rep :: LoT (Type -> Type) -> Type) where
  ginvK :: (a -> b) -> (b -> a) -> rep (LoT1 a) -> rep (LoT1 b)

instance (forall a b. GInvPos rep 'VZ (LoT1 a) (LoT1 b)) => GInvK rep where
  ginvK :: forall a b. (a -> b) -> (b -> a) -> rep (LoT1 a) -> rep (LoT1 b)
  ginvK = ginvp @_ @rep @'VZ @(LoT1 a) @(LoT1 b)

class GBiFirstK (rep :: LoT (Type -> Type -> Type) -> Type) where
  gbiFirstK :: (a -> c) -> rep (LoT2 a n) -> rep (LoT2 c n)

instance (forall a c n. GFunctorPos rep 'VZ (LoT2 a n) (LoT2 c n)) => GBiFirstK rep where
  gbiFirstK :: forall a c n. (a -> c) -> rep (LoT2 a n) -> rep (LoT2 c n)
  gbiFirstK = gfmapp @_ @rep @'VZ @(LoT2 a n) @(LoT2 c n)

class GProFirstK (rep :: LoT (Type -> Type -> Type) -> Type) where
  gproFirstK :: (c -> a) -> rep (LoT2 a n) -> rep (LoT2 c n)

instance (forall a c n. GContraPos rep 'VZ (LoT2 a n) (LoT2 c n)) => GProFirstK rep where
  gproFirstK :: forall a c n. (c -> a) -> rep (LoT2 a n) -> rep (LoT2 c n)
  gproFirstK = gcontrap @_ @rep @'VZ @(LoT2 a n) @(LoT2 c n)

class GBiFirstInvK (rep :: LoT (Type -> Type -> Type) -> Type) where
  gbiFirstInvK :: (a -> c) -> (c -> a) -> rep (LoT2 a n) -> rep (LoT2 c n)

instance (forall a c n. GInvPos rep 'VZ (LoT2 a n) (LoT2 c n)) => GBiFirstInvK rep where
  gbiFirstInvK :: forall a c n. (a -> c) -> (c -> a) -> rep (LoT2 a n) -> rep (LoT2 c n)
  gbiFirstInvK = ginvp @_ @rep @'VZ @(LoT2 a n) @(LoT2 c n)

type LoT3 a b c = a :&&: b :&&: c :&&: LoT0

class GTriFirstK (rep :: LoT (Type -> Type -> Type -> Type) -> Type) where
  gtriFirstK :: (a -> b) -> rep (LoT3 a x y) -> rep (LoT3 b x y)

instance (forall a b x y. GFunctorPos rep 'VZ (LoT3 a x y) (LoT3 b x y)) => GTriFirstK rep where
  gtriFirstK :: forall a b x y. (a -> b) -> rep (LoT3 a x y) -> rep (LoT3 b x y)
  gtriFirstK = gfmapp @_ @rep @'VZ @(LoT3 a x y) @(LoT3 b x y)

class GTriProFirstK (rep :: LoT (Type -> Type -> Type -> Type) -> Type) where
  gtriProFirstK :: (b -> a) -> rep (LoT3 a x y) -> rep (LoT3 b x y)

instance (forall a b x y. GContraPos rep 'VZ (LoT3 a x y) (LoT3 b x y)) => GTriProFirstK rep where
  gtriProFirstK :: forall a b x y. (b -> a) -> rep (LoT3 a x y) -> rep (LoT3 b x y)
  gtriProFirstK = gcontrap @_ @rep @'VZ @(LoT3 a x y) @(LoT3 b x y)

class GTriInvFirstK (rep :: LoT (Type -> Type -> Type -> Type) -> Type) where
  gtriInvFirstK :: (a -> b) -> (b -> a) -> rep (LoT3 a x y) -> rep (LoT3 b x y)

instance (forall a b x y. GInvPos rep 'VZ (LoT3 a x y) (LoT3 b x y)) => GTriInvFirstK rep where
  gtriInvFirstK :: forall a b x y. (a -> b) -> (b -> a) -> rep (LoT3 a x y) -> rep (LoT3 b x y)
  gtriInvFirstK = ginvp @_ @rep @'VZ @(LoT3 a x y) @(LoT3 b x y)

--------------------------------------------------------------------------------
-- One dispatch class keyed on the domain and codomain categories, so the single
-- 'map' default covers every variance and arity below.

class GMapFull (dom :: from -> from -> Type) (cod :: to -> to -> Type) (f :: from -> to) where
  gmapFull :: dom a b -> cod (f a) (f b)

-- covariant, single parameter (also the last argument of a two-parameter type)
instance
  (GenericK f, GFunctorK (RepK f)) =>
  GMapFull ((->) :: Type -> Type -> Type) ((->) :: Type -> Type -> Type) (f :: Type -> Type)
  where
  gmapFull :: forall a b. (a -> b) -> f a -> f b
  gmapFull d = toK @_ @f @(LoT1 b) . gfmapK @(RepK f) d . fromK @_ @f @(LoT1 a)

-- contravariant, single parameter
instance
  (GenericK f, GContraK (RepK f)) =>
  GMapFull (Op :: Type -> Type -> Type) ((->) :: Type -> Type -> Type) (f :: Type -> Type)
  where
  gmapFull :: forall a b. Op a b -> f a -> f b
  gmapFull (Op k) = toK @_ @f @(LoT1 b) . gcontraK @(RepK f) k . fromK @_ @f @(LoT1 a)

-- invariant, single parameter
instance
  (GenericK f, GInvK (RepK f)) =>
  GMapFull (Iso (->) :: Type -> Type -> Type) ((->) :: Type -> Type -> Type) (f :: Type -> Type)
  where
  gmapFull :: forall a b. Iso (->) a b -> f a -> f b
  gmapFull i = toK @_ @f @(LoT1 b) . ginvK @(RepK f) (embed i) (project i) . fromK @_ @f @(LoT1 a)

-- covariant in the first of two arguments, producing a Nat. The second
-- argument's category @cat2@ is the phantom source of the Nat and does not
-- affect the mapping, so a single instance covers every second-argument
-- variance.
instance
  (GenericK f, GBiFirstK (RepK f)) =>
  GMapFull ((->) :: Type -> Type -> Type) (cat2 ~> (->)) (f :: Type -> Type -> Type)
  where
  gmapFull :: forall a c. (a -> c) -> Nat cat2 (->) (f a) (f c)
  gmapFull h = Nat go
    where
      go :: forall n. f a n -> f c n
      go = toK @_ @f @(LoT2 c n) . gbiFirstK @(RepK f) h . fromK @_ @f @(LoT2 a n)

-- contravariant in the first of two arguments (a profunctor when the second is
-- covariant), producing a Nat.
instance
  (GenericK f, GProFirstK (RepK f)) =>
  GMapFull (Op :: Type -> Type -> Type) (cat2 ~> (->)) (f :: Type -> Type -> Type)
  where
  gmapFull :: forall a c. Op a c -> Nat cat2 (->) (f a) (f c)
  gmapFull (Op k) = Nat go
    where
      go :: forall n. f a n -> f c n
      go = toK @_ @f @(LoT2 c n) . gproFirstK @(RepK f) k . fromK @_ @f @(LoT2 a n)

-- invariant in the first of two arguments, producing a Nat.
instance
  (GenericK f, GBiFirstInvK (RepK f)) =>
  GMapFull (Iso (->) :: Type -> Type -> Type) (cat2 ~> (->)) (f :: Type -> Type -> Type)
  where
  gmapFull :: forall a c. Iso (->) a c -> Nat cat2 (->) (f a) (f c)
  gmapFull i = Nat go
    where
      go :: forall n. f a n -> f c n
      go = toK @_ @f @(LoT2 c n) . gbiFirstInvK @(RepK f) (embed i) (project i) . fromK @_ @f @(LoT2 a n)

-- covariant in the first of three arguments, producing a nested Nat. As with
-- two arguments, the trailing categories are phantom sources and one instance
-- per first-argument variance covers every combination of the other two.
instance
  (GenericK f, GTriFirstK (RepK f)) =>
  GMapFull ((->) :: Type -> Type -> Type) (cat2 ~> cat3 ~> (->)) (f :: Type -> Type -> Type -> Type)
  where
  gmapFull :: forall a b. (a -> b) -> Nat cat2 (cat3 ~> (->)) (f a) (f b)
  gmapFull morph = Nat middle
    where
      middle :: forall x. Nat cat3 (->) (f a x) (f b x)
      middle = Nat inner
        where
          inner :: forall y. f a x y -> f b x y
          inner = toK @_ @f @(LoT3 b x y) . gtriFirstK @(RepK f) morph . fromK @_ @f @(LoT3 a x y)

-- contravariant in the first of three arguments.
instance
  (GenericK f, GTriProFirstK (RepK f)) =>
  GMapFull (Op :: Type -> Type -> Type) (cat2 ~> cat3 ~> (->)) (f :: Type -> Type -> Type -> Type)
  where
  gmapFull :: forall a b. Op a b -> Nat cat2 (cat3 ~> (->)) (f a) (f b)
  gmapFull (Op k) = Nat middle
    where
      middle :: forall x. Nat cat3 (->) (f a x) (f b x)
      middle = Nat inner
        where
          inner :: forall y. f a x y -> f b x y
          inner = toK @_ @f @(LoT3 b x y) . gtriProFirstK @(RepK f) k . fromK @_ @f @(LoT3 a x y)

-- invariant in the first of three arguments.
instance
  (GenericK f, GTriInvFirstK (RepK f)) =>
  GMapFull (Iso (->) :: Type -> Type -> Type) (cat2 ~> cat3 ~> (->)) (f :: Type -> Type -> Type -> Type)
  where
  gmapFull :: forall a b. Iso (->) a b -> Nat cat2 (cat3 ~> (->)) (f a) (f b)
  gmapFull i = Nat middle
    where
      middle :: forall x. Nat cat3 (->) (f a x) (f b x)
      middle = Nat inner
        where
          inner :: forall y. f a x y -> f b x y
          inner = toK @_ @f @(LoT3 b x y) . gtriInvFirstK @(RepK f) (embed i) (project i) . fromK @_ @f @(LoT3 a x y)
