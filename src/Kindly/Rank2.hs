-- | Rank-2 functors: 'CategoricalFunctor's whose parameters are themselves
-- functors. A type with @N@ functor parameters is an @N@-ary functor between
-- functor categories, and 'bmap1' \/ 'bmap2' \/ 'bmap3' select which parameter
-- to map.
--
-- The selectors count parameters __from the right__, matching the core
-- 'Kindly.Class.map1' \/ 'Kindly.Class.map2' \/ 'Kindly.Class.map3'. On
-- @b f g h@, 'bmap1' maps @h@, 'bmap2' maps @g@, and 'bmap3' maps @f@.
module Kindly.Rank2
  ( -- * Covariant aliases
    FunctorB,
    BifunctorB,
    TrifunctorB,

    -- * Selectors
    bmap1,
    bmap2,
    bmap3,

    -- * Contravariant wrappers
    bcontramap1,
    bcontramap2,
    bcontramap3,

    -- * Invariant wrappers
    binvmap1,
    binvmap2,
    binvmap3,
  )
where

--------------------------------------------------------------------------------

import Data.Functor.Contravariant (Op (..))
import Data.Isomorphism (Iso (..))
import Kindly.Class

--------------------------------------------------------------------------------
-- Covariant aliases

-- | A rank-2 type covariant in its single functor parameter.
type FunctorB b = MapArg1 ((->) ~> (->)) b

-- | A rank-2 type covariant in both functor parameters.
type BifunctorB b = MapArg2 ((->) ~> (->)) ((->) ~> (->)) b

-- | A rank-2 type covariant in all three functor parameters.
type TrifunctorB b = MapArg3 ((->) ~> (->)) ((->) ~> (->)) ((->) ~> (->)) b

--------------------------------------------------------------------------------
-- Selectors

-- | Map the rightmost functor parameter of a rank-2 type.
--
-- For a one-parameter HKD:
--
-- > data Schema f = Schema (f Int) (f Bool)
-- >
-- > instance CategoricalFunctor Schema where
-- >   type Dom Schema = (->) ~> (->)
-- >   type Cod Schema = (->)
-- >   map (Nat nat) (Schema a b) = Schema (nat a) (nat b)
-- >
-- > -- turn every field's @Maybe@ into a list
-- > bmap1 maybeToList :: Schema Maybe -> Schema []
bmap1 :: (MapArg1 (c ~> d) b) => (forall x. d (f x) (g x)) -> b f -> b g
bmap1 n = map1 (Nat n)

-- | Map the second-from-right functor parameter of a rank-2 type.
--
-- On a two-parameter HKD @b f g@, 'bmap1' maps @g@ (rightmost) and 'bmap2' maps
-- @f@. Map both by nesting: @bmap2 n1 (bmap1 n2 x)@.
bmap2 :: (MapArg2 (c ~> d) e b) => (forall x. d (f x) (g x)) -> b f h -> b g h
bmap2 n = map2 (Nat n)

-- | Map the third-from-right functor parameter of a rank-2 type.
bmap3 :: (MapArg3 (c ~> d) e e' b) => (forall x. d (f x) (g x)) -> b f h i -> b g h i
bmap3 n = map3 (Nat n)

--------------------------------------------------------------------------------
-- Contravariant wrappers

-- | Map the rightmost parameter of a type contravariant in it.
bcontramap1 :: (MapArg1 (c ~> Op) b) => (forall x. g x -> f x) -> b f -> b g
bcontramap1 n = bmap1 (Op n)

-- | Map the second-from-right parameter of a type contravariant in it.
bcontramap2 :: (MapArg2 (c ~> Op) e b) => (forall x. g x -> f x) -> b f h -> b g h
bcontramap2 n = bmap2 (Op n)

-- | Map the third-from-right parameter of a type contravariant in it.
bcontramap3 :: (MapArg3 (c ~> Op) e e' b) => (forall x. g x -> f x) -> b f h i -> b g h i
bcontramap3 n = bmap3 (Op n)

--------------------------------------------------------------------------------
-- Invariant wrappers

-- | Map the rightmost parameter of a type invariant in it, supplying both legs.
binvmap1 :: (MapArg1 (c ~> Iso (->)) b) => (forall x. f x -> g x) -> (forall x. g x -> f x) -> b f -> b g
binvmap1 fwd bwd = bmap1 (Iso fwd bwd)

-- | Map the second-from-right parameter of a type invariant in it.
binvmap2 :: (MapArg2 (c ~> Iso (->)) e b) => (forall x. f x -> g x) -> (forall x. g x -> f x) -> b f h -> b g h
binvmap2 fwd bwd = bmap2 (Iso fwd bwd)

-- | Map the third-from-right parameter of a type invariant in it.
binvmap3 :: (MapArg3 (c ~> Iso (->)) e e' b) => (forall x. f x -> g x) -> (forall x. g x -> f x) -> b f h i -> b g h i
binvmap3 fwd bwd = bmap3 (Iso fwd bwd)

--------------------------------------------------------------------------------
-- Mapping several parameters at once
--
-- Map several parameters of a concrete rank-2 value by nesting selectors, which
-- type-checks because the type is known:
--
-- > bmap2 n1 (bmap1 n2 x)
--
-- There is no polymorphic @bbimap@ \/ @btrimap@ combinator. Every formulation
-- attempted failed:
--
--   * Nesting the selectors inside a polymorphic signature forces GHC to compute
--     @Dom (b f)@ (a stuck type family) through the @MapArgN@ fundep, and it will
--     not use the quantified constraint's superclass to unstick it. This is the
--     GHC < 9.4 quantified-constraint limitation, and it reproduces on 9.10.
--   * Composing raw @map1@ \/ @map2@ point-free is ambiguous (higher-order
--     unification of the intermediate); eta-expanded it hits the same stuck @Dom@.
--   * Routing through @Kindly.Bifunctor.bimap@ would work (a rank-2 HKD is a
--     bifunctor between functor categories) but its @Bifunctor@ alias pins the
--     kinds to @Type -> Type -> Type@; a poly-kinded quantified-constraint synonym
--     is rejected, and the point-free body goes ambiguous at poly-kind anyway.
--
-- Nesting already covers this, so the combinator is omitted.
--
-- bbimap ::
--   (MapArg2 (c ~> d) (c' ~> d') b, forall x. MapArg1 (c' ~> d') (b x)) =>
--   (forall x. d (f x) (g x)) ->
--   (forall x. d' (h x) (i x)) ->
--   b f h ->
--   b g i
-- bbimap n1 n2 x = bmap2 n1 (bmap1 n2 x)  -- does not compile: stuck Dom (b f)
