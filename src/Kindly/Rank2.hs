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

type FunctorB b = MapArg1 ((->) ~> (->)) b

type BifunctorB b = MapArg2 ((->) ~> (->)) ((->) ~> (->)) b

type TrifunctorB b = MapArg3 ((->) ~> (->)) ((->) ~> (->)) ((->) ~> (->)) b

--------------------------------------------------------------------------------
-- Selectors

-- | Map the rightmost functor parameter of a rank-2 type.
bmap1 :: (MapArg1 (c ~> d) b) => (forall x. d (f x) (g x)) -> b f -> b g
bmap1 n = map1 (Nat n)

-- | Map the second-from-right functor parameter of a rank-2 type.
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
