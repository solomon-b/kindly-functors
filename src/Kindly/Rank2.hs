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
  )
where

--------------------------------------------------------------------------------

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
