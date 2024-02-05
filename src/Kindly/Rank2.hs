-- | Work in Progress.
module Kindly.Rank2
  ( BFunctor,
    bmap,
    BFunctor2,
    bmap2,
  )
where

--------------------------------------------------------------------------------

import Kindly.Class

--------------------------------------------------------------------------------

type BFunctor b = FunctorOf ((->) ~> (->)) (->) b

bmap :: BFunctor b => forall f g. (forall x. f x -> g x) -> b f -> b g 
bmap nat = map (Nat nat)

type BFunctor2 b = FunctorOf ((->) ~> (->) ~> (->)) (->) b

bmap2 :: BFunctor2 b => forall f g. (forall x x'. f x x' -> g x x') -> b f -> b g 
bmap2 nat = map (Nat (Nat nat))

