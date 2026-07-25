{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

-- | @hedgehog-classes@ 'Laws' for the rank-2 selectors in "Kindly.Rank2".
--
-- Each bundle checks identity (@'map1' 'id' = 'id'@) and composition
-- (@'map1' (n1 '.' n2) = 'map1' n1 '.' 'map1' n2@) for one selector, stated
-- through the core map and the @Nat@ 'Cat.Category' so the sample morphisms live
-- in the component category. One bundle per selector therefore covers every
-- variance. Instantiate at a covariant witness for @d = (->)@, a contravariant
-- one for @d = 'Data.Functor.Contravariant.Op'@, an invariant one for
-- @d = 'Data.Isomorphism.Iso' (->)@. Hedgehog cannot generate natural
-- transformations, so each is checked at a caller-chosen witness with two sample
-- natural endo-transformations, comparing values with 'Eq'.
module Kindly.Rank2.Laws
  ( bmap1Laws,
    bmap2Laws,
    bmap3Laws,
  )
where

--------------------------------------------------------------------------------

import Control.Category qualified as Cat
import Hedgehog (Gen, forAll, property, (===))
import Hedgehog.Classes (Laws (..))
import Kindly.Class (MapArg1, MapArg2, MapArg3, Nat (..), map1, map2, map3, type (~>))
import Prelude

--------------------------------------------------------------------------------

bmap1Laws ::
  forall c d b f.
  (MapArg1 (c ~> d) b, Cat.Category c, Cat.Category d, Eq (b f), Show (b f)) =>
  Gen (b f) ->
  (forall x. d (f x) (f x)) ->
  (forall x. d (f x) (f x)) ->
  Laws
bmap1Laws genB s1 s2 =
  Laws
    "bmap1"
    [ ( "Identity",
        property $ do
          bf <- forAll genB
          map1 (Cat.id :: (c ~> d) f f) bf === bf
      ),
      ( "Composition",
        property $ do
          bf <- forAll genB
          map1 (Nat s1 Cat.. Nat s2) bf === map1 (Nat s1) (map1 (Nat s2) bf)
      )
    ]

bmap2Laws ::
  forall c d e b f h.
  (MapArg2 (c ~> d) e b, Cat.Category c, Cat.Category d, Eq (b f h), Show (b f h)) =>
  Gen (b f h) ->
  (forall x. d (f x) (f x)) ->
  (forall x. d (f x) (f x)) ->
  Laws
bmap2Laws genB s1 s2 =
  Laws
    "bmap2"
    [ ( "Identity",
        property $ do
          bf <- forAll genB
          map2 (Cat.id :: (c ~> d) f f) bf === bf
      ),
      ( "Composition",
        property $ do
          bf <- forAll genB
          map2 (Nat s1 Cat.. Nat s2) bf === map2 (Nat s1) (map2 (Nat s2) bf)
      )
    ]

bmap3Laws ::
  forall c d e e' b f h i.
  (MapArg3 (c ~> d) e e' b, Cat.Category c, Cat.Category d, Eq (b f h i), Show (b f h i)) =>
  Gen (b f h i) ->
  (forall x. d (f x) (f x)) ->
  (forall x. d (f x) (f x)) ->
  Laws
bmap3Laws genB s1 s2 =
  Laws
    "bmap3"
    [ ( "Identity",
        property $ do
          bf <- forAll genB
          map3 (Cat.id :: (c ~> d) f f) bf === bf
      ),
      ( "Composition",
        property $ do
          bf <- forAll genB
          map3 (Nat s1 Cat.. Nat s2) bf === map3 (Nat s1) (map3 (Nat s2) bf)
      )
    ]
