{-# LANGUAGE ImpredicativeTypes #-}

-- | @hedgehog-classes@ 'Laws' for this library's category-polymorphic functor
-- classes. A consumer can law-test their own 'CategoricalFunctor', 'MapArg1',
-- and 'MapArg2' instances the way they test 'Functor' or 'Monoid'.
--
-- > import Kindly.Functor.Laws (functorLaws)
-- > import Hedgehog.Classes (lawsCheck)
-- >
-- > main :: IO Bool
-- > main = lawsCheck (functorLaws genMyFunctor)
--
-- One bundle per variance, each stating the same two laws. Identity
-- (@'map1' 'id' = 'id'@) and composition
-- (@'map1' (f '.' g) = 'map1' f '.' 'map1' g@), with @'id'@ and @('.')@ in the
-- functor's /domain/ 'Category'. 'functorLaws' works at @('->')@ (covariant),
-- 'contravariantFunctorLaws' at @'Op'@, 'invariantFunctorLaws' at
-- @'Iso' ('->')@. 'bifunctorLaws' and 'profunctorLaws' also cover @'map2'@,
-- at the @('->')@ and 'Op' domains respectively. 'mapIsoLaws' checks
-- @'Kindly.Functor.mapIso'@, which maps an isomorphism through a functor of any
-- variance, so one bundle serves all three domains. 'bimapIsoLaws' and
-- 'trimapIsoLaws' do the same for @'Kindly.Bifunctor.bimapIso'@ and
-- @'Kindly.Trifunctor.trimapIso'@, checking functoriality in every 'Iso'
-- position. 'liftIsoLaws' checks 'liftIso' itself at any target category,
-- covering the 'LiftIso' instances no exported functor witnesses (e.g.
-- @Star f@ and @Kleisli m@).
--
-- The bundles are separate functions because the comparison differs. Covariant
-- functors compare directly with 'Eq'. Contravariant and invariant functors
-- usually have no 'Eq' or 'Show', so they are checked extensionally through a
-- caller-supplied @obs :: f 'Int' -> 'Int' -> r@ that observes both sides at the
-- 'Int' witness.
--
-- The rank-2 generator @forall x. 'Gen' x -> 'Gen' (f x)@ lets a covariant law
-- instantiate @f@ at whichever element type it needs.
module Kindly.Functor.Laws
  ( -- * Covariant functors
    functorLaws,

    -- * Contravariant functors
    contravariantFunctorLaws,

    -- * Invariant functors
    invariantFunctorLaws,

    -- * Isomorphism mapping (any variance)
    liftIsoLaws,
    mapIsoLaws,

    -- * Covariant bifunctors
    bifunctorLaws,
    observedBifunctorLaws,
    bimapIsoLaws,

    -- * Profunctors
    profunctorLaws,

    -- * Trifunctors
    observedTrifunctorLaws,
    trimapIsoLaws,
  )
where

--------------------------------------------------------------------------------

import Control.Category (Category (id, (.)))
import Data.Functor.Contravariant (Op (..))
import Data.Isomorphism (Iso (..))
import Hedgehog (Gen, Property, forAll, forAllWith, property, (===))
import Hedgehog.Classes (Laws (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kindly.Bifunctor (Bifunctor, bimapIso)
import Kindly.Class (LiftIso, MapArg1, MapArg2, MapArg3, liftIso, map1, map2, map3)
import Kindly.Functor (mapIso)
import Kindly.Trifunctor (Trifunctor, trimapIso)
import Prelude hiding (id, (.))

--------------------------------------------------------------------------------

-- | The element type the laws are witnessed at.
genInt :: Gen Int
genInt = Gen.int (Range.linear (-100) 100)

--------------------------------------------------------------------------------
-- Covariant

-- | The functor laws for a /covariant/ functor's @'map1'@ (domain @('->')@),
-- compared with 'Eq'.
functorLaws ::
  forall f.
  ( MapArg1 (->) f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x)
  ) =>
  (forall x. Gen x -> Gen (f x)) ->
  Laws
functorLaws genF =
  Laws
    "Functor"
    [ ("Identity", covariantIdentity genF),
      ("Composition", covariantComposition genF)
    ]

covariantIdentity ::
  forall f.
  ( MapArg1 (->) f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x)
  ) =>
  (forall x. Gen x -> Gen (f x)) ->
  Property
covariantIdentity genF = property $ do
  fa <- forAll (genF genInt)
  map1 (id :: Int -> Int) fa === fa

covariantComposition ::
  forall f.
  ( MapArg1 (->) f,
    forall x. (Eq x) => Eq (f x),
    forall x. (Show x) => Show (f x)
  ) =>
  (forall x. Gen x -> Gen (f x)) ->
  Property
covariantComposition genF = property $ do
  fa <- forAll (genF genInt)
  let g = (+ 1) :: Int -> Int
      h = (* 2) :: Int -> Int
  map1 (g . h) fa === map1 g (map1 h fa)

--------------------------------------------------------------------------------
-- Contravariant

-- | The functor laws for a /contravariant/ functor's @'map1'@ (domain 'Op'),
-- observed through @obs@ since such functors are not 'Eq' or 'Show'.
contravariantFunctorLaws ::
  forall f r.
  (MapArg1 Op f, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Laws
contravariantFunctorLaws genF obs =
  Laws
    "Functor (contravariant)"
    [ ("Identity", contravariantIdentity genF obs),
      ("Composition", contravariantComposition genF obs)
    ]

contravariantIdentity ::
  forall f r.
  (MapArg1 Op f, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Property
contravariantIdentity genF obs = property $ do
  fa <- forAllWith (const "<opaque>") genF
  a <- forAll genInt
  obs (map1 (id :: Op Int Int) fa) a === obs fa a

contravariantComposition ::
  forall f r.
  (MapArg1 Op f, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Property
contravariantComposition genF obs = property $ do
  fa <- forAllWith (const "<opaque>") genF
  a <- forAll genInt
  let g = Op (+ 1) :: Op Int Int
      h = Op (* 2) :: Op Int Int
  obs (map1 (g . h) fa) a === obs (map1 g (map1 h fa)) a

--------------------------------------------------------------------------------
-- Invariant

-- | The functor laws for an /invariant/ functor's @'map1'@ (domain
-- @'Iso' ('->')@), observed through @obs@.
invariantFunctorLaws ::
  forall f r.
  (MapArg1 (Iso (->)) f, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Laws
invariantFunctorLaws genF obs =
  Laws
    "Functor (invariant)"
    [ ("Identity", invariantIdentity genF obs),
      ("Composition", invariantComposition genF obs)
    ]

invariantIdentity ::
  forall f r.
  (MapArg1 (Iso (->)) f, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Property
invariantIdentity genF obs = property $ do
  fa <- forAllWith (const "<opaque>") genF
  a <- forAll genInt
  obs (map1 (id :: Iso (->) Int Int) fa) a === obs fa a

invariantComposition ::
  forall f r.
  (MapArg1 (Iso (->)) f, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Property
invariantComposition genF obs = property $ do
  fa <- forAllWith (const "<opaque>") genF
  a <- forAll genInt
  let g = Iso (+ 1) (subtract 1) :: Iso (->) Int Int
      h = Iso (* 2) (`div` 2) :: Iso (->) Int Int
  obs (map1 (g . h) fa) a === obs (map1 g (map1 h fa)) a

--------------------------------------------------------------------------------
-- Isomorphism mapping (any variance)

-- | The functor laws for 'liftIso', the identity-on-objects functor from the
-- @'Iso' ('->')@ groupoid into a target category @cat@. Identity
-- (@'liftIso' 'id' = 'id'@) and composition
-- (@'liftIso' (i '.' j) = 'liftIso' i '.' 'liftIso' j@), with @'id'@ and @('.')@
-- on the left in @'Iso' ('->')@ and on the right in @cat@. A @cat a b@ morphism
-- is usually neither 'Eq' nor 'Show', so it is observed through @obs@ at the
-- 'Int' witness. The target @cat@ is recovered from @obs@, so this bundle covers
-- every 'LiftIso' instance, including those no exported functor witnesses.
liftIsoLaws ::
  forall cat r.
  (LiftIso cat, Eq r, Show r) =>
  (cat Int Int -> Int -> r) ->
  Laws
liftIsoLaws obs =
  Laws
    "liftIso"
    [ ("Identity", liftIsoIdentity obs),
      ("Composition", liftIsoComposition obs)
    ]

liftIsoIdentity ::
  forall cat r.
  (LiftIso cat, Eq r, Show r) =>
  (cat Int Int -> Int -> r) ->
  Property
liftIsoIdentity obs = property $ do
  a <- forAll genInt
  obs (liftIso (id :: Iso (->) Int Int)) a === obs (id :: cat Int Int) a

liftIsoComposition ::
  forall cat r.
  (LiftIso cat, Eq r, Show r) =>
  (cat Int Int -> Int -> r) ->
  Property
liftIsoComposition obs = property $ do
  a <- forAll genInt
  let i = Iso (+ 1) (subtract 1) :: Iso (->) Int Int
      j = Iso (* 2) (`div` 2) :: Iso (->) Int Int
  obs (liftIso (i . j)) a === obs (liftIso i . liftIso j) a

-- | The functor laws stated through @'mapIso'@, which maps a @('->')@
-- isomorphism through a functor of /any/ variance. Identity
-- (@'mapIso' 'id' = 'id'@) and composition
-- (@'mapIso' (i '.' j) = 'mapIso' i '.' 'mapIso' j@), with @'id'@ and @('.')@ in
-- the @'Iso' ('->')@ groupoid, observed through @obs@. The functor's domain
-- category is recovered from @f@, so one bundle covers covariant, contravariant,
-- and invariant functors.
mapIsoLaws ::
  forall cat f r.
  (MapArg1 cat f, LiftIso cat, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Laws
mapIsoLaws genF obs =
  Laws
    "mapIso"
    [ ("Identity", mapIsoIdentity genF obs),
      ("Composition", mapIsoComposition genF obs)
    ]

mapIsoIdentity ::
  forall cat f r.
  (MapArg1 cat f, LiftIso cat, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Property
mapIsoIdentity genF obs = property $ do
  fa <- forAllWith (const "<opaque>") genF
  a <- forAll genInt
  obs (mapIso (id :: Iso (->) Int Int) fa) a === obs fa a

mapIsoComposition ::
  forall cat f r.
  (MapArg1 cat f, LiftIso cat, Eq r, Show r) =>
  Gen (f Int) ->
  (f Int -> Int -> r) ->
  Property
mapIsoComposition genF obs = property $ do
  fa <- forAllWith (const "<opaque>") genF
  a <- forAll genInt
  let i = Iso (+ 1) (subtract 1) :: Iso (->) Int Int
      j = Iso (* 2) (`div` 2) :: Iso (->) Int Int
  obs (mapIso (i . j) fa) a === obs (mapIso i (mapIso j fa)) a

--------------------------------------------------------------------------------
-- Covariant bifunctor

-- | The functor laws for a covariant bifunctor's @'map2'@ (its first argument),
-- compared with 'Eq'.
bifunctorLaws ::
  forall p.
  ( MapArg2 (->) (->) p,
    forall a b. (Eq a, Eq b) => Eq (p a b),
    forall a b. (Show a, Show b) => Show (p a b)
  ) =>
  (forall a b. Gen a -> Gen b -> Gen (p a b)) ->
  Laws
bifunctorLaws genP =
  Laws
    "Bifunctor"
    [ ("map2 Identity", bifunctorIdentity genP),
      ("map2 Composition", bifunctorComposition genP)
    ]

bifunctorIdentity ::
  forall p.
  ( MapArg2 (->) (->) p,
    forall a b. (Eq a, Eq b) => Eq (p a b),
    forall a b. (Show a, Show b) => Show (p a b)
  ) =>
  (forall a b. Gen a -> Gen b -> Gen (p a b)) ->
  Property
bifunctorIdentity genP = property $ do
  p <- forAll (genP genInt genInt)
  map2 (id :: Int -> Int) p === p

bifunctorComposition ::
  forall p.
  ( MapArg2 (->) (->) p,
    forall a b. (Eq a, Eq b) => Eq (p a b),
    forall a b. (Show a, Show b) => Show (p a b)
  ) =>
  (forall a b. Gen a -> Gen b -> Gen (p a b)) ->
  Property
bifunctorComposition genP = property $ do
  p <- forAll (genP genInt genInt)
  let g = (+ 1) :: Int -> Int
      h = (* 2) :: Int -> Int
  map2 (g . h) p === map2 g (map2 h p)

-- | The functor laws for a covariant @'map2'@ whose inner category is not
-- @('->')@ (e.g. t'Op', where @'MapArg2' ('->') 'Op' 'Op'@ holds), observed
-- through @obs@ since such bifunctors are function-shaped.
observedBifunctorLaws ::
  forall cat2 p r.
  (MapArg2 (->) cat2 p, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Laws
observedBifunctorLaws genP obs =
  Laws
    "Bifunctor (observed)"
    [ ("map2 Identity", observedBifunctorIdentity genP obs),
      ("map2 Composition", observedBifunctorComposition genP obs)
    ]

observedBifunctorIdentity ::
  forall cat2 p r.
  (MapArg2 (->) cat2 p, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Property
observedBifunctorIdentity genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  obs (map2 (id :: Int -> Int) p) a === obs p a

observedBifunctorComposition ::
  forall cat2 p r.
  (MapArg2 (->) cat2 p, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Property
observedBifunctorComposition genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  let g = (+ 1) :: Int -> Int
      h = (* 2) :: Int -> Int
  obs (map2 (g . h) p) a === obs (map2 g (map2 h p)) a

--------------------------------------------------------------------------------
-- Bifunctor isomorphism mapping (any variance)

-- | The functor laws stated through @'Kindly.Bifunctor.bimapIso'@, which maps a
-- @('->')@ isomorphism through each position of a bifunctor of /any/ variance.
-- Identity (@'bimapIso' 'id' 'id' = 'id'@) and composition
-- (@'bimapIso' (i '.' i') (j '.' j') = 'bimapIso' i j '.' 'bimapIso' i' j'@),
-- with @'id'@ and @('.')@ in the @'Iso' ('->')@ groupoid, observed through
-- @obs@. Each position's category is recovered from @p@, so one bundle covers
-- every combination of variances.
bimapIsoLaws ::
  forall cat1 cat2 p r.
  (Bifunctor cat1 cat2 p, LiftIso cat1, LiftIso cat2, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Laws
bimapIsoLaws genP obs =
  Laws
    "bimapIso"
    [ ("Identity", bimapIsoIdentity genP obs),
      ("Composition", bimapIsoComposition genP obs)
    ]

bimapIsoIdentity ::
  forall cat1 cat2 p r.
  (Bifunctor cat1 cat2 p, LiftIso cat1, LiftIso cat2, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Property
bimapIsoIdentity genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  obs (bimapIso (id :: Iso (->) Int Int) (id :: Iso (->) Int Int) p) a === obs p a

bimapIsoComposition ::
  forall cat1 cat2 p r.
  (Bifunctor cat1 cat2 p, LiftIso cat1, LiftIso cat2, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Property
bimapIsoComposition genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  let i1 = Iso (+ 1) (subtract 1) :: Iso (->) Int Int
      i2 = Iso (* 2) (`div` 2) :: Iso (->) Int Int
      j1 = Iso (+ 3) (subtract 3) :: Iso (->) Int Int
      j2 = Iso (* 5) (`div` 5) :: Iso (->) Int Int
  obs (bimapIso (i1 . i2) (j1 . j2) p) a === obs (bimapIso i1 j1 (bimapIso i2 j2 p)) a

--------------------------------------------------------------------------------
-- Trifunctor

-- | The functor laws for a covariant @'map3'@, observed through @obs@ so the
-- bundle works for both 'Eq'-comparable trifunctors (observe with @const@)
-- and function-shaped ones like 'Data.Profunctor.Forget'.
observedTrifunctorLaws ::
  forall cat2 cat3 p r.
  (MapArg3 (->) cat2 cat3 p, Eq r, Show r) =>
  Gen (p Int Int Int) ->
  (p Int Int Int -> Int -> r) ->
  Laws
observedTrifunctorLaws genP obs =
  Laws
    "Trifunctor (observed)"
    [ ("map3 Identity", observedTrifunctorIdentity genP obs),
      ("map3 Composition", observedTrifunctorComposition genP obs)
    ]

observedTrifunctorIdentity ::
  forall cat2 cat3 p r.
  (MapArg3 (->) cat2 cat3 p, Eq r, Show r) =>
  Gen (p Int Int Int) ->
  (p Int Int Int -> Int -> r) ->
  Property
observedTrifunctorIdentity genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  obs (map3 (id :: Int -> Int) p) a === obs p a

observedTrifunctorComposition ::
  forall cat2 cat3 p r.
  (MapArg3 (->) cat2 cat3 p, Eq r, Show r) =>
  Gen (p Int Int Int) ->
  (p Int Int Int -> Int -> r) ->
  Property
observedTrifunctorComposition genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  let g = (+ 1) :: Int -> Int
      h = (* 2) :: Int -> Int
  obs (map3 (g . h) p) a === obs (map3 g (map3 h p)) a

--------------------------------------------------------------------------------
-- Trifunctor isomorphism mapping (any variance)

-- | The functor laws stated through @'Kindly.Trifunctor.trimapIso'@, which maps
-- a @('->')@ isomorphism through each position of a trifunctor of /any/
-- variance. Identity (@'trimapIso' 'id' 'id' 'id' = 'id'@) and composition
-- (@'trimapIso' (i '.' i') (j '.' j') (k '.' k') =
-- 'trimapIso' i j k '.' 'trimapIso' i' j' k'@), with @'id'@ and @('.')@ in the
-- @'Iso' ('->')@ groupoid, observed through @obs@. Each position's category is
-- recovered from @p@, so one bundle covers every combination of variances.
trimapIsoLaws ::
  forall cat1 cat2 cat3 p r.
  (Trifunctor cat1 cat2 cat3 p, LiftIso cat1, LiftIso cat2, LiftIso cat3, Eq r, Show r) =>
  Gen (p Int Int Int) ->
  (p Int Int Int -> Int -> r) ->
  Laws
trimapIsoLaws genP obs =
  Laws
    "trimapIso"
    [ ("Identity", trimapIsoIdentity genP obs),
      ("Composition", trimapIsoComposition genP obs)
    ]

trimapIsoIdentity ::
  forall cat1 cat2 cat3 p r.
  (Trifunctor cat1 cat2 cat3 p, LiftIso cat1, LiftIso cat2, LiftIso cat3, Eq r, Show r) =>
  Gen (p Int Int Int) ->
  (p Int Int Int -> Int -> r) ->
  Property
trimapIsoIdentity genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  obs (trimapIso (id :: Iso (->) Int Int) (id :: Iso (->) Int Int) (id :: Iso (->) Int Int) p) a === obs p a

trimapIsoComposition ::
  forall cat1 cat2 cat3 p r.
  (Trifunctor cat1 cat2 cat3 p, LiftIso cat1, LiftIso cat2, LiftIso cat3, Eq r, Show r) =>
  Gen (p Int Int Int) ->
  (p Int Int Int -> Int -> r) ->
  Property
trimapIsoComposition genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  let i1 = Iso (+ 1) (subtract 1) :: Iso (->) Int Int
      i2 = Iso (* 2) (`div` 2) :: Iso (->) Int Int
      j1 = Iso (+ 3) (subtract 3) :: Iso (->) Int Int
      j2 = Iso (* 5) (`div` 5) :: Iso (->) Int Int
      k1 = Iso (+ 7) (subtract 7) :: Iso (->) Int Int
      k2 = Iso (* 11) (`div` 11) :: Iso (->) Int Int
  obs (trimapIso (i1 . i2) (j1 . j2) (k1 . k2) p) a
    === obs (trimapIso i1 j1 k1 (trimapIso i2 j2 k2 p)) a

--------------------------------------------------------------------------------
-- Profunctor

-- | The functor laws for a /profunctor's/ @'map2'@ (domain 'Op' in its first
-- argument), observed through @obs@ since profunctors are function-shaped and
-- have no 'Eq' or 'Show'.
profunctorLaws ::
  forall p r.
  (MapArg2 Op (->) p, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Laws
profunctorLaws genP obs =
  Laws
    "Profunctor"
    [ ("map2 Identity", profunctorIdentity genP obs),
      ("map2 Composition", profunctorComposition genP obs)
    ]

profunctorIdentity ::
  forall p r.
  (MapArg2 Op (->) p, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Property
profunctorIdentity genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  obs (map2 (id :: Op Int Int) p) a === obs p a

profunctorComposition ::
  forall p r.
  (MapArg2 Op (->) p, Eq r, Show r) =>
  Gen (p Int Int) ->
  (p Int Int -> Int -> r) ->
  Property
profunctorComposition genP obs = property $ do
  p <- forAllWith (const "<opaque>") genP
  a <- forAll genInt
  let g = Op (+ 1) :: Op Int Int
      h = Op (* 2) :: Op Int Int
  obs (map2 (g . h) p) a === obs (map2 g (map2 h p)) a
