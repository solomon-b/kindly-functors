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
-- at the @('->')@ and 'Op' domains respectively.
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

    -- * Covariant bifunctors
    bifunctorLaws,
    observedBifunctorLaws,

    -- * Profunctors
    profunctorLaws,

    -- * Trifunctors
    observedTrifunctorLaws,
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
import Kindly.Class (MapArg1, MapArg2, MapArg3, map1, map2, map3)
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
