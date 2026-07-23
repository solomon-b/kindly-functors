{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Self-test for the @kindly-functors:laws@ sublibrary: run the exported
-- 'Laws' against known-good library instances across all three variances and,
-- crucially, across the structural / generic-representation instances
-- (@'Data.Functor.Compose.Compose'@, @'Data.Functor.Product.Product'@,
-- @(':*:')@, @'Data.Functor.Sum.Sum'@, @'GHC.Generics.Rec1'@,
-- @'GHC.Generics.Par1'@) — the ones the blanket @MapArgN@ instances must keep
-- resolving. Each instance's functor laws are checked as hedgehog properties.
module LawsSpec (tests) where

--------------------------------------------------------------------------------

import Data.Functor.Compose (Compose (..))
import Data.Functor.Contravariant (Predicate (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product qualified as Product
import Data.Functor.Sum (Sum (..))
import Data.List.NonEmpty (NonEmpty)
import Data.Monoid (Endo (..))
import Data.String (fromString)
import GHC.Generics (Par1 (..), Rec1 (..), (:*:) (..))
import Hedgehog (Gen, Group (..), Property, PropertyName, checkSequential)
import Hedgehog.Classes (Laws (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
-- 'Kindly' is imported only to bring the library's (orphan) CategoricalFunctor
-- and MapArgN instances into the test's transitive scope.
import Kindly ()
import Kindly.Functor.Laws
  ( bifunctorLaws,
    contravariantFunctorLaws,
    functorLaws,
    invariantFunctorLaws,
  )
import Prelude

--------------------------------------------------------------------------------
-- Generators

genInt :: Gen Int
genInt = Gen.int (Range.linear (-100) 100)

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe = Gen.maybe

genList :: Gen a -> Gen [a]
genList = Gen.list (Range.linear 0 4)

genNonEmpty :: Gen a -> Gen (NonEmpty a)
genNonEmpty = Gen.nonEmpty (Range.linear 1 4)

genIdentity :: Gen a -> Gen (Identity a)
genIdentity g = Identity <$> g

-- Structural / generic-representation functors.

genCompose :: Gen a -> Gen (Compose Maybe [] a)
genCompose g = Compose <$> genMaybe (genList g)

genProduct :: Gen a -> Gen (Product.Product Maybe [] a)
genProduct g = Product.Pair <$> genMaybe g <*> genList g

genGenProd :: Gen a -> Gen ((Maybe :*: []) a)
genGenProd g = (:*:) <$> genMaybe g <*> genList g

genSum :: Gen a -> Gen (Sum Maybe [] a)
genSum g = Gen.choice [InL <$> genMaybe g, InR <$> genList g]

genRec1 :: Gen a -> Gen (Rec1 Maybe a)
genRec1 g = Rec1 <$> genMaybe g

genPar1 :: Gen a -> Gen (Par1 a)
genPar1 g = Par1 <$> g

-- Contravariant witness ('Predicate' has no 'Eq' \/ 'Show'; observe by running).

genPredicate :: Gen (Predicate Int)
genPredicate = (\n -> Predicate (> n)) <$> genInt

obsPredicate :: Predicate a -> a -> Bool
obsPredicate (Predicate p) = p

-- Invariant witness ('Endo'; observe by applying).

genEndo :: Gen (Endo Int)
genEndo = (\n -> Endo (+ n)) <$> genInt

obsEndo :: Endo a -> a -> a
obsEndo (Endo h) = h

-- Bifunctor witnesses.

genPairT :: Gen a -> Gen b -> Gen (a, b)
genPairT ga gb = (,) <$> ga <*> gb

genEitherT :: Gen a -> Gen b -> Gen (Either a b)
genEitherT ga gb = Gen.choice [Left <$> ga, Right <$> gb]

--------------------------------------------------------------------------------

-- | Splice a sublibrary 'Laws' into a hedgehog 'Group', prefixing each property
-- with the instance under test.
labeled :: String -> Laws -> [(PropertyName, Property)]
labeled prefix ls = [(fromString (prefix <> " " <> n), p) | (n, p) <- lawsProperties ls]

tests :: IO Bool
tests =
  checkSequential $
    Group "Functor laws" $
      concat
        [ -- Covariant leaves.
          labeled "Maybe" (functorLaws genMaybe),
          labeled "[]" (functorLaws genList),
          labeled "Identity" (functorLaws genIdentity),
          labeled "NonEmpty" (functorLaws genNonEmpty),
          -- Structural / generic-representation instances.
          labeled "Compose Maybe []" (functorLaws genCompose),
          labeled "Product Maybe []" (functorLaws genProduct),
          labeled "Maybe :*: []" (functorLaws genGenProd),
          labeled "Sum Maybe []" (functorLaws genSum),
          labeled "Rec1 Maybe" (functorLaws genRec1),
          labeled "Par1" (functorLaws genPar1),
          -- Contravariant and invariant variances.
          labeled "Predicate" (contravariantFunctorLaws genPredicate obsPredicate),
          labeled "Endo" (invariantFunctorLaws genEndo obsEndo),
          -- Covariant bifunctors (map2).
          labeled "(,)" (bifunctorLaws genPairT),
          labeled "Either" (bifunctorLaws genEitherT)
        ]
