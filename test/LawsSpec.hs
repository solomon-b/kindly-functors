{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Self-test for the @kindly-functors:laws@ sublibrary. Runs the exported
-- 'Laws' against known-good library instances across all three variances, and
-- across the structural and generic-representation instances the blanket
-- @MapArgN@ instances must keep resolving (@'Data.Functor.Compose.Compose'@,
-- @'Data.Functor.Product.Product'@, @(':*:')@, @'Data.Functor.Sum.Sum'@,
-- @'GHC.Generics.Rec1'@, @'GHC.Generics.Par1'@). Each instance's functor laws
-- run as hedgehog properties.
module LawsSpec (tests) where

--------------------------------------------------------------------------------

import Control.Applicative.Lift (Lift (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bifunctor.Biff (Biff (..))
import Data.Bifunctor.Clown (Clown (..))
import Data.Bifunctor.Flip (Flip (..))
import Data.Bifunctor.Joker (Joker (..))
import Data.Bifunctor.Product qualified as BiProduct
import Data.Bifunctor.Sum qualified as BiSum
import Data.Bifunctor.Tannen (Tannen (..))
import Data.Bifunctor.Wrapped (WrappedBifunctor (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Constant (Constant (..))
import Data.Functor.Contravariant (Comparison (..), Equivalence (..), Op (..), Predicate (..))
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product qualified as Product
import Data.Functor.Reverse (Reverse (..))
import Data.Functor.Sum (Sum (..))
import Data.Functor.These (These1 (..))
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

-- Structural and generic-representation functors.

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

-- Transformer functors, instantiated at 'Maybe' so 'Eq1' gives back 'Eq'.

genIdentityT :: Gen a -> Gen (IdentityT Maybe a)
genIdentityT g = IdentityT <$> genMaybe g

genMaybeT :: Gen a -> Gen (MaybeT Maybe a)
genMaybeT g = MaybeT <$> genMaybe (genMaybe g)

genExceptT :: Gen a -> Gen (ExceptT Int Maybe a)
genExceptT g = ExceptT <$> genMaybe (genEitherT genInt g)

genReverse :: Gen a -> Gen (Reverse Maybe a)
genReverse g = Reverse <$> genMaybe g

genConstant :: Gen a -> Gen (Constant Int a)
genConstant _ = Constant <$> genInt

genLift :: Gen a -> Gen (Lift Maybe a)
genLift g = Gen.choice [Pure <$> g, Other <$> genMaybe g]

genThese1 :: Gen a -> Gen (These1 Maybe [] a)
genThese1 g =
  Gen.choice
    [ This1 <$> genMaybe g,
      That1 <$> genList g,
      These1 <$> genMaybe g <*> genList g
    ]

-- Contravariant witness. 'Predicate' has no 'Eq' or 'Show', so observe by running.

genPredicate :: Gen (Predicate Int)
genPredicate = (\n -> Predicate (> n)) <$> genInt

obsPredicate :: Predicate a -> a -> Bool
obsPredicate (Predicate p) = p

genComparison :: Gen (Comparison Int)
genComparison = (\n -> Comparison (\x y -> compare (x + n) y)) <$> genInt

obsComparison :: Comparison Int -> Int -> (Ordering, Ordering)
obsComparison (Comparison c) a = (c a 0, c 0 a)

genEquivalence :: Gen (Equivalence Int)
genEquivalence = (\n -> Equivalence (\x y -> div x n == div y n)) <$> Gen.int (Range.linear 1 10)

obsEquivalence :: Equivalence Int -> Int -> (Bool, Bool)
obsEquivalence (Equivalence e) a = (e a 0, e 0 a)

genOp :: Gen (Op Int Int)
genOp = (\n -> Op (* n)) <$> genInt

obsOp :: Op Int Int -> Int -> Int
obsOp (Op g) = g

-- Invariant witness 'Endo', observed by applying.

genEndo :: Gen (Endo Int)
genEndo = (\n -> Endo (+ n)) <$> genInt

obsEndo :: Endo a -> a -> a
obsEndo (Endo h) = h

-- Bifunctor witnesses.

genPairT :: Gen a -> Gen b -> Gen (a, b)
genPairT ga gb = (,) <$> ga <*> gb

genEitherT :: Gen a -> Gen b -> Gen (Either a b)
genEitherT ga gb = Gen.choice [Left <$> ga, Right <$> gb]

-- Bifunctors-package witnesses.

genFlipT :: Gen a -> Gen b -> Gen (Flip (,) a b)
genFlipT ga gb = Flip <$> genPairT gb ga

genClownT :: Gen a -> Gen b -> Gen (Clown Maybe a b)
genClownT ga _ = Clown <$> genMaybe ga

genJokerT :: Gen a -> Gen b -> Gen (Joker Maybe a b)
genJokerT _ gb = Joker <$> genMaybe gb

genBiProductT :: Gen a -> Gen b -> Gen (BiProduct.Product (,) Either a b)
genBiProductT ga gb = BiProduct.Pair <$> genPairT ga gb <*> genEitherT ga gb

genBiSumT :: Gen a -> Gen b -> Gen (BiSum.Sum (,) Either a b)
genBiSumT ga gb = Gen.choice [BiSum.L2 <$> genPairT ga gb, BiSum.R2 <$> genEitherT ga gb]

genTannenT :: Gen a -> Gen b -> Gen (Tannen Maybe (,) a b)
genTannenT ga gb = Tannen <$> genMaybe (genPairT ga gb)

genBiffT :: Gen a -> Gen b -> Gen (Biff (,) Maybe [] a b)
genBiffT ga gb = Biff <$> genPairT (genMaybe ga) (genList gb)

genWrappedT :: Gen a -> Gen b -> Gen (WrappedBifunctor (,) a b)
genWrappedT ga gb = WrapBifunctor <$> genPairT ga gb

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
          -- Structural and generic-representation instances.
          labeled "Compose Maybe []" (functorLaws genCompose),
          labeled "Product Maybe []" (functorLaws genProduct),
          labeled "Maybe :*: []" (functorLaws genGenProd),
          labeled "Sum Maybe []" (functorLaws genSum),
          labeled "Rec1 Maybe" (functorLaws genRec1),
          labeled "Par1" (functorLaws genPar1),
          -- Transformer and companion functors.
          labeled "IdentityT Maybe" (functorLaws genIdentityT),
          labeled "MaybeT Maybe" (functorLaws genMaybeT),
          labeled "ExceptT Int Maybe" (functorLaws genExceptT),
          labeled "Reverse Maybe" (functorLaws genReverse),
          labeled "Constant Int" (functorLaws genConstant),
          labeled "Lift Maybe" (functorLaws genLift),
          labeled "These1 Maybe []" (functorLaws genThese1),
          -- Contravariant and invariant variances.
          labeled "Predicate" (contravariantFunctorLaws genPredicate obsPredicate),
          labeled "Comparison" (contravariantFunctorLaws genComparison obsComparison),
          labeled "Equivalence" (contravariantFunctorLaws genEquivalence obsEquivalence),
          labeled "Op Int" (contravariantFunctorLaws genOp obsOp),
          labeled "Endo" (invariantFunctorLaws genEndo obsEndo),
          -- Covariant bifunctors (map2).
          labeled "(,)" (bifunctorLaws genPairT),
          labeled "Either" (bifunctorLaws genEitherT),
          -- Bifunctors-package types (map2 and map1 at the partial application).
          labeled "Flip (,)" (bifunctorLaws genFlipT),
          labeled "Flip (,) Int" (functorLaws (genFlipT genInt)),
          labeled "Clown Maybe" (bifunctorLaws genClownT),
          labeled "Clown Maybe Int" (functorLaws (genClownT genInt)),
          labeled "Joker Maybe" (bifunctorLaws genJokerT),
          labeled "Joker Maybe Int" (functorLaws (genJokerT genInt)),
          labeled "Product (,) Either" (bifunctorLaws genBiProductT),
          labeled "Product (,) Either Int" (functorLaws (genBiProductT genInt)),
          labeled "Sum (,) Either" (bifunctorLaws genBiSumT),
          labeled "Sum (,) Either Int" (functorLaws (genBiSumT genInt)),
          labeled "Tannen Maybe (,)" (bifunctorLaws genTannenT),
          labeled "Tannen Maybe (,) Int" (functorLaws (genTannenT genInt)),
          labeled "Biff (,) Maybe []" (bifunctorLaws genBiffT),
          labeled "Biff (,) Maybe [] Int" (functorLaws (genBiffT genInt)),
          labeled "WrappedBifunctor (,)" (bifunctorLaws genWrappedT),
          labeled "WrappedBifunctor (,) Int" (functorLaws (genWrappedT genInt))
        ]
