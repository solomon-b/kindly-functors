{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Self-test for @kindly-functors:laws@' rank-2 bundles. Runs 'bmap1Laws',
-- 'bmap2Laws', and 'bmap3Laws' against covariant one-, two-, and three-functor
-- witnesses, plus a contravariant and an invariant witness, so the reused
-- bundles exercise every variance. The witnesses carry lists, so @reverse@ and
-- @drop 1@ give genuine, composition-distinguishing natural transformations.
-- The contravariant and invariant witnesses hold functions, so they are compared
-- by observation against fixed probe inputs.
module Rank2LawsSpec (tests) where

--------------------------------------------------------------------------------

import Data.Functor.Contravariant (Op (..))
import Data.Isomorphism (Iso (..), embed, project)
import Data.String (fromString)
import Hedgehog (Gen, Group (..), Property, PropertyName, checkSequential)
import Hedgehog.Classes (Laws (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kindly (CategoricalFunctor (..), Nat (..), type (~>))
import Kindly.Rank2.Laws (bmap1Laws, bmap2Laws, bmap3Laws)
import Prelude

--------------------------------------------------------------------------------
-- Covariant witnesses (one, two, three functor parameters)

data H1 f = H1 (f Bool) (f Int)

deriving instance (Eq (f Bool), Eq (f Int)) => Eq (H1 f)

deriving instance (Show (f Bool), Show (f Int)) => Show (H1 f)

instance CategoricalFunctor H1 where
  type Dom H1 = (->) ~> (->)
  type Cod H1 = (->)
  map :: Nat (->) (->) f g -> H1 f -> H1 g
  map (Nat nat) (H1 a b) = H1 (nat a) (nat b)

data H2 f g = H2 (f Bool) (g Int)

deriving instance (Eq (f Bool), Eq (g Int)) => Eq (H2 f g)

deriving instance (Show (f Bool), Show (g Int)) => Show (H2 f g)

instance CategoricalFunctor (H2 f) where
  type Dom (H2 f) = (->) ~> (->)
  type Cod (H2 f) = (->)
  map (Nat nat) (H2 a b) = H2 a (nat b)

instance CategoricalFunctor H2 where
  type Dom H2 = (->) ~> (->)
  type Cod H2 = ((->) ~> (->)) ~> (->)
  map (Nat nat) = Nat (\(H2 a b) -> H2 (nat a) b)

data H3 f g h = H3 (f Bool) (g Int) (h Bool)

deriving instance (Eq (f Bool), Eq (g Int), Eq (h Bool)) => Eq (H3 f g h)

deriving instance (Show (f Bool), Show (g Int), Show (h Bool)) => Show (H3 f g h)

instance CategoricalFunctor (H3 f g) where
  type Dom (H3 f g) = (->) ~> (->)
  type Cod (H3 f g) = (->)
  map (Nat nat) (H3 a b c) = H3 a b (nat c)

instance CategoricalFunctor (H3 f) where
  type Dom (H3 f) = (->) ~> (->)
  type Cod (H3 f) = ((->) ~> (->)) ~> (->)
  map (Nat nat) = Nat (\(H3 a b c) -> H3 a (nat b) c)

instance CategoricalFunctor H3 where
  type Dom H3 = (->) ~> (->)
  type Cod H3 = ((->) ~> (->)) ~> ((->) ~> (->)) ~> (->)
  map (Nat nat) = Nat (Nat (\(H3 a b c) -> H3 (nat a) b c))

--------------------------------------------------------------------------------
-- Contravariant and invariant witnesses (function-shaped, over lists)

newtype Consumer f = Consumer (f Int -> Int)

instance CategoricalFunctor Consumer where
  type Dom Consumer = (->) ~> Op
  type Cod Consumer = (->)
  map (Nat opnat) (Consumer c) = Consumer (\g -> c (getOp opnat g))

newtype Endo1 f = Endo1 (f Int -> f Int)

instance CategoricalFunctor Endo1 where
  type Dom Endo1 = (->) ~> Iso (->)
  type Cod Endo1 = (->)
  map (Nat iso) (Endo1 h) = Endo1 (\g -> embed iso (h (project iso g)))

-- Observation: compare function-shaped witnesses by running them on probes.

consumerProbes :: [[Int]]
consumerProbes = [[], [0], [1, 2], [3, 4, 5]]

instance Eq (Consumer []) where
  Consumer p == Consumer q = fmap p consumerProbes == fmap q consumerProbes

instance Show (Consumer []) where
  show (Consumer p) = "Consumer " <> show (fmap p consumerProbes)

instance Eq (Endo1 []) where
  Endo1 p == Endo1 q = fmap p consumerProbes == fmap q consumerProbes

instance Show (Endo1 []) where
  show (Endo1 p) = "Endo1 " <> show (fmap p consumerProbes)

--------------------------------------------------------------------------------
-- Generators and sample natural transformations

genInt :: Gen Int
genInt = Gen.int (Range.linear (-100) 100)

genList :: Gen a -> Gen [a]
genList = Gen.list (Range.linear 0 4)

genH1 :: Gen (H1 [])
genH1 = H1 <$> genList Gen.bool <*> genList genInt

genH2 :: Gen (H2 [] [])
genH2 = H2 <$> genList Gen.bool <*> genList genInt

genH3 :: Gen (H3 [] [] [])
genH3 = H3 <$> genList Gen.bool <*> genList genInt <*> genList Gen.bool

genConsumer :: Gen (Consumer [])
genConsumer = Gen.element [Consumer sum, Consumer length, Consumer (sum . drop 1)]

genEndo1 :: Gen (Endo1 [])
genEndo1 = Gen.element [Endo1 id, Endo1 reverse, Endo1 (drop 1)]

-- Covariant samples: natural transformations @forall x. [x] -> [x]@.
-- Contravariant samples: the same wrapped in @Op@.
-- Invariant samples: paired legs in @Iso (->)@.

opReverse :: forall x. Op [x] [x]
opReverse = Op reverse

opDrop :: forall x. Op [x] [x]
opDrop = Op (drop 1)

isoReverse :: forall x. Iso (->) [x] [x]
isoReverse = Iso reverse reverse

isoDrop :: forall x. Iso (->) [x] [x]
isoDrop = Iso (drop 1) (drop 1)

--------------------------------------------------------------------------------

labeled :: String -> Laws -> [(PropertyName, Property)]
labeled prefix ls = [(fromString (prefix <> " " <> n), p) | (n, p) <- lawsProperties ls]

tests :: IO Bool
tests =
  checkSequential $
    Group "Rank-2 functor laws" $
      concat
        [ labeled "H1 [] bmap1" (bmap1Laws genH1 reverse (drop 1)),
          labeled "H2 [] [] bmap1" (bmap1Laws genH2 reverse (drop 1)),
          labeled "H2 [] [] bmap2" (bmap2Laws genH2 reverse (drop 1)),
          labeled "H3 [] [] [] bmap1" (bmap1Laws genH3 reverse (drop 1)),
          labeled "H3 [] [] [] bmap2" (bmap2Laws genH3 reverse (drop 1)),
          labeled "H3 [] [] [] bmap3" (bmap3Laws genH3 reverse (drop 1)),
          labeled "Consumer [] bmap1 (contravariant)" (bmap1Laws genConsumer opReverse opDrop),
          labeled "Endo1 [] bmap1 (invariant)" (bmap1Laws genEndo1 isoReverse isoDrop)
        ]
