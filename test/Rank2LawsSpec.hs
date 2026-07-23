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

-- | Self-test for @kindly-functors:laws@' rank-2 (higher-kinded) bundles: run
-- 'bmapLaws' \/ 'bmap2Laws' against witness functors whose natural
-- endo-transformations are non-trivial (they carry lists, so @reverse@ \/ @drop@
-- give genuine, composition-distinguishing morphisms).
module Rank2LawsSpec (tests) where

--------------------------------------------------------------------------------

import Data.String (fromString)
import Hedgehog (Gen, Group (..), Property, PropertyName, checkSequential)
import Hedgehog.Classes (Laws (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kindly (CategoricalFunctor (..), Nat (..), type (~>))
import Kindly.Rank2.Laws (bmap2Laws, bmapLaws)
import Prelude

--------------------------------------------------------------------------------
-- Witness rank-2 functor over @f :: Type -> Type@.

data HKD f = HKD (f Bool) (f Int)

deriving instance (Eq (f Bool), Eq (f Int)) => Eq (HKD f)

deriving instance (Show (f Bool), Show (f Int)) => Show (HKD f)

instance CategoricalFunctor HKD where
  type Dom HKD = (->) ~> (->)
  type Cod HKD = (->)

  map :: (Nat (->) (->)) f g -> HKD f -> HKD g
  map (Nat nat) (HKD a b) = HKD (nat a) (nat b)

-- Witness bifunctor with non-trivial natural endomorphisms.

data BiList x y = BiList [x] [y]

deriving instance (Eq x, Eq y) => Eq (BiList x y)

deriving instance (Show x, Show y) => Show (BiList x y)

-- Witness rank-2 functor over @p :: Type -> Type -> Type@.

newtype HKD2 p = HKD2 (p Bool Int)

deriving instance (Eq (p Bool Int)) => Eq (HKD2 p)

deriving instance (Show (p Bool Int)) => Show (HKD2 p)

instance CategoricalFunctor HKD2 where
  type Dom HKD2 = (->) ~> ((->) ~> (->))
  type Cod HKD2 = (->)

  map :: Dom HKD2 p q -> HKD2 p -> HKD2 q
  map (Nat (Nat nat)) (HKD2 p) = HKD2 (nat p)

--------------------------------------------------------------------------------
-- Generators and sample natural transformations

genInt :: Gen Int
genInt = Gen.int (Range.linear (-100) 100)

genList :: Gen a -> Gen [a]
genList = Gen.list (Range.linear 0 4)

genHKD :: Gen (HKD [])
genHKD = HKD <$> genList Gen.bool <*> genList genInt

genHKD2 :: Gen (HKD2 BiList)
genHKD2 = (\xs ys -> HKD2 (BiList xs ys)) <$> genList Gen.bool <*> genList genInt

biReverse :: BiList x y -> BiList x y
biReverse (BiList xs ys) = BiList (reverse xs) (reverse ys)

biDrop :: BiList x y -> BiList x y
biDrop (BiList xs ys) = BiList (drop 1 xs) ys

--------------------------------------------------------------------------------

labeled :: String -> Laws -> [(PropertyName, Property)]
labeled prefix ls = [(fromString (prefix <> " " <> n), p) | (n, p) <- lawsProperties ls]

tests :: IO Bool
tests =
  checkSequential $
    Group "Rank-2 functor laws" $
      concat
        [ labeled "HKD []" (bmapLaws genHKD reverse (drop 1)),
          labeled "HKD2 BiList" (bmap2Laws genHKD2 biReverse biDrop)
        ]
