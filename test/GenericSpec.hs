{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Law-checks 'CategoricalFunctor' instances that use the generic @map@
-- default. Every instance below has an empty body, only its 'Dom' and 'Cod'.
module GenericSpec (tests) where

--------------------------------------------------------------------------------

import Data.Functor.Contravariant (Op (..))
import Data.String (fromString)
import Generics.Kind.TH (deriveGenericK)
import Hedgehog (Gen, Group (..), Property, PropertyName, checkSequential, forAll, property, (===))
import Hedgehog.Classes (Laws (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Kindly (CategoricalFunctor (..), Iso (..), map1, map2, map3, type (~>))
import Kindly.Functor qualified as K
import Kindly.Functor.Laws (bifunctorLaws, contravariantFunctorLaws, functorLaws, invariantFunctorLaws, observedBifunctorLaws, observedTrifunctorLaws, profunctorLaws)
import Prelude

--------------------------------------------------------------------------------

genInt :: Gen Int
genInt = Gen.int (Range.linear (-100) 100)

-- Covariant product.
data Pair a = Pair a a
  deriving (Eq, Show)

$(deriveGenericK ''Pair)

instance CategoricalFunctor Pair where
  type Dom Pair = (->)
  type Cod Pair = (->)

genPair :: Gen a -> Gen (Pair a)
genPair g = Pair <$> g <*> g

-- Covariant nested container.
newtype Wrap a = Wrap [a]
  deriving (Eq, Show)

$(deriveGenericK ''Wrap)

instance CategoricalFunctor Wrap where
  type Dom Wrap = (->)
  type Cod Wrap = (->)

genWrap :: Gen a -> Gen (Wrap a)
genWrap g = Wrap <$> Gen.list (Range.linear 0 4) g

-- Contravariant: the parameter to the left of an arrow.
newtype Pred' a = Pred' (a -> Bool)

$(deriveGenericK ''Pred')

instance CategoricalFunctor Pred' where
  type Dom Pred' = Op
  type Cod Pred' = (->)

genPred' :: Gen (Pred' Int)
genPred' = (\n -> Pred' (> n)) <$> genInt

obsPred' :: Pred' Int -> Int -> Bool
obsPred' (Pred' p) = p

-- Covariant with the parameter to the right of an arrow.
data Box a = Box a (Int -> a)

$(deriveGenericK ''Box)

instance CategoricalFunctor Box where
  type Dom Box = (->)
  type Cod Box = (->)

boxArrowCodomain :: Property
boxArrowCodomain = property $ do
  n <- forAll genInt
  let Box v f = K.fmap (show :: Int -> String) (Box n (+ n))
  v === show n
  f 3 === show (3 + n)

-- Invariant: the parameter on both sides of an arrow.
newtype Endo' a = Endo' (a -> a)

$(deriveGenericK ''Endo')

instance CategoricalFunctor Endo' where
  type Dom Endo' = Iso (->)
  type Cod Endo' = (->)

genEndo' :: Gen (Endo' Int)
genEndo' = (\n -> Endo' (+ n)) <$> genInt

obsEndo' :: Endo' Int -> Int -> Int
obsEndo' (Endo' f) = f

-- Invariant with mixed occurrences: contravariant in the arrow, covariant in
-- the list.
data Mix a = Mix (a -> Bool) [a]

$(deriveGenericK ''Mix)

instance CategoricalFunctor Mix where
  type Dom Mix = Iso (->)
  type Cod Mix = (->)

genMix :: Gen (Mix Int)
genMix = Mix . (\n -> (> n)) <$> genInt <*> Gen.list (Range.linear 0 4) genInt

obsMix :: Mix Int -> Int -> (Bool, [Int])
obsMix (Mix p xs) a = (p a, xs)

-- Covariant bifunctor.
data BiT a b = BiT a b
  deriving (Eq, Show)

$(deriveGenericK ''BiT)

instance CategoricalFunctor (BiT a) where
  type Dom (BiT a) = (->)
  type Cod (BiT a) = (->)

instance CategoricalFunctor BiT where
  type Dom BiT = (->)
  type Cod BiT = (->) ~> (->)

genBiT :: Gen a -> Gen b -> Gen (BiT a b)
genBiT ga gb = BiT <$> ga <*> gb

-- Profunctor: contravariant in the first argument, covariant in the second.
newtype ProT a b = ProT (a -> b)

$(deriveGenericK ''ProT)

instance CategoricalFunctor (ProT a) where
  type Dom (ProT a) = (->)
  type Cod (ProT a) = (->)

instance CategoricalFunctor ProT where
  type Dom ProT = Op
  type Cod ProT = (->) ~> (->)

genProT :: Gen (ProT Int Int)
genProT = (\n -> ProT (+ n)) <$> genInt

obsProT :: ProT Int Int -> Int -> Int
obsProT (ProT f) = f

-- Covariant first, contravariant second (a `Bifunctor (->) Op`).
data CovCon a b = CovCon a (b -> Int)

$(deriveGenericK ''CovCon)

instance CategoricalFunctor (CovCon a) where
  type Dom (CovCon a) = Op
  type Cod (CovCon a) = (->)

instance CategoricalFunctor CovCon where
  type Dom CovCon = (->)
  type Cod CovCon = Op ~> (->)

genCovCon :: Gen (CovCon Int Int)
genCovCon = CovCon <$> genInt <*> ((\n b -> b * 2 + n) <$> genInt)

obsCovCon :: CovCon Int Int -> Int -> (Int, Int)
obsCovCon (CovCon a f) x = (a, f x)

-- Contravariant in both arguments (an Op/Op bifunctor).
data ConCon a b = ConCon (a -> Int) (b -> Int)

$(deriveGenericK ''ConCon)

instance CategoricalFunctor (ConCon a) where
  type Dom (ConCon a) = Op
  type Cod (ConCon a) = (->)

instance CategoricalFunctor ConCon where
  type Dom ConCon = Op
  type Cod ConCon = Op ~> (->)

conConProp :: Property
conConProp = property $ do
  n <- forAll genInt
  let ConCon f1 g1 = map2 (Op ((+ 1) :: Int -> Int)) (ConCon (* 2) (+ n))
  f1 3 === (3 + 1) * 2
  g1 5 === 5 + n
  let ConCon f2 g2 = map1 (Op ((+ 3) :: Int -> Int)) (ConCon (* 2) (+ n))
  f2 3 === 3 * 2
  g2 5 === (5 + 3) + n

-- Invariant first, covariant second.
data InvCov a b = InvCov (a -> a) b

$(deriveGenericK ''InvCov)

instance CategoricalFunctor (InvCov a) where
  type Dom (InvCov a) = (->)
  type Cod (InvCov a) = (->)

instance CategoricalFunctor InvCov where
  type Dom InvCov = Iso (->)
  type Cod InvCov = (->) ~> (->)

invCovProp :: Property
invCovProp = property $ do
  n <- forAll genInt
  let InvCov g1 b1 = map2 (Iso (+ (1 :: Int)) (subtract 1)) (InvCov (* 2) n)
  g1 3 === ((3 - 1) * 2) + 1
  b1 === n
  let InvCov g2 b2 = map1 ((+ 10) :: Int -> Int) (InvCov ((* 2) :: Int -> Int) n)
  g2 4 === 4 * 2
  b2 === n + 10

-- Covariant trifunctor. map3 hits the first argument, map2 the second (through
-- the bifunctor partial application), map1 the third.
data Tri a b c = Tri a b c
  deriving (Eq, Show)

$(deriveGenericK ''Tri)

instance CategoricalFunctor (Tri a b) where
  type Dom (Tri a b) = (->)
  type Cod (Tri a b) = (->)

instance CategoricalFunctor (Tri a) where
  type Dom (Tri a) = (->)
  type Cod (Tri a) = (->) ~> (->)

instance CategoricalFunctor Tri where
  type Dom Tri = (->)
  type Cod Tri = (->) ~> (->) ~> (->)

genTriBi :: Gen a -> Gen b -> Gen (Tri Int a b)
genTriBi ga gb = Tri <$> genInt <*> ga <*> gb

obsTri :: Tri Int Int Int -> Int -> (Int, Int, Int)
obsTri (Tri a b c) _ = (a, b, c)

-- Contravariant-first trifunctor, exercising the nested-Nat contravariant path.
data TriC a b c = TriC (a -> Int) b c

$(deriveGenericK ''TriC)

instance CategoricalFunctor (TriC a b) where
  type Dom (TriC a b) = (->)
  type Cod (TriC a b) = (->)

instance CategoricalFunctor (TriC a) where
  type Dom (TriC a) = (->)
  type Cod (TriC a) = (->) ~> (->)

instance CategoricalFunctor TriC where
  type Dom TriC = Op
  type Cod TriC = (->) ~> (->) ~> (->)

triCProp :: Property
triCProp = property $ do
  n <- forAll genInt
  let TriC f b c = map3 (Op ((+ 1) :: Int -> Int)) (TriC (* 2) n (n + 1))
  f 3 === (3 + 1) * 2
  b === n
  c === n + 1

-- Sum types with a nullary constructor, one per variance, to exercise the
-- interpreters' @:+:@ and @U1@ cases.

data Sum3 a = S3None | S3One a | S3Two a a
  deriving (Eq, Show)

$(deriveGenericK ''Sum3)

instance CategoricalFunctor Sum3 where
  type Dom Sum3 = (->)
  type Cod Sum3 = (->)

genSum3 :: Gen a -> Gen (Sum3 a)
genSum3 g = Gen.choice [pure S3None, S3One <$> g, S3Two <$> g <*> g]

data ConSum a = CSNone | CSOne (a -> Bool) | CSTwo (a -> Int) (a -> Bool)

$(deriveGenericK ''ConSum)

instance CategoricalFunctor ConSum where
  type Dom ConSum = Op
  type Cod ConSum = (->)

genConSum :: Gen (ConSum Int)
genConSum = Gen.choice [pure CSNone, (\n -> CSOne (> n)) <$> genInt, (\n -> CSTwo (+ n) (> n)) <$> genInt]

obsConSum :: ConSum Int -> Int -> (Bool, Int, Bool)
obsConSum CSNone _ = (False, 0, False)
obsConSum (CSOne p) x = (p x, 0, False)
obsConSum (CSTwo f p) x = (False, f x, p x)

data InvSum a = ISNone | ISOne (a -> a)

$(deriveGenericK ''InvSum)

instance CategoricalFunctor InvSum where
  type Dom InvSum = Iso (->)
  type Cod InvSum = (->)

genInvSum :: Gen (InvSum Int)
genInvSum = Gen.choice [pure ISNone, (\n -> ISOne (+ n)) <$> genInt]

obsInvSum :: InvSum Int -> Int -> Int
obsInvSum ISNone x = x
obsInvSum (ISOne f) x = f x

-- Invariant-first trifunctor.
data TriI a b c = TriI (a -> a) b c

$(deriveGenericK ''TriI)

instance CategoricalFunctor (TriI a b) where
  type Dom (TriI a b) = (->)
  type Cod (TriI a b) = (->)

instance CategoricalFunctor (TriI a) where
  type Dom (TriI a) = (->)
  type Cod (TriI a) = (->) ~> (->)

instance CategoricalFunctor TriI where
  type Dom TriI = Iso (->)
  type Cod TriI = (->) ~> (->) ~> (->)

triIProp :: Property
triIProp = property $ do
  n <- forAll genInt
  let TriI f b c = map3 (Iso (+ (1 :: Int)) (subtract 1)) (TriI (* 2) n (n + 1))
  f 3 === ((3 - 1) * 2) + 1
  b === n
  c === n + 1

--------------------------------------------------------------------------------

labeled :: String -> Laws -> [(PropertyName, Property)]
labeled prefix ls = [(fromString (prefix <> " " <> n), p) | (n, p) <- lawsProperties ls]

tests :: IO Bool
tests =
  checkSequential $
    Group "Generic deriving" $
      concat
        [ labeled "Pair (product)" (functorLaws genPair),
          labeled "Wrap [] (nested)" (functorLaws genWrap),
          labeled "Pred' (a -> Bool)" (contravariantFunctorLaws genPred' obsPred'),
          labeled "Endo' (a -> a)" (invariantFunctorLaws genEndo' obsEndo'),
          labeled "Mix (a -> Bool, [a])" (invariantFunctorLaws genMix obsMix),
          labeled "BiT (bifunctor, map2)" (bifunctorLaws genBiT),
          labeled "BiT Int (bifunctor, map1)" (functorLaws (genBiT genInt)),
          labeled "ProT (profunctor, map2)" (profunctorLaws genProT obsProT),
          labeled "CovCon ((->)/Op bifunctor, map2)" (observedBifunctorLaws genCovCon obsCovCon),
          labeled "CovCon Int (contravariant second)" (contravariantFunctorLaws genCovCon obsCovCon),
          labeled "Tri (trifunctor, map3)" (observedTrifunctorLaws (genTriBi genInt genInt) obsTri),
          labeled "Tri Int (trifunctor, map2)" (bifunctorLaws genTriBi),
          labeled "Tri Int Int (trifunctor, map1)" (functorLaws (genTriBi genInt)),
          labeled "Sum3 (covariant sum + nullary)" (functorLaws genSum3),
          labeled "ConSum (contravariant sum + nullary)" (contravariantFunctorLaws genConSum obsConSum),
          labeled "InvSum (invariant sum + nullary)" (invariantFunctorLaws genInvSum obsInvSum)
        ]
        ++ [ (fromString "Box (Int -> a) covariant arrow codomain", boxArrowCodomain),
             (fromString "ConCon (Op/Op bifunctor)", conConProp),
             (fromString "InvCov (invariant/covariant bifunctor)", invCovProp),
             (fromString "TriC (contravariant-first trifunctor)", triCProp),
             (fromString "TriI (invariant-first trifunctor)", triIProp)
           ]
