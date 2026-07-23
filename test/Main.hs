{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Main (main) where

--------------------------------------------------------------------------------

import Control.Arrow (Kleisli (..))
import Control.Monad (when)
import Data.Functor.Contravariant (Op (..), Predicate (..))
import Data.Functor.Identity (Identity (..))
import Data.Maybe (maybeToList)
import Data.Monoid (Endo (..))
import Kindly qualified as UUT
import LawsSpec qualified
import System.Exit (exitFailure)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (defaultConfig, hspecWithResult, summaryFailures)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig exampleSpec
  lawsOk <- LawsSpec.tests
  when (summaryFailures summary > 0 || not lawsOk) exitFailure

--------------------------------------------------------------------------------
-- Example-based (characterization) tests

exampleSpec :: Spec
exampleSpec = do
  describe "fmap" $ do
    it "works covariantly" $ do
      UUT.fmap show (Identity True) `shouldBe` Identity "True"
    it "works contravariantly" $ do
      getPredicate (UUT.fmap (Op read) (Predicate not)) "True" `shouldBe` False
    it "composes" $ do
      (UUT.fmap . UUT.fmap) show (Just (Just True)) `shouldBe` Just (Just "True")
      UUT.fmap ((\f -> f "True") . getPredicate) ((UUT.fmap . UUT.fmap) (Op read) (Just (Predicate not))) `shouldBe` Just False
    it "works over a constrained instance (Kleisli)" $ do
      runKleisli (UUT.fmap show (Kleisli (\x -> Identity x))) (5 :: Int) `shouldBe` Identity "5"

  describe "invmap" $ do
    it "works invariantly (Endo)" $ do
      appEndo (UUT.invmap (+ 1) (subtract 1) (Endo (* 2))) (5 :: Int) `shouldBe` 9

  describe "lmap" $ do
    it "works covariantly" $ do
      UUT.lmap show (True, False) `shouldBe` ("True", False)
    it "works contravariantly" $ do
      UUT.lmap (Op read) not "True" `shouldBe` False

  describe "rmap" $ do
    it "works covariantly" $ do
      UUT.rmap show (True, False) `shouldBe` (True, "False")

  describe "bimap" $ do
    it "works covariantly" $ do
      UUT.bimap show (read @()) (Left True) `shouldBe` Left "True"
      UUT.bimap (read @Int) show ("1", True) `shouldBe` (1, "True")
    it "works contravariantly" $ do
      UUT.bimap (Op (read @Int)) show (+ 1) "0" `shouldBe` "1"

  describe "trimap" $ do
    it "works covariantly" $ do
      UUT.trimap show show show (True, False, ()) `shouldBe` ("True", "False", "()")

  describe "bmap" $ do
    it "works" $ do
      let hkd = MyHKD (Just True) Nothing
      project (UUT.bmap maybeToList hkd) `shouldBe` ([True], [])

  describe "bmap2" $ do
    it "works" $ do
      field (UUT.bmap2 (\(a, _) -> Left a) (MyHKD2 ((), True))) `shouldBe` (Left () :: Either () Bool)

--------------------------------------------------------------------------------
-- Rank-2 witnesses

data MyHKD f = MyHKD {one :: f Bool, two :: f ()}

project :: MyHKD f -> (f Bool, f ())
project MyHKD {..} = (one, two)

instance UUT.CategoricalFunctor MyHKD where
  type Dom MyHKD = (->) UUT.~> (->)
  type Cod MyHKD = (->)

  map :: (UUT.Nat (->) (->)) f g -> MyHKD f -> MyHKD g
  map (UUT.Nat nat) MyHKD {..} = MyHKD (nat one) (nat two)

newtype MyHKD2 p = MyHKD2 {field :: p () Bool}

instance UUT.CategoricalFunctor MyHKD2 where
  type Dom MyHKD2 = (->) UUT.~> ((->) UUT.~> (->))
  type Cod MyHKD2 = (->)

  map :: UUT.Dom MyHKD2 p q -> MyHKD2 p -> MyHKD2 q
  map (UUT.Nat (UUT.Nat nat)) MyHKD2 {..} = MyHKD2 (nat field)
