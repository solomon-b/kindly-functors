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
import Data.Isomorphism (Iso (Iso))
import Data.Maybe (maybeToList)
import Data.Monoid (Endo (..))
import Data.Profunctor (Star (..))
import GenericSpec qualified
import Kindly qualified as UUT
import LawsSpec qualified
import Rank2LawsSpec qualified
import System.Exit (exitFailure)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (defaultConfig, hspecWithResult, summaryFailures)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  summary <- hspecWithResult defaultConfig exampleSpec
  lawsOk <- LawsSpec.tests
  rank2Ok <- Rank2LawsSpec.tests
  genOk <- GenericSpec.tests
  when (summaryFailures summary > 0 || not lawsOk || not rank2Ok || not genOk) exitFailure

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
      runKleisli (UUT.fmap show (Kleisli Identity)) (5 :: Int) `shouldBe` Identity "5"

  describe "invmap" $ do
    it "works invariantly (Endo)" $ do
      appEndo (UUT.invmap (+ 1) (subtract 1) (Endo (* 2))) (5 :: Int) `shouldBe` 9
    it "works covariantly (Identity), dropping the backward leg" $ do
      UUT.invmap (show :: Int -> String) (read :: String -> Int) (Identity (5 :: Int)) `shouldBe` Identity "5"
    it "works contravariantly (Predicate), dropping the forward leg" $ do
      getPredicate (UUT.invmap (show :: Int -> String) (read :: String -> Int) (Predicate even)) "4" `shouldBe` True
      getPredicate (UUT.invmap (show :: Int -> String) (read :: String -> Int) (Predicate even)) "5" `shouldBe` False

  describe "mapIso" $ do
    it "maps an iso through a covariant functor (Identity)" $ do
      UUT.mapIso (Iso (show :: Int -> String) (read :: String -> Int)) (Identity (5 :: Int)) `shouldBe` Identity "5"
    it "maps an iso through a contravariant functor (Predicate)" $ do
      getPredicate (UUT.mapIso (Iso (show :: Int -> String) (read :: String -> Int)) (Predicate even)) "4" `shouldBe` True
    it "maps an iso through an invariant functor (Endo)" $ do
      appEndo (UUT.mapIso (Iso (+ 1) (subtract 1)) (Endo (* 2))) (5 :: Int) `shouldBe` 9

  describe "liftIso (Star)" $ do
    it "wraps the forward leg in pure, dropping the backward leg" $ do
      runStar (UUT.liftIso (Iso (show :: Int -> String) (read :: String -> Int)) :: Star Maybe Int String) 5 `shouldBe` Just "5"
    it "sends an identity iso to the Kleisli identity" $ do
      runStar (UUT.liftIso (Iso id id :: Iso (->) Int Int) :: Star Maybe Int Int) 5 `shouldBe` Just 5

  describe "liftIso (Kleisli)" $ do
    it "wraps the forward leg in pure, dropping the backward leg" $ do
      runKleisli (UUT.liftIso (Iso (show :: Int -> String) (read :: String -> Int)) :: Kleisli Maybe Int String) 5 `shouldBe` Just "5"

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

  describe "bmap1" $ do
    it "maps the rightmost functor" $ do
      let hkd = MyHKD (Just True) Nothing
      project (UUT.bmap1 maybeToList hkd) `shouldBe` ([True], [])

  describe "bmap2 / bmap1 on a two-functor HKD" $ do
    it "bmap2 hits the first parameter" $ do
      projeH2a (UUT.bmap2 maybeToList (MyHKD2 (Just True) (Just 1))) `shouldBe` ([True], Just (1 :: Int))
    it "bmap1 hits the second parameter" $ do
      projeH2b (UUT.bmap1 maybeToList (MyHKD2 (Just True) (Just 1))) `shouldBe` (Just True, [1 :: Int])

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

data MyHKD2 f g = MyHKD2 (f Bool) (g Int)

projeH2a :: MyHKD2 [] Maybe -> ([Bool], Maybe Int)
projeH2a (MyHKD2 a b) = (a, b)

projeH2b :: MyHKD2 Maybe [] -> (Maybe Bool, [Int])
projeH2b (MyHKD2 a b) = (a, b)

instance UUT.CategoricalFunctor (MyHKD2 f) where
  type Dom (MyHKD2 f) = (->) UUT.~> (->)
  type Cod (MyHKD2 f) = (->)

  map (UUT.Nat nat) (MyHKD2 a b) = MyHKD2 a (nat b)

instance UUT.CategoricalFunctor MyHKD2 where
  type Dom MyHKD2 = (->) UUT.~> (->)
  type Cod MyHKD2 = ((->) UUT.~> (->)) UUT.~> (->)

  map (UUT.Nat nat) = UUT.Nat (\(MyHKD2 a b) -> MyHKD2 (nat a) b)
