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

import Control.Applicative (WrappedArrow (..))
import Control.Applicative.Lift (Lift (..))
import Control.Arrow (Kleisli (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Bifunctor.Biff (Biff (..))
import Data.Bifunctor.Clown (Clown (..))
import Data.Bifunctor.Fix (Fix (..))
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
import Data.Graph (SCC (..))
import Data.IntMap qualified as IntMap
import Data.List.NonEmpty (NonEmpty)
import Data.Map qualified as Map
import Data.Monoid (Endo (..))
import Data.Profunctor (Costar (..), Forget (..), Star (..))
import Data.Profunctor.Cayley (Cayley (..))
import Data.Profunctor.Choice (CopastroSum (..), CotambaraSum (..), PastroSum (..), TambaraSum (..))
import Data.Profunctor.Closed (Closure (..), Environment (..))
import Data.Profunctor.Composition (Procompose (..), Rift (..))
import Data.Profunctor.Mapping (CofreeMapping (..), FreeMapping (..))
import Data.Profunctor.Strong (Copastro (..), Cotambara (..), Pastro (..), Tambara (..))
import Data.Profunctor.Traversing (CofreeTraversing (..), FreeTraversing (..))
import Data.Profunctor.Yoneda (Coyoneda (..), Yoneda (..))
import Data.Semigroupoid.Dual (Dual (..))
import Data.Sequence qualified as Seq
import Data.String (fromString)
import Data.Tagged (Tagged (..))
import Data.Tree (Tree (..))
import GHC.Generics (K1 (..), Par1 (..), Rec1 (..), (:*:) (..))
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
    observedBifunctorLaws,
    observedTrifunctorLaws,
    profunctorLaws,
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

-- Profunctor witnesses, observed by running since they are function-shaped.

genFn :: Gen (Int -> Int)
genFn = (\n x -> x * 2 + n) <$> genInt

obsFn :: (Int -> Int) -> Int -> Int
obsFn g = g

genStar :: Gen (Star Maybe Int Int)
genStar = (\n -> Star (\x -> if x > n then Just (x + n) else Nothing)) <$> genInt

obsStar :: Star Maybe Int Int -> Int -> Maybe Int
obsStar (Star g) = g

genCostar :: Gen (Costar Maybe Int Int)
genCostar = (\n -> Costar (maybe n (+ n))) <$> genInt

obsCostar :: Costar Maybe Int Int -> Int -> (Int, Int)
obsCostar (Costar g) a = (g (Just a), g Nothing)

genForget :: Gen (Forget Int Int Int)
genForget = (\n -> Forget (* n)) <$> genInt

obsForget :: Forget Int Int Int -> Int -> Int
obsForget (Forget g) = g

genKleisli :: Gen (Kleisli Maybe Int Int)
genKleisli = (\n -> Kleisli (\x -> if x > n then Just (x - n) else Nothing)) <$> genInt

obsKleisli :: Kleisli Maybe Int Int -> Int -> Maybe Int
obsKleisli (Kleisli g) = g

genWrappedArrow :: Gen (WrappedArrow (->) Int Int)
genWrappedArrow = (\n -> WrapArrow (+ n)) <$> genInt

obsWrappedArrow :: WrappedArrow (->) Int Int -> Int -> Int
obsWrappedArrow (WrapArrow g) = g

genProcompose :: Gen (Procompose (->) (->) Int Int)
genProcompose = (\n m -> Procompose (+ n) (* m)) <$> genInt <*> genInt

obsProcompose :: Procompose (->) (->) Int Int -> Int -> Int
obsProcompose (Procompose g h) = g . h

genRift :: Gen (Rift (->) (->) Int Int)
genRift = (\n -> Rift (\g -> g . (+ n))) <$> genInt

obsRift :: Rift (->) (->) Int Int -> Int -> Int
obsRift r = runRift r (* 2)

genYoneda :: Gen (Yoneda (->) Int Int)
genYoneda = (\n -> Yoneda (\l r -> r . (+ n) . l)) <$> genInt

obsYoneda :: Yoneda (->) Int Int -> Int -> Int
obsYoneda y = runYoneda y id id

genCoyoneda :: Gen (Coyoneda (->) Int Int)
genCoyoneda = (\n -> Coyoneda id id (+ n)) <$> genInt

obsCoyoneda :: Coyoneda (->) Int Int -> Int -> Int
obsCoyoneda (Coyoneda l r g) = r . g . l

genCayley :: Gen (Cayley Maybe (->) Int Int)
genCayley = Cayley <$> genMaybe ((+) <$> genInt)

obsCayley :: Cayley Maybe (->) Int Int -> Int -> Maybe Int
obsCayley (Cayley mf) a = fmap ($ a) mf

genTambara :: Gen (Tambara (->) Int Int)
genTambara = (\n -> Tambara (\(a, c) -> (a + n, c))) <$> genInt

obsTambara :: Tambara (->) Int Int -> Int -> Int
obsTambara (Tambara t) a = fst (t (a, ()))

genPastro :: Gen (Pastro (->) Int Int)
genPastro = (\n k -> Pastro (\(y, z) -> y + z) (* n) (\a -> (a, k))) <$> genInt <*> genInt

obsPastro :: Pastro (->) Int Int -> Int -> Int
obsPastro (Pastro l m r) a = case r a of (x, z) -> l (m x, z)

genCotambara :: Gen (Cotambara (->) Int Int)
genCotambara = (\n -> Cotambara id (+ n)) <$> genInt

obsCotambara :: Cotambara (->) Int Int -> Int -> Int
obsCotambara (Cotambara n r) = n r

genCopastro :: Gen (Copastro (->) Int Int)
genCopastro = (\n -> Copastro (\k -> k (+ n))) <$> genInt

obsCopastro :: Copastro (->) Int Int -> Int -> Int
obsCopastro (Copastro g) = g id

genTambaraSum :: Gen (TambaraSum (->) Int Int)
genTambaraSum = (\n -> TambaraSum (either (Left . (+ n)) Right)) <$> genInt

obsTambaraSum :: TambaraSum (->) Int Int -> Int -> Either Int ()
obsTambaraSum (TambaraSum t) a = t (Left a)

genPastroSum :: Gen (PastroSum (->) Int Int)
genPastroSum = (\n -> PastroSum (either id id) (* n) Left) <$> genInt

obsPastroSum :: PastroSum (->) Int Int -> Int -> Int
obsPastroSum (PastroSum l m r) a = case r a of
  Left x -> l (Left (m x))
  Right z -> l (Right z)

genCotambaraSum :: Gen (CotambaraSum (->) Int Int)
genCotambaraSum = (\n -> CotambaraSum id (+ n)) <$> genInt

obsCotambaraSum :: CotambaraSum (->) Int Int -> Int -> Int
obsCotambaraSum (CotambaraSum n r) = n r

genCopastroSum :: Gen (CopastroSum (->) Int Int)
genCopastroSum = (\n -> CopastroSum (\k -> k (+ n))) <$> genInt

obsCopastroSum :: CopastroSum (->) Int Int -> Int -> Int
obsCopastroSum (CopastroSum g) = g id

genClosure :: Gen (Closure (->) Int Int)
genClosure = (\n -> Closure (\g x -> g x + n)) <$> genInt

obsClosure :: Closure (->) Int Int -> Int -> Int
obsClosure (Closure t) = t (* 2)

genEnvironment :: Gen (Environment (->) Int Int)
genEnvironment = (\n -> Environment ($ n) (* 2) (+)) <$> genInt

obsEnvironment :: Environment (->) Int Int -> Int -> Int
obsEnvironment (Environment l m r) a = l (m . r a)

genFreeTraversing :: Gen (FreeTraversing (->) Int Int)
genFreeTraversing = (\n -> FreeTraversing sum (* n) (\a -> [a, a + 1])) <$> genInt

obsFreeTraversing :: FreeTraversing (->) Int Int -> Int -> Int
obsFreeTraversing (FreeTraversing l m r) a = l (fmap m (r a))

genCofreeTraversing :: Gen (CofreeTraversing (->) Int Int)
genCofreeTraversing = (\n -> CofreeTraversing (fmap (+ n))) <$> genInt

obsCofreeTraversing :: CofreeTraversing (->) Int Int -> Int -> [Int]
obsCofreeTraversing (CofreeTraversing t) a = t [a, a + 1]

genFreeMapping :: Gen (FreeMapping (->) Int Int)
genFreeMapping = (\n -> FreeMapping sum (* n) (\a -> [a, a + 1])) <$> genInt

obsFreeMapping :: FreeMapping (->) Int Int -> Int -> Int
obsFreeMapping (FreeMapping l m r) a = l (fmap m (r a))

genCofreeMapping :: Gen (CofreeMapping (->) Int Int)
genCofreeMapping = (\n -> CofreeMapping (fmap (+ n))) <$> genInt

obsCofreeMapping :: CofreeMapping (->) Int Int -> Int -> [Int]
obsCofreeMapping (CofreeMapping t) a = t [a, a + 1]

genTagged :: Gen a -> Gen (Tagged () a)
genTagged g = Tagged <$> g

genTaggedP :: Gen (Tagged Int Int)
genTaggedP = Tagged <$> genInt

obsTaggedP :: Tagged Int Int -> Int -> Int
obsTaggedP (Tagged b) _ = b

genFix :: Gen a -> Gen (Fix Either a)
genFix g = Gen.recursive Gen.choice [In . Right <$> g] [In . Left <$> genFix g]

genMap :: Gen a -> Gen (Map.Map Int a)
genMap g = Map.fromList <$> genList ((,) <$> genInt <*> g)

genIntMap :: Gen a -> Gen (IntMap.IntMap a)
genIntMap g = IntMap.fromList <$> genList ((,) <$> genInt <*> g)

genSeq :: Gen a -> Gen (Seq.Seq a)
genSeq g = Seq.fromList <$> genList g

genViewL :: Gen a -> Gen (Seq.ViewL a)
genViewL g = Seq.viewl <$> genSeq g

genViewR :: Gen a -> Gen (Seq.ViewR a)
genViewR g = Seq.viewr <$> genSeq g

genTree :: Gen a -> Gen (Tree a)
genTree g = Gen.recursive Gen.choice [Node <$> g <*> pure []] [Node <$> g <*> Gen.list (Range.linear 0 3) (genTree g)]

genSCC :: Gen a -> Gen (SCC a)
genSCC g = Gen.choice [AcyclicSCC <$> g, CyclicSCC <$> Gen.list (Range.linear 1 4) g]

genConstantT :: Gen a -> Gen b -> Gen (Constant a b)
genConstantT ga _ = Constant <$> ga

genDual :: Gen (Dual (->) Int Int)
genDual = (\n -> Dual (+ n)) <$> genInt

obsDual :: Dual (->) Int Int -> Int -> Int
obsDual (Dual g) = g

genTriple :: Gen (Int, Int, Int)
genTriple = (,,) <$> genInt <*> genInt <*> genInt

obsTriple :: (Int, Int, Int) -> Int -> (Int, Int, Int)
obsTriple t _ = t

genForgetT :: Gen (Forget Int Int Int)
genForgetT = (\n -> Forget (* n)) <$> genInt

obsForgetT :: Forget Int Int Int -> Int -> Int
obsForgetT (Forget g) = g

genK1 :: Gen (K1 Int Int Int)
genK1 = K1 <$> genInt

obsK1 :: K1 Int Int Int -> Int -> Int
obsK1 (K1 c) _ = c

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
          labeled "WrappedBifunctor (,) Int" (functorLaws (genWrappedT genInt)),
          -- Profunctors (map2 at Op).
          labeled "(->)" (profunctorLaws genFn obsFn),
          labeled "Star Maybe" (profunctorLaws genStar obsStar),
          labeled "Costar Maybe" (profunctorLaws genCostar obsCostar),
          labeled "Forget Int" (profunctorLaws genForget obsForget),
          labeled "Kleisli Maybe" (profunctorLaws genKleisli obsKleisli),
          labeled "WrappedArrow (->)" (profunctorLaws genWrappedArrow obsWrappedArrow),
          labeled "Procompose (->) (->)" (profunctorLaws genProcompose obsProcompose),
          labeled "Rift (->) (->)" (profunctorLaws genRift obsRift),
          labeled "Yoneda (->)" (profunctorLaws genYoneda obsYoneda),
          labeled "Coyoneda (->)" (profunctorLaws genCoyoneda obsCoyoneda),
          labeled "Cayley Maybe (->)" (profunctorLaws genCayley obsCayley),
          labeled "Tambara (->)" (profunctorLaws genTambara obsTambara),
          labeled "Pastro (->)" (profunctorLaws genPastro obsPastro),
          labeled "Cotambara (->)" (profunctorLaws genCotambara obsCotambara),
          labeled "Copastro (->)" (profunctorLaws genCopastro obsCopastro),
          labeled "TambaraSum (->)" (profunctorLaws genTambaraSum obsTambaraSum),
          labeled "PastroSum (->)" (profunctorLaws genPastroSum obsPastroSum),
          labeled "CotambaraSum (->)" (profunctorLaws genCotambaraSum obsCotambaraSum),
          labeled "CopastroSum (->)" (profunctorLaws genCopastroSum obsCopastroSum),
          labeled "Closure (->)" (profunctorLaws genClosure obsClosure),
          labeled "Environment (->)" (profunctorLaws genEnvironment obsEnvironment),
          labeled "FreeTraversing (->)" (profunctorLaws genFreeTraversing obsFreeTraversing),
          labeled "CofreeTraversing (->)" (profunctorLaws genCofreeTraversing obsCofreeTraversing),
          labeled "FreeMapping (->)" (profunctorLaws genFreeMapping obsFreeMapping),
          labeled "CofreeMapping (->)" (profunctorLaws genCofreeMapping obsCofreeMapping),
          labeled "Tagged ()" (functorLaws genTagged),
          labeled "Tagged" (profunctorLaws genTaggedP obsTaggedP),
          -- Op as a bifunctor into Op: covariant map2, contravariant map1.
          labeled "Op" (observedBifunctorLaws genOp obsOp),
          labeled "Fix Either" (functorLaws genFix),
          labeled "Map Int" (functorLaws genMap),
          labeled "IntMap" (functorLaws genIntMap),
          labeled "Seq" (functorLaws genSeq),
          labeled "ViewL" (functorLaws genViewL),
          labeled "ViewR" (functorLaws genViewR),
          labeled "Tree" (functorLaws genTree),
          labeled "SCC" (functorLaws genSCC),
          labeled "Constant" (bifunctorLaws genConstantT),
          labeled "Dual (->) Int" (contravariantFunctorLaws genDual obsDual),
          labeled "Dual (->)" (observedBifunctorLaws genDual obsDual),
          -- Trifunctors (map3).
          labeled "(,,)" (observedTrifunctorLaws genTriple obsTriple),
          labeled "Forget" (observedTrifunctorLaws genForgetT obsForgetT),
          labeled "K1" (observedTrifunctorLaws genK1 obsK1)
        ]
