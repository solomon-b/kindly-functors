module Kindly.Class where

--------------------------------------------------------------------------------

import Control.Category
import Data.Kind (Constraint)
import Data.Semigroupoid (Semigroupoid (..))
import GHC.Base (Type)

--------------------------------------------------------------------------------

type CategoricalFunctor :: (from -> to) -> Constraint
class (Category (Dom f), Category (Cod f)) => CategoricalFunctor (f :: from -> to) where
  type Dom f :: from -> from -> Type
  type Cod f :: to -> to -> Type

  map :: Dom f a b -> Cod f (f a) (f b)

type Cat i = i -> i -> Type

type Nat :: Cat s -> Cat t -> Cat (s -> t)
newtype Nat source target f f' where
  Nat :: (forall x. target (f x) (f' x)) -> Nat source target f f'

runNat :: Nat source target f f' -> (forall x. target (f x) (f' x))
runNat (Nat f) = f

infixr 0 ~>

type (~>) c1 c2 = Nat c1 c2

instance (Semigroupoid c1, Semigroupoid c2) => Semigroupoid (Nat c1 c2) where
  o :: Nat c1 c2 j k1 -> Nat c1 c2 i j -> Nat c1 c2 i k1
  Nat c1 `o` Nat c2 = Nat (c1 `o` c2)

instance (Semigroupoid c1, Semigroupoid c2, Category c1, Category c2) => Category (c1 ~> c2) where
  id :: (c1 ~> c2) a a
  id = Nat id

  (.) = o

type FunctorOf :: Cat from -> Cat to -> (from -> to) -> Constraint
class (CategoricalFunctor f, dom ~ Dom f, cod ~ Cod f) => FunctorOf dom cod f

instance (CategoricalFunctor f, dom ~ Dom f, cod ~ Cod f) => FunctorOf dom cod f

--------------------------------------------------------------------------------
-- NOTE: These these classes go from right to left:

class (FunctorOf cat1 (->) p) => MapArg1 cat1 p | p -> cat1 where
  map1 :: (a `cat1` b) -> p a -> p b
  map1 = map

class (FunctorOf cat1 (cat2 ~> (->)) p, forall x. MapArg1 cat2 (p x)) => MapArg2 cat1 cat2 p | p -> cat2 cat2 where
  map2 :: (a `cat1` b) -> forall x. p a x -> p b x
  map2 = runNat . map

class (FunctorOf cat1 (cat2 ~> cat3 ~> (->)) p, forall x. MapArg2 cat2 cat3 (p x)) => MapArg3 cat1 cat2 cat3 p | p -> cat1 cat2 cat3 where
  map3 :: (a `cat1` b) -> forall x y. p a x y -> p b x y
  map3 f = runNat (runNat (map f))
