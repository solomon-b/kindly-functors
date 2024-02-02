module Kindly.Class where

--------------------------------------------------------------------------------

import Control.Category
import Data.Kind (Constraint)
import GHC.Base (Type)
import Data.Semigroupoid (Semigroupoid (..))

--------------------------------------------------------------------------------

type Functor :: (from -> to) -> Constraint
class (Category (Dom f), Category (Cod f)) => Functor (f :: from -> to) where
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
class (Functor f, dom ~ Dom f, cod ~ Cod f) => FunctorOf dom cod f

instance (Functor f, dom ~ Dom f, cod ~ Cod f) => FunctorOf dom cod f
