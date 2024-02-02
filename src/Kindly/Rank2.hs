-- | Work in Progress.
module Kindly.Rank2 where

--------------------------------------------------------------------------------

import Data.Bool (Bool (..))
import Kindly.Class

--------------------------------------------------------------------------------

data MyHKD f = MyHKD {one :: f Bool, two :: f ()}

instance Functor MyHKD where
  type Dom MyHKD = (->) ~> (->)
  type Cod MyHKD = (->)

  map :: (Nat (->) (->)) f g -> MyHKD f -> MyHKD g
  map (Nat nat) MyHKD {..} = MyHKD (nat one) (nat two)

newtype MyHKD2 p = MyHKD2 {field :: p () Bool}

instance Functor MyHKD2 where
  type Dom MyHKD2 = (->) ~> ((->) ~> (->))
  type Cod MyHKD2 = (->)

  map :: Dom MyHKD2 p q -> MyHKD2 p -> MyHKD2 q
  map (Nat (Nat nat)) MyHKD2 {..} = MyHKD2 (nat field)
