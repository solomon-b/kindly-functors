module Kindly.Iso where

--------------------------------------------------------------------------------

import Control.Category (Category (..))
import Data.Kind (Type)
import Kindly.Class (Cat)

--------------------------------------------------------------------------------

-- | An invertible mapping between 'a' and 'b' in category 'cat'.
--
-- === Laws
--
-- @
-- 'fwd' '.' 'bwd' ≡ 'id'
-- 'bwd' '.' 'fwd' ≡ 'id'
-- @
data Iso cat a b = Iso {fwd :: a `cat` b, bwd :: b `cat` a}

instance (Category cat) => Category (Iso cat) where
  id :: (Category cat) => Iso cat a a
  id = Iso id id

  (.) :: Iso cat b c -> Iso cat a b -> Iso cat a c
  Iso fwd bwd . Iso fwd' bwd' = Iso (fwd . fwd') (bwd' . bwd)

type (<->) :: Cat Type
type (<->) = Iso (->)
