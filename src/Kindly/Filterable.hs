module Kindly.Filterable where

--------------------------------------------------------------------------------

import Control.Category
import Control.Monad qualified as Hask
import Data.Bool (Bool (..))
import Data.Maybe (Maybe (..))
import Data.Profunctor qualified as Hask
import Kindly.Class
import Witherable qualified as Hask

--------------------------------------------------------------------------------

newtype FromFilterable f a = FromFilterable (f a)
  deriving newtype (Hask.Functor, Hask.Filterable)

instance (Hask.Filterable f) => CategoricalFunctor (FromFilterable f) where
  type Dom (FromFilterable f) = (Hask.Star Maybe)
  type Cod (FromFilterable f) = (->)

  map :: Hask.Star Maybe a b -> FromFilterable f a -> FromFilterable f b
  map (Hask.Star f) (FromFilterable fa) = FromFilterable (Hask.mapMaybe f fa)

mapMaybe :: (FunctorOf (Hask.Star Maybe) (->) f) => (a -> Maybe b) -> f a -> f b
mapMaybe f = map (Hask.Star f)

catMaybes :: (FunctorOf (Hask.Star Maybe) (->) f) => f (Maybe a) -> f a
catMaybes = map (Hask.Star id)

filter :: (FunctorOf (Hask.Star Maybe) (->) f) => (a -> Bool) -> f a -> f a
filter f = map (Hask.Star (\a -> if f a then Just a else Nothing))

--------------------------------------------------------------------------------

-- NOTE: These instances conflict with our Covariant Functor
-- instances. Switching from associated types to Multi Parameter type
-- classes would fix this:

-- deriving via (FromFilterable []) instance Functor []

-- deriving via (FromFilterable Maybe) instance Functor Maybe
