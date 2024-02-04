{-# OPTIONS_GHC -Wno-orphans #-}

module Kindly.Functor where

--------------------------------------------------------------------------------

import Control.Applicative (Const, WrappedArrow, WrappedMonad, ZipList)
import Control.Arrow (Arrow, ArrowMonad, Kleisli (..))
import Control.Category (Category (..))
import Control.Exception (Handler)
import Control.Monad qualified as Hask.Monad
import Control.Monad.ST (ST)
import Control.Monad.ST.Lazy qualified as Lazy
import Data.Complex (Complex)
import Data.Either (Either)
import Data.Functor qualified as Hask
import Data.Functor.Compose (Compose (..))
import Data.Functor.Contravariant (Op (..), Predicate)
import Data.Functor.Contravariant qualified as Hask
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Sum (Sum (..))
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe (..))
import Data.Monoid qualified as Monoid
import Data.Ord (Down)
import Data.Proxy (Proxy)
import Data.Semigroup qualified as Semigroup
import Data.These (These)
import Data.Tuple (Solo)
import Foreign (Ptr)
import GHC.Arr (Array)
import GHC.Base (Char, Double, IO, Int, Word, ($))
import GHC.Conc (STM)
import GHC.Exts (Float)
import GHC.Generics (K1, M1 (..), Par1, Rec1 (..), U1, URec, V1, (:*:) (..), (:+:) (..), (:.:) (..))
import Kindly.Class
import Kindly.Iso
import System.Console.GetOpt (ArgDescr, ArgOrder, OptDescr)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)

--------------------------------------------------------------------------------

type Endofunctor :: (Type -> Type -> Type) -> (Type -> Type) -> Constraint
type Endofunctor cat1 p = (MapArg1 cat1 p)

fmap :: forall cat1 p. (Endofunctor cat1 p) => forall a b. (a `cat1` b) -> p a -> p b
fmap = map1

-- TODO: Do I keep this around?
contramap :: (Endofunctor Op p) => (a -> b) -> p b -> p a
contramap = fmap . Op

-- TODO: Do I keep this around?
invmap :: (Endofunctor (<->) f) => (a -> b) -> (b -> a) -> f a -> f b
invmap f g = fmap (Iso f g)

--------------------------------------------------------------------------------

newtype FromFunctor f a = FromFunctor (f a)
  deriving newtype (Hask.Functor)

instance (Hask.Functor f) => Functor (FromFunctor f) where
  type Dom (FromFunctor f) = (->)
  type Cod (FromFunctor f) = (->)

  map :: (a -> b) -> FromFunctor f a -> FromFunctor f b
  map = Hask.fmap

--------------------------------------------------------------------------------
-- Covariant Functor instances

deriving via (FromFunctor ZipList) instance Functor ZipList

deriving via (FromFunctor Handler) instance Functor Handler

deriving via (FromFunctor Complex) instance Functor Complex

deriving via (FromFunctor Identity) instance Functor Identity

deriving via (FromFunctor Monoid.First) instance Functor Monoid.First

deriving via (FromFunctor Monoid.Last) instance Functor Monoid.Last

deriving via (FromFunctor Down) instance Functor Down

deriving via (FromFunctor Semigroup.First) instance Functor Semigroup.First

deriving via (FromFunctor Semigroup.Last) instance Functor Semigroup.Last

deriving via (FromFunctor Semigroup.Max) instance Functor Semigroup.Max

deriving via (FromFunctor Semigroup.Min) instance Functor Semigroup.Min

deriving via (FromFunctor Semigroup.Dual) instance Functor Semigroup.Dual

deriving via (FromFunctor Semigroup.Product) instance Functor Semigroup.Product

deriving via (FromFunctor Semigroup.Sum) instance Functor Semigroup.Sum

deriving via (FromFunctor NonEmpty) instance Functor NonEmpty

deriving via (FromFunctor STM) instance Functor STM

deriving via (FromFunctor Par1) instance Functor Par1

deriving via (FromFunctor ArgDescr) instance Functor ArgDescr

deriving via (FromFunctor ArgOrder) instance Functor ArgOrder

deriving via (FromFunctor OptDescr) instance Functor OptDescr

deriving via (FromFunctor ReadP) instance Functor ReadP

deriving via (FromFunctor ReadPrec) instance Functor ReadPrec

deriving via (FromFunctor IO) instance Functor IO

deriving via (FromFunctor Maybe) instance Functor Maybe

deriving via (FromFunctor Solo) instance Functor Solo

deriving via (FromFunctor []) instance Functor []

deriving via (FromFunctor (WrappedMonad m)) instance (Hask.Monad.Monad m) => Functor (WrappedMonad m)

deriving via (FromFunctor (ArrowMonad a)) instance (Arrow a) => Functor (ArrowMonad a)

deriving via (FromFunctor (Lazy.ST s)) instance Functor (Lazy.ST s)

deriving via (FromFunctor (Either a)) instance Functor (Either a)

deriving via (FromFunctor (These a)) instance Functor (These a)

deriving via (FromFunctor Proxy) instance Functor (Proxy :: Type -> Type)

deriving via (FromFunctor (Semigroup.Arg a)) instance Functor (Semigroup.Arg a)

deriving via (FromFunctor (Array i)) instance Functor (Array i)

deriving via (FromFunctor U1) instance Functor (U1 :: Type -> Type)

deriving via (FromFunctor V1) instance Functor (V1 :: Type -> Type)

deriving via (FromFunctor (ST s)) instance Functor (ST s)

deriving via (FromFunctor ((,) a)) instance Functor ((,) a)

deriving via (FromFunctor (WrappedArrow a b)) instance (Arrow a) => Functor (WrappedArrow a b)

instance (FunctorOf (->) (->) m) => Functor (Kleisli m a) where
  type Dom (Kleisli m a) = (->)
  type Cod (Kleisli m a) = (->)

  map :: (a1 -> b) -> Kleisli m a a1 -> Kleisli m a b
  map f (Kleisli m) = Kleisli $ \a -> map f (m a)

deriving via (FromFunctor (Const m)) instance Functor (Const m :: Type -> Type)

instance (FunctorOf (->) (->) f) => Functor (Monoid.Ap f) where
  type Dom (Monoid.Ap f) = (->)
  type Cod (Monoid.Ap f) = (->)

  map f (Monoid.Ap m) = Monoid.Ap $ map f m

instance (FunctorOf (->) (->) f) => Functor (Monoid.Alt f) where
  type Dom (Monoid.Alt f) = (->)
  type Cod (Monoid.Alt f) = (->)

  map f (Monoid.Alt m) = Monoid.Alt $ map f m

instance (FunctorOf (->) (->) f) => Functor (Rec1 f) where
  type Dom (Rec1 f) = (->)
  type Cod (Rec1 f) = (->)

  map f (Rec1 m) = Rec1 $ map f m

deriving via (FromFunctor (URec (Ptr ()))) instance Functor (URec (Ptr ()) :: Type -> Type)

deriving via (FromFunctor (URec Char)) instance Functor (URec Char :: Type -> Type)

deriving via (FromFunctor (URec Double)) instance Functor (URec Double :: Type -> Type)

deriving via (FromFunctor (URec Float)) instance Functor (URec Float :: Type -> Type)

deriving via (FromFunctor (URec Int)) instance Functor (URec Int :: Type -> Type)

deriving via (FromFunctor (URec Word)) instance Functor (URec Word :: Type -> Type)

deriving via (FromFunctor ((,,) a b)) instance Functor ((,,) a b)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => Functor (Product f g) where
  type Dom (Product f g) = (->)
  type Cod (Product f g) = (->)

  map f (Pair m1 m2) = Pair (map f m1) (map f m2)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => Functor (Sum f g) where
  type Dom (Sum f g) = (->)
  type Cod (Sum f g) = (->)

  map f (InL m1) = InL $ map f m1
  map f (InR m2) = InR $ map f m2

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => Functor (f :*: g) where
  type Dom (f :*: g) = (->)
  type Cod (f :*: g) = (->)

  map f (m1 :*: m2) = map f m1 :*: map f m2

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => Functor (f :+: g) where
  type Dom (f :+: g) = (->)
  type Cod (f :+: g) = (->)

  map f (L1 m1) = L1 $ map f m1
  map f (R1 m2) = R1 $ map f m2

deriving via (FromFunctor (K1 i c)) instance Functor (K1 i c :: Type -> Type)

deriving via (FromFunctor ((,,,) a b c)) instance Functor ((,,,) a b c)

deriving via (FromFunctor ((->) r)) instance Functor ((->) r)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => Functor (Compose f g) where
  type Dom (Compose f g) = (->)
  type Cod (Compose f g) = (->)

  map f (Compose fga) = Compose $ map (map f) fga

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => Functor (f :.: g) where
  type Dom (f :.: g) = (->)
  type Cod (f :.: g) = (->)

  map f (Comp1 fga) = Comp1 $ map (map f) fga

instance (FunctorOf (->) (->) f) => Functor (M1 i c f) where
  type Dom (M1 i c f) = (->)
  type Cod (M1 i c f) = (->)

  map f (M1 fp) = M1 $ map f fp

deriving via (FromFunctor ((,,,,) a b c d)) instance Functor ((,,,,) a b c d)

deriving via (FromFunctor ((,,,,,) a b c d e)) instance Functor ((,,,,,) a b c d e)

deriving via (FromFunctor ((,,,,,,) a b c d e f)) instance Functor ((,,,,,,) a b c d e f)

--------------------------------------------------------------------------------
-- Covariant MapArg1 instances

instance MapArg1 (->) ZipList

instance MapArg1 (->) Handler

instance MapArg1 (->) Complex

instance MapArg1 (->) Identity

instance MapArg1 (->) Monoid.First

instance MapArg1 (->) Monoid.Last

instance MapArg1 (->) Down

instance MapArg1 (->) Semigroup.First

instance MapArg1 (->) Semigroup.Last

instance MapArg1 (->) Semigroup.Max

instance MapArg1 (->) Semigroup.Min

instance MapArg1 (->) Semigroup.Dual

instance MapArg1 (->) Semigroup.Product

instance MapArg1 (->) Semigroup.Sum

instance MapArg1 (->) NonEmpty

instance MapArg1 (->) STM

instance MapArg1 (->) Par1

instance MapArg1 (->) ArgDescr

instance MapArg1 (->) ArgOrder

instance MapArg1 (->) OptDescr

instance MapArg1 (->) ReadP

instance MapArg1 (->) ReadPrec

instance MapArg1 (->) IO

instance MapArg1 (->) Maybe

instance MapArg1 (->) Solo

instance MapArg1 (->) []

instance (Hask.Monad.Monad m) => MapArg1 (->) (WrappedMonad m)

instance (Arrow a) => MapArg1 (->) (ArrowMonad a)

instance MapArg1 (->) (Lazy.ST s)

instance MapArg1 (->) (Either a)

instance MapArg1 (->) (Proxy :: Type -> Type)

instance MapArg1 (->) (Semigroup.Arg a)

instance MapArg1 (->) (Array i)

instance MapArg1 (->) (U1 :: Type -> Type)

instance MapArg1 (->) (V1 :: Type -> Type)

instance MapArg1 (->) (ST s)

instance MapArg1 (->) ((,) a)

instance (Arrow a) => MapArg1 (->) (WrappedArrow a b)

instance (FunctorOf (->) (->) m) => MapArg1 (->) (Kleisli m a)

instance MapArg1 (->) (Const m :: Type -> Type)

instance (FunctorOf (->) (->) f) => MapArg1 (->) (Monoid.Ap f)

instance (FunctorOf (->) (->) f) => MapArg1 (->) (Monoid.Alt f)

instance (FunctorOf (->) (->) f) => MapArg1 (->) (Rec1 f)

instance MapArg1 (->) (URec (Ptr ()) :: Type -> Type)

instance MapArg1 (->) (URec Char :: Type -> Type)

instance MapArg1 (->) (URec Double :: Type -> Type)

instance MapArg1 (->) (URec Float :: Type -> Type)

instance MapArg1 (->) (URec Int :: Type -> Type)

instance MapArg1 (->) (URec Word :: Type -> Type)

instance MapArg1 (->) ((,,) a b)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => MapArg1 (->) (Product f g)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => MapArg1 (->) (Sum f g)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => MapArg1 (->) (f :*: g)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => MapArg1 (->) (f :+: g)

instance MapArg1 (->) (K1 i c :: Type -> Type)

instance MapArg1 (->) ((,,,) a b c)

instance MapArg1 (->) ((->) r)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => MapArg1 (->) (Compose f g)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => MapArg1 (->) (f :.: g)

instance (FunctorOf (->) (->) f) => MapArg1 (->) (M1 i c f)

instance MapArg1 (->) ((,,,,) a b c d)

instance MapArg1 (->) ((,,,,,) a b c d e)

instance MapArg1 (->) ((,,,,,,) a b c d e f)

--------------------------------------------------------------------------------

newtype FromContra f a = FromContra {getContra :: f a}
  deriving newtype (Hask.Contravariant)

instance (Hask.Contravariant f) => Functor (FromContra f) where
  type Dom (FromContra f) = Op
  type Cod (FromContra f) = (->)

  map :: Dom (FromContra f) a b -> Cod (FromContra f) ((FromContra f) a) ((FromContra f) b)
  map = Hask.contramap . getOp

--------------------------------------------------------------------------------
-- Contravariant Functor instances

deriving via (FromContra Predicate) instance Functor Predicate

-- TODO: Add remaining Contravariant instances

--------------------------------------------------------------------------------
-- Contravariant MapArg1 instances

instance MapArg1 Op Predicate

-- TODO: Add remaining Contravariant instances

--------------------------------------------------------------------------------

instance Functor Monoid.Endo where
  type Dom Monoid.Endo = (<->)
  type Cod Monoid.Endo = (->)

  map :: (a <-> b) -> Monoid.Endo a -> Monoid.Endo b
  map Iso {..} (Monoid.Endo f) = Monoid.Endo (fwd . f . bwd)

instance MapArg1 (<->) Monoid.Endo
