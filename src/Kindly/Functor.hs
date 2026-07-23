{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Single Parameter Functors of arbitrary categories.
module Kindly.Functor
  ( Functor,
    fmap,
    contramap,
    invmap,
    Filterable,
    mapMaybe,
    catMaybes,
    filter,
  )
where

--------------------------------------------------------------------------------

import Control.Applicative (Const, WrappedArrow, WrappedMonad, ZipList)
import Control.Applicative.Backwards (Backwards (..))
import Control.Applicative.Lift (Lift (..))
import Control.Arrow (Arrow, ArrowMonad, Kleisli (..))
import Control.Category (Category (..))
import Control.Exception (Handler)
import Control.Monad (Monad)
import Control.Monad.ST (ST)
import Control.Monad.ST.Lazy qualified as Lazy
import Control.Monad.Trans.Accum (AccumT (..))
import Control.Monad.Trans.Cont (ContT (..))
import Control.Monad.Trans.Except (ExceptT (..))
import Control.Monad.Trans.Identity (IdentityT (..))
import Control.Monad.Trans.Maybe (MaybeT (..))
import Control.Monad.Trans.RWS.CPS qualified as CPS
import Control.Monad.Trans.RWS.Lazy qualified as Lazy
import Control.Monad.Trans.RWS.Strict qualified as Strict
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.Select (SelectT (..))
import Control.Monad.Trans.State.Lazy qualified as Lazy
import Control.Monad.Trans.State.Strict qualified as Strict
import Control.Monad.Trans.Writer.CPS qualified as CPS
import Control.Monad.Trans.Writer.Lazy qualified as Lazy
import Control.Monad.Trans.Writer.Strict qualified as Strict
import Data.Bifunctor.Biff (Biff (..))
import Data.Bifunctor.Clown (Clown (..))
import Data.Bifunctor.Flip (Flip (..))
import Data.Bifunctor.Joker (Joker (..))
import Data.Bifunctor.Product qualified as Bifunctor
import Data.Bifunctor.Sum qualified as Bifunctor
import Data.Bifunctor.Tannen (Tannen (..))
import Data.Bifunctor.Wrapped (WrappedBifunctor (..))
import Data.Complex (Complex)
import Data.Either (Either (..))
import Data.Functor qualified as Hask
import Data.Functor.Apply (MaybeApply (..), WrappedApplicative (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Constant (Constant)
import Data.Functor.Contravariant (Comparison, Equivalence, Op (..), Predicate)
import Data.Functor.Contravariant qualified as Hask
import Data.Functor.Identity (Identity (..))
import Data.Functor.Product (Product (..))
import Data.Functor.Reverse (Reverse (..))
import Data.Functor.Sum (Sum (..))
import Data.Functor.These (These1 (..))
import Data.Isomorphism
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (Maybe (..))
import Data.Monoid qualified as Monoid
import Data.Ord (Down)
import Data.Profunctor qualified as Hask.Profunctor
import Data.Profunctor.Cayley qualified as Hask.Profunctor
import Data.Profunctor.Choice qualified as Hask.Profunctor
import Data.Profunctor.Closed qualified as Hask.Profunctor
import Data.Profunctor.Composition qualified as Hask.Profunctor
import Data.Profunctor.Mapping qualified as Hask.Profunctor
import Data.Profunctor.Strong qualified as Hask.Profunctor
import Data.Profunctor.Traversing qualified as Hask.Profunctor
import Data.Profunctor.Yoneda qualified as Hask.Profunctor
import Data.Proxy (Proxy)
import Data.Semigroup qualified as Semigroup
import Data.Semigroupoid.Static (Static (..))
import Data.Tagged (Tagged)
import Data.These (These)
#if MIN_VERSION_base(4,16,0)
import Data.Tuple (Solo)
#endif
import Foreign (Ptr)
import GHC.Arr (Array)
import GHC.Base (Char, Double, IO, Int, Word, ($))
import GHC.Conc (STM)
import GHC.Exts (Float)
import GHC.Generics (K1, M1 (..), Par1, Rec1 (..), U1, URec, V1, (:*:) (..), (:+:) (..), (:.:) (..))
#if MIN_VERSION_base(4,17,0)
import GHC.Generics (Generic1, Generically1, Rep1)
#endif
import Kindly.Class
import System.Console.GetOpt (ArgDescr, ArgOrder, OptDescr)
import Text.ParserCombinators.ReadP (ReadP)
import Text.ParserCombinators.ReadPrec (ReadPrec)
import Witherable qualified as Hask
import Prelude (Bool)

--------------------------------------------------------------------------------

-- | A 'CategoricalFunctor' of kind @Type -> Type@ mapping from an
-- arbitrary category @cat@ to @->@.
type Functor :: (Type -> Type -> Type) -> (Type -> Type) -> Constraint
type Functor cat p = (MapArg1 cat p)

-- | Lift a function @cat a b@ into a function @f a -> f b@.
fmap :: forall cat f. (Functor cat f) => forall a b. (a `cat` b) -> f a -> f b
fmap = map1

-- | A specialization of 'fmap' for contravariant functors as defined
-- in 'Data.Functor.Contravariant.'
--
-- TODO: Do we keep this around? This is nice to have so that library
-- users don't have to manually pack functions in t'Op'.
contramap :: (Functor Op p) => (a -> b) -> p b -> p a
contramap = fmap . Op

-- | A specialization of 'fmap' for invariant functors as defined
-- in 'Data.Functor.Invariant.'
--
-- TODO: Do we keep this around? This is nice to have so that library
-- users don't have to manually pack functions in t'Iso'.
invmap :: (Functor (Iso (->)) f) => (a -> b) -> (b -> a) -> f a -> f b
invmap f g = fmap (Iso f g)

-- TODO: 'Filterable' is currently unusable due to fundeps. This can
-- be fixed by making it @FunctorOf (Hask.Star Maybe) (->) p@, but I
-- think we can do better by switching away from associated types.
type Filterable p = Functor (Hask.Profunctor.Star Maybe) p

-- | A specialization of 'fmap' for filterable functors as defined
-- in "Witherable"
--
-- TODO: Do we keep this around? This is nice to have so that library
-- users don't have to manually pack functions in 'Hask.Star'.
mapMaybe :: (Filterable f) => (a -> Maybe b) -> f a -> f b
mapMaybe f = map (Hask.Profunctor.Star f)

-- | The 'catMaybes' function takes a list of 'Maybe's and returns
-- a list of all the 'Just' values.
--
-- TODO: Do we keep this around? This is nice to have so that library
-- users don't have to manually pack functions in 'Hask.Star'.
catMaybes :: (Filterable f) => f (Maybe a) -> f a
catMaybes = map (Hask.Profunctor.Star id)

-- | Applied to a predicate and a functor @f a@, returns the those
-- elements that satisfy the predicate.
--
-- TODO: Do we keep this around? This is nice to have so that library
-- users don't have to manually pack functions in 'Hask.Star'.
filter :: (Filterable f) => (a -> Bool) -> f a -> f a
filter f = map (Hask.Profunctor.Star (\a -> if f a then Just a else Nothing))

--------------------------------------------------------------------------------

newtype FromFunctor f a = FromFunctor (f a)
  deriving newtype (Hask.Functor)

instance (Hask.Functor f) => CategoricalFunctor (FromFunctor f) where
  type Dom (FromFunctor f) = (->)
  type Cod (FromFunctor f) = (->)

  map :: (a -> b) -> FromFunctor f a -> FromFunctor f b
  map = Hask.fmap

--------------------------------------------------------------------------------
-- Covariant Functor instances

deriving via (FromFunctor ZipList) instance CategoricalFunctor ZipList

deriving via (FromFunctor Handler) instance CategoricalFunctor Handler

deriving via (FromFunctor Complex) instance CategoricalFunctor Complex

deriving via (FromFunctor Identity) instance CategoricalFunctor Identity

deriving via (FromFunctor Monoid.First) instance CategoricalFunctor Monoid.First

deriving via (FromFunctor Monoid.Last) instance CategoricalFunctor Monoid.Last

deriving via (FromFunctor Down) instance CategoricalFunctor Down

deriving via (FromFunctor Semigroup.First) instance CategoricalFunctor Semigroup.First

deriving via (FromFunctor Semigroup.Last) instance CategoricalFunctor Semigroup.Last

deriving via (FromFunctor Semigroup.Max) instance CategoricalFunctor Semigroup.Max

deriving via (FromFunctor Semigroup.Min) instance CategoricalFunctor Semigroup.Min

deriving via (FromFunctor Semigroup.Dual) instance CategoricalFunctor Semigroup.Dual

deriving via (FromFunctor Semigroup.Product) instance CategoricalFunctor Semigroup.Product

deriving via (FromFunctor Semigroup.Sum) instance CategoricalFunctor Semigroup.Sum

deriving via (FromFunctor NonEmpty) instance CategoricalFunctor NonEmpty

deriving via (FromFunctor STM) instance CategoricalFunctor STM

deriving via (FromFunctor Par1) instance CategoricalFunctor Par1

deriving via (FromFunctor ArgDescr) instance CategoricalFunctor ArgDescr

deriving via (FromFunctor ArgOrder) instance CategoricalFunctor ArgOrder

deriving via (FromFunctor OptDescr) instance CategoricalFunctor OptDescr

deriving via (FromFunctor ReadP) instance CategoricalFunctor ReadP

deriving via (FromFunctor ReadPrec) instance CategoricalFunctor ReadPrec

deriving via (FromFunctor IO) instance CategoricalFunctor IO

deriving via (FromFunctor Maybe) instance CategoricalFunctor Maybe

#if MIN_VERSION_base(4,16,0)
deriving via (FromFunctor Solo) instance CategoricalFunctor Solo
#endif

deriving via (FromFunctor []) instance CategoricalFunctor []

deriving via (FromFunctor (WrappedMonad m)) instance (Monad m) => CategoricalFunctor (WrappedMonad m)

deriving via (FromFunctor (ArrowMonad a)) instance (Arrow a) => CategoricalFunctor (ArrowMonad a)

deriving via (FromFunctor (Lazy.ST s)) instance CategoricalFunctor (Lazy.ST s)

deriving via (FromFunctor (Either a)) instance CategoricalFunctor (Either a)

deriving via (FromFunctor (These a)) instance CategoricalFunctor (These a)

deriving via (FromFunctor Proxy) instance CategoricalFunctor (Proxy :: Type -> Type)

deriving via (FromFunctor (Semigroup.Arg a)) instance CategoricalFunctor (Semigroup.Arg a)

deriving via (FromFunctor (Array i)) instance CategoricalFunctor (Array i)

deriving via (FromFunctor U1) instance CategoricalFunctor (U1 :: Type -> Type)

deriving via (FromFunctor V1) instance CategoricalFunctor (V1 :: Type -> Type)

deriving via (FromFunctor (ST s)) instance CategoricalFunctor (ST s)

deriving via (FromFunctor ((,) a)) instance CategoricalFunctor ((,) a)

deriving via (FromFunctor (WrappedArrow a b)) instance (Arrow a) => CategoricalFunctor (WrappedArrow a b)

-- TODO: Figure out if these instances be written with Deriving Via.
instance (FunctorOf (->) (->) m) => CategoricalFunctor (Kleisli m a) where
  type Dom (Kleisli m a) = (->)
  type Cod (Kleisli m a) = (->)

  map :: (a1 -> b) -> Kleisli m a a1 -> Kleisli m a b
  map f (Kleisli m) = Kleisli $ \a -> map f (m a)

deriving via (FromFunctor (Const m)) instance CategoricalFunctor (Const m :: Type -> Type)

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Monoid.Ap f) where
  type Dom (Monoid.Ap f) = (->)
  type Cod (Monoid.Ap f) = (->)

  map f (Monoid.Ap m) = Monoid.Ap $ map f m

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Monoid.Alt f) where
  type Dom (Monoid.Alt f) = (->)
  type Cod (Monoid.Alt f) = (->)

  map f (Monoid.Alt m) = Monoid.Alt $ map f m

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Rec1 f) where
  type Dom (Rec1 f) = (->)
  type Cod (Rec1 f) = (->)

  map f (Rec1 m) = Rec1 $ map f m

deriving via (FromFunctor (URec (Ptr ()))) instance CategoricalFunctor (URec (Ptr ()) :: Type -> Type)

deriving via (FromFunctor (URec Char)) instance CategoricalFunctor (URec Char :: Type -> Type)

deriving via (FromFunctor (URec Double)) instance CategoricalFunctor (URec Double :: Type -> Type)

deriving via (FromFunctor (URec Float)) instance CategoricalFunctor (URec Float :: Type -> Type)

deriving via (FromFunctor (URec Int)) instance CategoricalFunctor (URec Int :: Type -> Type)

deriving via (FromFunctor (URec Word)) instance CategoricalFunctor (URec Word :: Type -> Type)

deriving via (FromFunctor ((,,) a b)) instance CategoricalFunctor ((,,) a b)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => CategoricalFunctor (Product f g) where
  type Dom (Product f g) = (->)
  type Cod (Product f g) = (->)

  map f (Pair m1 m2) = Pair (map f m1) (map f m2)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => CategoricalFunctor (Sum f g) where
  type Dom (Sum f g) = (->)
  type Cod (Sum f g) = (->)

  map f (InL m1) = InL $ map f m1
  map f (InR m2) = InR $ map f m2

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => CategoricalFunctor (f :*: g) where
  type Dom (f :*: g) = (->)
  type Cod (f :*: g) = (->)

  map f (m1 :*: m2) = map f m1 :*: map f m2

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => CategoricalFunctor (f :+: g) where
  type Dom (f :+: g) = (->)
  type Cod (f :+: g) = (->)

  map f (L1 m1) = L1 $ map f m1
  map f (R1 m2) = R1 $ map f m2

deriving via (FromFunctor (K1 i c)) instance CategoricalFunctor (K1 i c :: Type -> Type)

deriving via (FromFunctor ((,,,) a b c)) instance CategoricalFunctor ((,,,) a b c)

deriving via (FromFunctor ((->) r)) instance CategoricalFunctor ((->) r)

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => CategoricalFunctor (Compose f g) where
  type Dom (Compose f g) = (->)
  type Cod (Compose f g) = (->)

  map f (Compose fga) = Compose $ map (map f) fga

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => CategoricalFunctor (f :.: g) where
  type Dom (f :.: g) = (->)
  type Cod (f :.: g) = (->)

  map f (Comp1 fga) = Comp1 $ map (map f) fga

instance (FunctorOf (->) (->) f) => CategoricalFunctor (M1 i c f) where
  type Dom (M1 i c f) = (->)
  type Cod (M1 i c f) = (->)

  map f (M1 fp) = M1 $ map f fp

deriving via (FromFunctor ((,,,,) a b c d)) instance CategoricalFunctor ((,,,,) a b c d)

deriving via (FromFunctor ((,,,,,) a b c d e)) instance CategoricalFunctor ((,,,,,) a b c d e)

deriving via (FromFunctor ((,,,,,,) a b c d e f)) instance CategoricalFunctor ((,,,,,,) a b c d e f)

#if MIN_VERSION_base(4,17,0)
deriving via (FromFunctor (Generically1 (f :: Type -> Type))) instance (Generic1 f, Hask.Functor (Rep1 f)) => CategoricalFunctor (Generically1 f)
#endif

instance (FunctorOf (->) (->) m) => CategoricalFunctor (IdentityT m) where
  type Dom (IdentityT m) = (->)
  type Cod (IdentityT m) = (->)

  map f (IdentityT m) = IdentityT $ map f m

instance (FunctorOf (->) (->) m) => CategoricalFunctor (MaybeT m) where
  type Dom (MaybeT m) = (->)
  type Cod (MaybeT m) = (->)

  map f (MaybeT m) = MaybeT $ map (map f) m

instance (FunctorOf (->) (->) m) => CategoricalFunctor (ExceptT e m) where
  type Dom (ExceptT e m) = (->)
  type Cod (ExceptT e m) = (->)

  map f (ExceptT m) = ExceptT $ map (map f) m

instance (FunctorOf (->) (->) m) => CategoricalFunctor (ReaderT r m) where
  type Dom (ReaderT r m) = (->)
  type Cod (ReaderT r m) = (->)

  map f (ReaderT g) = ReaderT $ \r -> map f (g r)

instance (FunctorOf (->) (->) m) => CategoricalFunctor (Lazy.StateT s m) where
  type Dom (Lazy.StateT s m) = (->)
  type Cod (Lazy.StateT s m) = (->)

  map f (Lazy.StateT g) = Lazy.StateT $ \s -> map (\(a, s') -> (f a, s')) (g s)

instance (FunctorOf (->) (->) m) => CategoricalFunctor (Strict.StateT s m) where
  type Dom (Strict.StateT s m) = (->)
  type Cod (Strict.StateT s m) = (->)

  map f (Strict.StateT g) = Strict.StateT $ \s -> map (\(a, s') -> (f a, s')) (g s)

instance (FunctorOf (->) (->) m) => CategoricalFunctor (Lazy.WriterT w m) where
  type Dom (Lazy.WriterT w m) = (->)
  type Cod (Lazy.WriterT w m) = (->)

  map f (Lazy.WriterT m) = Lazy.WriterT $ map (\(a, w) -> (f a, w)) m

instance (FunctorOf (->) (->) m) => CategoricalFunctor (Strict.WriterT w m) where
  type Dom (Strict.WriterT w m) = (->)
  type Cod (Strict.WriterT w m) = (->)

  map f (Strict.WriterT m) = Strict.WriterT $ map (\(a, w) -> (f a, w)) m

-- The CPS 'CPS.WriterT' and 'CPS.RWST' constructors are not exported, so these
-- two cannot be written against @FunctorOf (->) (->) m@ and instead reuse the
-- 'Hask.Functor' instance.
deriving via (FromFunctor (CPS.WriterT w m)) instance (Hask.Functor m) => CategoricalFunctor (CPS.WriterT w m)

instance CategoricalFunctor (ContT r m) where
  type Dom (ContT r m) = (->)
  type Cod (ContT r m) = (->)

  map f (ContT g) = ContT $ \k -> g (k . f)

instance (FunctorOf (->) (->) m) => CategoricalFunctor (Lazy.RWST r w s m) where
  type Dom (Lazy.RWST r w s m) = (->)
  type Cod (Lazy.RWST r w s m) = (->)

  map f (Lazy.RWST g) = Lazy.RWST $ \r s -> map (\(a, s', w) -> (f a, s', w)) (g r s)

instance (FunctorOf (->) (->) m) => CategoricalFunctor (Strict.RWST r w s m) where
  type Dom (Strict.RWST r w s m) = (->)
  type Cod (Strict.RWST r w s m) = (->)

  map f (Strict.RWST g) = Strict.RWST $ \r s -> map (\(a, s', w) -> (f a, s', w)) (g r s)

deriving via (FromFunctor (CPS.RWST r w s m)) instance (Hask.Functor m) => CategoricalFunctor (CPS.RWST r w s m)

instance (FunctorOf (->) (->) m) => CategoricalFunctor (AccumT w m) where
  type Dom (AccumT w m) = (->)
  type Cod (AccumT w m) = (->)

  map f (AccumT g) = AccumT $ \w -> map (\(a, w') -> (f a, w')) (g w)

instance (FunctorOf (->) (->) m) => CategoricalFunctor (SelectT r m) where
  type Dom (SelectT r m) = (->)
  type Cod (SelectT r m) = (->)

  map f (SelectT g) = SelectT $ \k -> map f (g (k . f))

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Backwards f) where
  type Dom (Backwards f) = (->)
  type Cod (Backwards f) = (->)

  map f (Backwards m) = Backwards $ map f m

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Reverse f) where
  type Dom (Reverse f) = (->)
  type Cod (Reverse f) = (->)

  map f (Reverse m) = Reverse $ map f m

deriving via (FromFunctor (Constant a)) instance CategoricalFunctor (Constant a :: Type -> Type)

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Lift f) where
  type Dom (Lift f) = (->)
  type Cod (Lift f) = (->)

  map f (Pure a) = Pure $ f a
  map f (Other m) = Other $ map f m

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Hask.Profunctor.Star f a) where
  type Dom (Hask.Profunctor.Star f a) = (->)
  type Cod (Hask.Profunctor.Star f a) = (->)

  map f (Hask.Profunctor.Star g) = Hask.Profunctor.Star $ \x -> map f (g x)

deriving via (FromFunctor (Hask.Profunctor.Costar f a)) instance CategoricalFunctor (Hask.Profunctor.Costar f a)

deriving via (FromFunctor (Hask.Profunctor.Forget r a)) instance CategoricalFunctor (Hask.Profunctor.Forget r a :: Type -> Type)

instance (FunctorOf (->) (->) f) => CategoricalFunctor (WrappedApplicative f) where
  type Dom (WrappedApplicative f) = (->)
  type Cod (WrappedApplicative f) = (->)

  map f (WrapApplicative m) = WrapApplicative $ map f m

instance (FunctorOf (->) (->) f) => CategoricalFunctor (MaybeApply f) where
  type Dom (MaybeApply f) = (->)
  type Cod (MaybeApply f) = (->)

  map f (MaybeApply (Left fa)) = MaybeApply $ Left $ map f fa
  map f (MaybeApply (Right a)) = MaybeApply $ Right $ f a

instance (FunctorOf (->) (->) f) => CategoricalFunctor (Static f a) where
  type Dom (Static f a) = (->)
  type Cod (Static f a) = (->)

  map f (Static g) = Static $ map (f .) g

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) g) => CategoricalFunctor (These1 f g) where
  type Dom (These1 f g) = (->)
  type Cod (These1 f g) = (->)

  map f (This1 fa) = This1 $ map f fa
  map f (That1 ga) = That1 $ map f ga
  map f (These1 fa ga) = These1 (map f fa) (map f ga)

instance (MapArg2 (->) (->) p) => CategoricalFunctor (Flip p a) where
  type Dom (Flip p a) = (->)
  type Cod (Flip p a) = (->)

  map f (Flip pba) = Flip $ map2 f pba

deriving via (FromFunctor (Clown f a)) instance CategoricalFunctor (Clown f a :: Type -> Type)

instance (FunctorOf (->) (->) g) => CategoricalFunctor (Joker g a) where
  type Dom (Joker g a) = (->)
  type Cod (Joker g a) = (->)

  map f (Joker gb) = Joker $ map f gb

instance (FunctorOf (->) (->) (p a), FunctorOf (->) (->) (q a)) => CategoricalFunctor (Bifunctor.Product p q a) where
  type Dom (Bifunctor.Product p q a) = (->)
  type Cod (Bifunctor.Product p q a) = (->)

  map f (Bifunctor.Pair pab qab) = Bifunctor.Pair (map f pab) (map f qab)

instance (FunctorOf (->) (->) (p a), FunctorOf (->) (->) (q a)) => CategoricalFunctor (Bifunctor.Sum p q a) where
  type Dom (Bifunctor.Sum p q a) = (->)
  type Cod (Bifunctor.Sum p q a) = (->)

  map f (Bifunctor.L2 pab) = Bifunctor.L2 $ map f pab
  map f (Bifunctor.R2 qab) = Bifunctor.R2 $ map f qab

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) (p a)) => CategoricalFunctor (Tannen f p a) where
  type Dom (Tannen f p a) = (->)
  type Cod (Tannen f p a) = (->)

  map g (Tannen fp) = Tannen $ map (map g) fp

instance (FunctorOf (->) (->) (p (f a)), FunctorOf (->) (->) g) => CategoricalFunctor (Biff p f g a) where
  type Dom (Biff p f g a) = (->)
  type Cod (Biff p f g a) = (->)

  map h (Biff pfg) = Biff $ map (map h) pfg

instance (FunctorOf (->) (->) (p a)) => CategoricalFunctor (WrappedBifunctor p a) where
  type Dom (WrappedBifunctor p a) = (->)
  type Cod (WrappedBifunctor p a) = (->)

  map f (WrapBifunctor pab) = WrapBifunctor $ map f pab

instance (forall x. MapArg1 (->) (p x)) => CategoricalFunctor (Hask.Profunctor.Procompose p q a) where
  type Dom (Hask.Profunctor.Procompose p q a) = (->)
  type Cod (Hask.Profunctor.Procompose p q a) = (->)

  map f (Hask.Profunctor.Procompose pxc qdx) = Hask.Profunctor.Procompose (map1 f pxc) qdx

instance (MapArg2 Op (->) p) => CategoricalFunctor (Hask.Profunctor.Rift p q a) where
  type Dom (Hask.Profunctor.Rift p q a) = (->)
  type Cod (Hask.Profunctor.Rift p q a) = (->)

  map f (Hask.Profunctor.Rift g) = Hask.Profunctor.Rift $ \p -> g (map2 (Op f) p)

instance CategoricalFunctor (Hask.Profunctor.Yoneda p a) where
  type Dom (Hask.Profunctor.Yoneda p a) = (->)
  type Cod (Hask.Profunctor.Yoneda p a) = (->)

  map f (Hask.Profunctor.Yoneda g) = Hask.Profunctor.Yoneda $ \l r -> g l (r . f)

instance CategoricalFunctor (Hask.Profunctor.Coyoneda p a) where
  type Dom (Hask.Profunctor.Coyoneda p a) = (->)
  type Cod (Hask.Profunctor.Coyoneda p a) = (->)

  map f (Hask.Profunctor.Coyoneda l r p) = Hask.Profunctor.Coyoneda l (f . r) p

instance (FunctorOf (->) (->) f, FunctorOf (->) (->) (p a)) => CategoricalFunctor (Hask.Profunctor.Cayley f p a) where
  type Dom (Hask.Profunctor.Cayley f p a) = (->)
  type Cod (Hask.Profunctor.Cayley f p a) = (->)

  map g (Hask.Profunctor.Cayley fp) = Hask.Profunctor.Cayley $ map (map g) fp

instance (forall x. MapArg1 (->) (p x)) => CategoricalFunctor (Hask.Profunctor.Tambara p a) where
  type Dom (Hask.Profunctor.Tambara p a) = (->)
  type Cod (Hask.Profunctor.Tambara p a) = (->)

  map f (Hask.Profunctor.Tambara t) = Hask.Profunctor.Tambara $ map1 (\(b, c) -> (f b, c)) t

instance CategoricalFunctor (Hask.Profunctor.Pastro p a) where
  type Dom (Hask.Profunctor.Pastro p a) = (->)
  type Cod (Hask.Profunctor.Pastro p a) = (->)

  map f (Hask.Profunctor.Pastro l m r) = Hask.Profunctor.Pastro (f . l) m r

instance CategoricalFunctor (Hask.Profunctor.Cotambara q a) where
  type Dom (Hask.Profunctor.Cotambara q a) = (->)
  type Cod (Hask.Profunctor.Cotambara q a) = (->)

  map f (Hask.Profunctor.Cotambara n r) = Hask.Profunctor.Cotambara n (Hask.Profunctor.rmap f r)

instance CategoricalFunctor (Hask.Profunctor.Copastro p a) where
  type Dom (Hask.Profunctor.Copastro p a) = (->)
  type Cod (Hask.Profunctor.Copastro p a) = (->)

  map f (Hask.Profunctor.Copastro g) = Hask.Profunctor.Copastro $ \n -> Hask.Profunctor.rmap f (g n)

instance (forall x. MapArg1 (->) (p x)) => CategoricalFunctor (Hask.Profunctor.TambaraSum p a) where
  type Dom (Hask.Profunctor.TambaraSum p a) = (->)
  type Cod (Hask.Profunctor.TambaraSum p a) = (->)

  map f (Hask.Profunctor.TambaraSum t) =
    Hask.Profunctor.TambaraSum $ map1 (\e -> case e of Left b -> Left (f b); Right c -> Right c) t

instance CategoricalFunctor (Hask.Profunctor.PastroSum p a) where
  type Dom (Hask.Profunctor.PastroSum p a) = (->)
  type Cod (Hask.Profunctor.PastroSum p a) = (->)

  map f (Hask.Profunctor.PastroSum l m r) = Hask.Profunctor.PastroSum (f . l) m r

instance CategoricalFunctor (Hask.Profunctor.CotambaraSum q a) where
  type Dom (Hask.Profunctor.CotambaraSum q a) = (->)
  type Cod (Hask.Profunctor.CotambaraSum q a) = (->)

  map f (Hask.Profunctor.CotambaraSum n r) = Hask.Profunctor.CotambaraSum n (Hask.Profunctor.rmap f r)

instance CategoricalFunctor (Hask.Profunctor.CopastroSum p a) where
  type Dom (Hask.Profunctor.CopastroSum p a) = (->)
  type Cod (Hask.Profunctor.CopastroSum p a) = (->)

  map f (Hask.Profunctor.CopastroSum g) = Hask.Profunctor.CopastroSum $ \n -> Hask.Profunctor.rmap f (g n)

instance (forall x. MapArg1 (->) (p x)) => CategoricalFunctor (Hask.Profunctor.Closure p a) where
  type Dom (Hask.Profunctor.Closure p a) = (->)
  type Cod (Hask.Profunctor.Closure p a) = (->)

  map f (Hask.Profunctor.Closure t) = Hask.Profunctor.Closure $ map1 (f .) t

instance CategoricalFunctor (Hask.Profunctor.Environment p a) where
  type Dom (Hask.Profunctor.Environment p a) = (->)
  type Cod (Hask.Profunctor.Environment p a) = (->)

  map f (Hask.Profunctor.Environment l m r) = Hask.Profunctor.Environment (f . l) m r

instance CategoricalFunctor (Hask.Profunctor.FreeTraversing p a) where
  type Dom (Hask.Profunctor.FreeTraversing p a) = (->)
  type Cod (Hask.Profunctor.FreeTraversing p a) = (->)

  map f (Hask.Profunctor.FreeTraversing l m r) = Hask.Profunctor.FreeTraversing (f . l) m r

instance (forall x. MapArg1 (->) (p x)) => CategoricalFunctor (Hask.Profunctor.CofreeTraversing p a) where
  type Dom (Hask.Profunctor.CofreeTraversing p a) = (->)
  type Cod (Hask.Profunctor.CofreeTraversing p a) = (->)

  map f (Hask.Profunctor.CofreeTraversing t) = Hask.Profunctor.CofreeTraversing $ map1 (Hask.fmap f) t

instance CategoricalFunctor (Hask.Profunctor.FreeMapping p a) where
  type Dom (Hask.Profunctor.FreeMapping p a) = (->)
  type Cod (Hask.Profunctor.FreeMapping p a) = (->)

  map f (Hask.Profunctor.FreeMapping l m r) = Hask.Profunctor.FreeMapping (f . l) m r

instance (forall x. MapArg1 (->) (p x)) => CategoricalFunctor (Hask.Profunctor.CofreeMapping p a) where
  type Dom (Hask.Profunctor.CofreeMapping p a) = (->)
  type Cod (Hask.Profunctor.CofreeMapping p a) = (->)

  map f (Hask.Profunctor.CofreeMapping t) = Hask.Profunctor.CofreeMapping $ map1 (Hask.fmap f) t

deriving via (FromFunctor (Tagged s)) instance CategoricalFunctor (Tagged s)

--------------------------------------------------------------------------------

newtype FromContra f a = FromContra (f a)
  deriving newtype (Hask.Contravariant)

instance (Hask.Contravariant f) => CategoricalFunctor (FromContra f) where
  type Dom (FromContra f) = Op
  type Cod (FromContra f) = (->)

  map :: Dom (FromContra f) a b -> Cod (FromContra f) ((FromContra f) a) ((FromContra f) b)
  map = Hask.contramap . getOp

--------------------------------------------------------------------------------
-- Contravariant Functor instances

deriving via (FromContra Predicate) instance CategoricalFunctor Predicate

deriving via (FromContra Comparison) instance CategoricalFunctor Comparison

deriving via (FromContra Equivalence) instance CategoricalFunctor Equivalence

deriving via (FromContra (Op a)) instance CategoricalFunctor (Op a)

-- NOTE: The remaining 'Hask.Contravariant' instances in base (t'Const',
-- 'Proxy', 'U1', 'V1', etc.) are phantom in their last parameter and so are
-- also covariant. Each type gets a single 'CategoricalFunctor' instance and
-- those types are committed to @Dom = (->)@ above.

--------------------------------------------------------------------------------

instance CategoricalFunctor Monoid.Endo where
  type Dom Monoid.Endo = Iso (->)
  type Cod Monoid.Endo = (->)

  map :: Iso (->) a b -> Monoid.Endo a -> Monoid.Endo b
  map Iso {..} (Monoid.Endo f) = Monoid.Endo (embed . f . project)

--------------------------------------------------------------------------------

newtype FromFilterable f a = FromFilterable (f a)
  deriving newtype (Hask.Functor, Hask.Filterable)

instance (Hask.Filterable f) => CategoricalFunctor (FromFilterable f) where
  type Dom (FromFilterable f) = (Hask.Profunctor.Star Maybe)
  type Cod (FromFilterable f) = (->)

  map :: Hask.Profunctor.Star Maybe a b -> FromFilterable f a -> FromFilterable f b
  map (Hask.Profunctor.Star f) (FromFilterable fa) = FromFilterable (Hask.mapMaybe f fa)

--------------------------------------------------------------------------------

-- NOTE: These instances conflict with our Covariant Functor
-- instances. Switching from associated types to Multi Parameter type
-- classes would fix this:

-- deriving via (FromFilterable []) instance Functor []

-- deriving via (FromFilterable Maybe) instance Functor Maybe
