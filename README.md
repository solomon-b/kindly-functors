Kindly Functors
===============

🚨 **WORK IN PROGRESS** 🚨

[![kindly-functors::CI](https://github.com/solomon-b/kindly-functors/actions/workflows/nix.yml/badge.svg)](https://github.com/solomon-b/kindly-functors/actions/workflows/nix.yml)
[![kindly-functors::CI](https://github.com/solomon-b/kindly-functors/actions/workflows/cabal.yml/badge.svg)](https://github.com/solomon-b/kindly-functors/actions/workflows/cabal.yml)


A category polymorphic `Functor` typeclass based on the work of [IcelandJack](https://www.reddit.com/r/haskell/comments/eoo16m/base_category_polymorphic_functor_and_functorof/?utm_source=reddit&utm_medium=usertext&utm_name=haskell&utm_content=t1_khkwtph) and [Ed Kmett](https://gist.github.com/ekmett/b26363fc0f38777a637d).

```
type Functor f = FunctorOf (->) (->)
type Contravariant f = FunctorOf Op (->)
type Invariant f = FunctorOf (<->) (->)
type Filterable f = FunctorOf (Star Maybe) (->)
type Bifunctor p = FunctorOf (->) (Nat (->) (->))
type Profunctor p = FunctorOf Op (Nat (->) (->))
type Trifunctor p = FunctorOf cat1 (Nat cat2 (Nat cat3 cat4))
```

```
> :t map
map :: forall from to (f :: from -> to) (a :: from) (b :: from). Functor f => Dom f a b -> Cod f (f a) (f b)
> :t fmap
fmap :: FunctorOf (->) (->) f => (a -> b) -> f a -> f b
> :t contramap
contramap :: FunctorOf Data.Functor.Contravariant.Op (->) f => (a -> b) -> f b -> f a
> :t invmap
invmap
  :: FunctorOf (<->) (->) f => (a -> b) -> (b -> a) -> f a -> f b
> :t first
first :: FunctorOf (->) ((->) ~> (->)) p => (a -> b) -> p a x -> p b x
> :t second
second :: forall {k} (p :: k -> * -> *) (x :: k) a b. FunctorOf (->) (->) (p x) => (a -> b) -> p x a -> p x b
> :t bimap
bimap :: (FunctorOf (->) (->) (p a), FunctorOf (->) ((->) ~> (->)) p) => (a -> b) -> (c -> d) -> p a c -> p b d
> :t dimap
dimap :: (FunctorOf Data.Functor.Contravariant.Op ((->) ~> (->)) p, forall x. FunctorOf (->) (->) (p x)) => (a -> b) -> (c -> d) -> p b c -> p a d
```
