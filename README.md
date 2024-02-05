Kindly Functors
===============

🚨 **WORK IN PROGRESS** 🚨

[![nix:build](https://github.com/solomon-b/kindly-functors/actions/workflows/nix.yml/badge.svg?branch=main)](https://github.com/solomon-b/kindly-functors/actions/workflows/nix.yml)
[![cabal:build](https://github.com/solomon-b/kindly-functors/actions/workflows/cabal.yml/badge.svg?branch=main)](https://github.com/solomon-b/kindly-functors/actions/workflows/cabal.yml)


A category polymorphic `Functor` typeclass based on the work of [IcelandJack](https://www.reddit.com/r/haskell/comments/eoo16m/base_category_polymorphic_functor_and_functorof/?utm_source=reddit&utm_medium=usertext&utm_name=haskell&utm_content=t1_khkwtph) and [Ed Kmett](https://gist.github.com/ekmett/b26363fc0f38777a637d) allowing you to pick out arbitrary kinds and variances for your functors.

This library offers direct access to the `FunctorOf` and `Functor` classes defined in the above work but also a slightly more familiar API for one, two, and three parameter functors.

# High Level Interface
`fmap`, `bimap`, `lmap`, and `rmap` have been made polymorphic over variances:
```
> fmap show (Identity True)
Identity "True"

> getPredicate (fmap (Op read) (Predicate not)) "True"
False

> lmap show (True, False)
("True",False)

> lmap (Op read) not "True"
False

> rmap show (True, False)
(True,"False")

> bimap show read (Left True)
Left "True"

> bimap (read @Int) show ("1", True)
(1,"True")

> bimap (Op (read @Int)) show (+1) "0"
"1"

> trimap show show show (True, False, ())
("True","False","()")
```

# Lower Level Interface

The above functions are all just aliases for the `MapArg1`, `MapArg2`, and `MapArg3` interfaces:
```haskell
-- NOTE: These these classes are labeled from right to left:

class (FunctorOf cat1 (->) p) => MapArg1 cat1 p | p -> cat1 where
  map1 :: (a `cat1` b) -> p a -> p b
  map1 = map

class (FunctorOf cat1 (cat2 ~> (->)) p, forall x. MapArg1 cat2 (p x)) => MapArg2 cat1 cat2 p | p -> cat2 cat2 where
  map2 :: (a `cat1` b) -> forall x. p a x -> p b x
  map2 = runNat . map

class (FunctorOf cat1 (cat2 ~> cat3 ~> (->)) p, forall x. MapArg2 cat2 cat3 (p x)) => MapArg3 cat1 cat2 cat3 p | p -> cat1 cat2 cat3 where
  map3 :: (a `cat1` b) -> forall x y. p a x y -> p b x y
  map3 f = runNat (runNat (map f))

type Functor :: (Type -> Type -> Type) -> (Type -> Type) -> Constraint
type Functor cat p = (MapArg1 cat p)

type Bifunctor :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type) -> Constraint
type Bifunctor cat1 cat2 p = (MapArg2 cat1 cat2 p, forall x. MapArg1 cat2 (p x))

type Trifunctor :: (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type) -> (Type -> Type -> Type -> Type) -> Constraint
type Trifunctor cat1 cat2 cat3 p = (MapArg3 cat3 cat2 cat1 p, forall x. MapArg2 cat2 cat1 (p x), forall x y. MapArg1 cat1 (p x y))
```

`map1`, `map2`, and `map3` can be used directly:
```
> map1 show (True, False, ())
(True,False,"()")

> map1 show (Left True)
Left True

> map2 show (True, False, ())
(True,"False",())

> map3 show (True, False, ())
("True",False,())
```

But be careful when using these directly as GHC might pick out a surprising instance:
```
> map2 show (Left True)
Left "True"
```

# How does this actually work?

`MapArg1`, `MapArg2`, and `MapArg3` are in fact just a frontend for yet another class called `CategoricalFunctor`:
```haskell
type CategoricalFunctor :: (from -> to) -> Constraint
class (Category (Dom f), Category (Cod f)) => CategoricalFunctor (f :: from -> to) where
  type Dom f :: from -> from -> Type
  type Cod f :: to -> to -> Type

  map :: Dom f a b -> Cod f (f a) (f b)
```

This class describes a `Functor` just like the ordinary `base`
`Functor` class but with the key difference that it is polymorphic
over the source and target categories of the functor.

`Dom f` (domain) is the source category and `Cod f` (co-domain) is the
target category.

This means that you can instantiate `CategoricalFunctor` with
Covariant (`->`), Contravariant (`Op`), Invariant (via `Iso`),
`Kleisli`, or any product of the above by using a functor category
(via `~>`).

A helpful tool for working with `CategoricalFunctor` is the `FunctorOf` class:

```haskell
type FunctorOf :: Cat from -> Cat to -> (from -> to) -> Constraint
class (CategoricalFunctor f, dom ~ Dom f, cod ~ Cod f) => FunctorOf dom cod f

instance (CategoricalFunctor f, dom ~ Dom f, cod ~ Cod f) => FunctorOf dom cod f
```

`FunctorOf` gives an easy way of aliasing `CategoricalFunctor`
instances which target specific categories and parameters. We use
`FunctorOf` to implement the outer `MapArg*` interface.

```haskell
type Functor f = FunctorOf (->) (->)
type Contravariant f = FunctorOf Op (->)
type Invariant f = FunctorOf (<->) (->)
type Filterable f = FunctorOf (Star Maybe) (->)
type Bifunctor p = FunctorOf (->) (Nat (->) (->))
type Profunctor p = FunctorOf Op (Nat (->) (->))
type Trifunctor p = FunctorOf cat1 (Nat cat2 (Nat cat3 cat4))
```

In the case of Functors kinds greater then `Type -> Type` the above
aliases are a little deceptive.

For example, to replace the typeclass we all know as `Bifunctor` one
would need both the `Functor f` and `Bifunctor f` aliases from the
above list. This is because each of these aliases picks out a specific
single parameter and sets its variance.

The `MapArg*` classes and higher level interface was built to smooth
over this issue at the cost of less granular control.
