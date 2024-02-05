Kindly Functors
===============

🚨 **WORK IN PROGRESS** 🚨
[![nix:build](https://github.com/solomon-b/kindly-functors/actions/workflows/nix.yml/badge.svg?branch=main)](https://github.com/solomon-b/kindly-functors/actions/workflows/nix.yml)
[![cabal:build](https://github.com/solomon-b/kindly-functors/actions/workflows/cabal.yml/badge.svg?branch=main)](https://github.com/solomon-b/kindly-functors/actions/workflows/cabal.yml)


A category polymorphic `Functor` typeclass based on the work of [IcelandJack](https://www.reddit.com/r/haskell/comments/eoo16m/base_category_polymorphic_functor_and_functorof/?utm_source=reddit&utm_medium=usertext&utm_name=haskell&utm_content=t1_khkwtph) and [Ed Kmett](https://gist.github.com/ekmett/b26363fc0f38777a637d) allowing you to pick out arbitrary kinds and variances for your functors.

This library offers direct access to the `FunctorOf` and `Functor` classes defined in the above work but also a slightly more familiar API for one, two, and three parameter functors.
```haskell
type Functor f = FunctorOf (->) (->)
type Contravariant f = FunctorOf Op (->)
type Invariant f = FunctorOf (<->) (->)
type Filterable f = FunctorOf (Star Maybe) (->)
type Bifunctor p = FunctorOf (->) (Nat (->) (->))
type Profunctor p = FunctorOf Op (Nat (->) (->))
type Trifunctor p = FunctorOf cat1 (Nat cat2 (Nat cat3 cat4))
```

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

# How does this work?

The above functions are all just instantions of `map1`, `map2`, and `map3`:
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

Becareful when using these directly as GHC might pick out a surprising instance:
```
> map2 show (Left True)
Left "True"
```

These functions themselves are a frontend for `map` from the (kindly) `Functor` class:

```haskell
type Functor :: (from -> to) -> Constraint
class (Category (Dom f), Category (Cod f)) => Functor (f :: from -> to) where
  type Dom f :: from -> from -> Type
  type Cod f :: to -> to -> Type

  map :: Dom f a b -> Cod f (f a) (f b)

-- NOTE: These these classes are labeled from right to left:
k
class (FunctorOf cat (->) p) => MapArg1 cat p | p -> cat where
  map1 :: (a `cat` b) -> p a -> p b
  map1 = map

class (FunctorOf cat1 (cat2 ~> (->)) p) => MapArg2 cat1 cat2 p | p -> cat2 cat2 where
  map2 :: (a `cat1` b) -> forall x. p a x -> p b x
  map2 = runNat . map

class (FunctorOf cat1 (cat2 ~> cat3 ~> (->)) p) => MapArg3 cat1 cat2 cat3 p | p -> cat1 cat2 cat3 where
  map3 :: (a `cat1` b) -> forall x y. p a x y -> p b x y
  map3 f = runNat (runNat (map f))
```
