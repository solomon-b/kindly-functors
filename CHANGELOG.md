# Revision history for kindly-functors

## Upcoming

## 0.2.0.0 -- 2026-07-26
* Expand GHC support through 9.12. Bump nixpkgs, Cabal, and CI tooling.
* Replace the `Makefile` with a `justfile`, including release commands.
* Add a public `kindly-functors:laws` sublibrary of `hedgehog-classes` `Laws`
  for the functor classes: identity and composition for `map1`/`map2`, one
  bundle per variance, plus `Kindly.Rank2.Laws` for the rank-2 `bmap`/`bmap2`
  functors. The test suite law-checks them across the structural and
  generic-representation instances.
* Replace the ~60 empty per-type `MapArg1`/`MapArg2`/`MapArg3` instances with
  three blanket instances in `Kindly.Class`, keyed on the functor's domain
  category. A `CategoricalFunctor` instance no longer needs a paired `MapArgN`
  instance. No public API change.
* Fill out the `Type -> Type` instance coverage in `Kindly.Functor`: the
  `transformers` stack (`ReaderT`, `StateT`, `WriterT` (Lazy/Strict/CPS),
  `ExceptT`, `MaybeT`, `IdentityT`, `ContT`, `RWST` (Lazy/Strict/CPS),
  `AccumT`, `SelectT`, `Backwards`, `Reverse`, `Constant`, `Lift`),
  partially-applied profunctors (`Star`, `Costar`, `Forget`), `semigroupoids`
  (`WrappedApplicative`, `MaybeApply`, `Static`), `These1`, `Generically1`
  (base >= 4.17), and the remaining non-phantom `Contravariant` types from
  base (`Comparison`, `Equivalence`, `Op`). Adds a direct `transformers`
  dependency.
* Add instances for the `bifunctors` package (new dependency): `Flip`, `Clown`,
  `Joker`, `Product`, `Sum`, `Tannen`, `Biff`, and `WrappedBifunctor`, each
  with its partial application in `Kindly.Functor`.
* Add profunctor (`Dom = Op`) instances for `Star`, `Costar`, `Forget`,
  `Kleisli`, and `WrappedArrow`, and a `profunctorLaws` bundle to the laws
  sublibrary. `Kleisli`, `Star`, and `Forget` need no `Monad`/`Functor`
  constraint for `map2`, unlike their Hask `Profunctor` instances.
* Add profunctor instances for `Procompose`, `Rift`, `Yoneda`, `Coyoneda`,
  and `Cayley`, with their partial applications in `Kindly.Functor`.
* Add profunctor instances for the `Tambara`/`Pastro` families (plain and
  `Sum`), `Closure`, `Environment`, `FreeTraversing`, `CofreeTraversing`,
  `FreeMapping`, and `CofreeMapping`, with their partial applications in
  `Kindly.Functor`. This completes coverage of the `profunctors` package.
* Add instances for `Tagged` (new `tagged` dependency): covariant in its last
  argument, profunctorial (phantom) in its first.
* Add a bifunctor instance for `Op`: covariant in its first argument with
  contravariant partial applications, i.e. `Bifunctor (->) Op Op`. Hask's
  `Bifunctor` and `Profunctor` cannot express this. Add
  `observedBifunctorLaws` for law-testing such instances.
* Add instances for `Fix` (bifunctors), `Constant` at two arguments, and
  `Dual` (semigroupoids). Add trifunctor instances for `Forget` and `K1`,
  and an `observedTrifunctorLaws` bundle for `map3`.
* Add instances for `containers` (new dependency): `Map k`, `IntMap`, `Seq`,
  `ViewL`, `ViewR`, `Tree`, and `SCC`.
* Generalize `invmap` to functors of any variance and add `mapIso`, both backed
  by a new `LiftIso` class in `Kindly.Class` that reflects a `(->)` isomorphism
  into an arbitrary category. `invmap` and `mapIso` now resolve for covariant
  and contravariant functors, not just invariant ones, dropping the leg the
  functor cannot use. The domain category is fixed by the functor argument, so
  existing invariant call sites are unchanged. Add `liftIsoLaws` and
  `mapIsoLaws` bundles to the laws sublibrary. `LiftIso` instances cover `(->)`,
  `Op`, `Iso (->)`, and the Kleisli categories `Star f` and `Kleisli f` (for
  `Monad f`). `Star Maybe` is the domain of a `Filterable` functor.
* Add `bimapIso` and `trimapIso`, the bifunctor and trifunctor analogs of
  `mapIso`. Each maps a `(->)` isomorphism through every position of a
  bifunctor/trifunctor regardless of that position's variance, taking one `Iso`
  per position and reflecting it into that position's category with `liftIso`.
  Add `bimapIsoLaws` and `trimapIsoLaws` bundles to the laws sublibrary.
  Re-export `Iso` from `Kindly.Functor`, `Kindly.Bifunctor`, `Kindly.Trifunctor`,
  and `Kindly` so callers of `mapIso`/`bimapIso`/`trimapIso` can build
  isomorphisms without importing `Data.Isomorphism` directly.
* Give `CategoricalFunctor`'s `map` a generic default backed by `kind-generics`,
  so a datatype with a `GenericK` instance (from `deriveGenericK`) gets a
  `CategoricalFunctor` instance from an empty body carrying only its `Dom` and
  `Cod`. The default reads variance off the field structure and dispatches on the
  instance's categories: covariant (`Dom = (->)`), contravariant (`Op`), and
  invariant (`Iso (->)`) single-parameter functors, and two- and three-parameter
  functors (bifunctors, profunctors, trifunctors) in any per-argument combination
  of those variances. A covariant or contravariant instance of the wrong sign is
  a compile error rather than a wrong answer. Adds a `kind-generics` dependency.

## 0.1.0.1 -- 2024-02-04

* Initial Release.
