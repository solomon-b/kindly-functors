# Revision history for kindly-functors

## Upcoming
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

## 0.1.0.1 -- 2024-02-04

* Initial Release.
