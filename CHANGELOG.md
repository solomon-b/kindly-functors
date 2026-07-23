# Revision history for kindly-functors

## Upcoming
* Expands GHC support through 9.12; bumps nixpkgs, Cabal, and CI tooling.
* Replaces the `Makefile` with a `justfile` for local development.
* Adds release commands to `justfile`.
* Adds a `kindly-functors:laws` public sublibrary of `hedgehog-classes` `Laws`
  for the categorical-functor classes (the identity and composition laws for
  `map1`/`map2`, stated over the functor's domain category so one bundle covers
  covariant, contravariant, and invariant functors), plus `Kindly.Rank2.Laws`
  for the rank-2 (higher-kinded) `bmap` / `bmap2` functors, and expands the test
  suite to law-check them across the structural / generic-representation
  instances.

## 0.1.0.1 -- 2024-02-04

* Initial Release.
