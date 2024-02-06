{
  description = "Kindly Functors";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-23.11;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVersion = "963";
      compiler = "ghc${ghcVersion}";
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system; };
          hsPkgs = pkgs.haskell.packages.${compiler}.override (old: {
            overrides = pkgs.lib.composeExtensions (old.overrides or (_: _: { }))
              (hfinal: hprev: {
                kindly-functors = (hfinal.callCabal2nix "kindly-functors" ./. { }).overrideScope (hfinal': hprev': {
                  bifunctors = hfinal.bifunctors_5_6_1;
                  semigroupoids = hfinal.semigroupoids_6_0_0_1.overrideScope (hfinal': hprev': {
                    bifunctors = hfinal.bifunctors_5_6_1;
                  });
                });
              });
          });
        in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = with pkgs; [
              cabal-install
              haskell.compiler.${compiler}
              haskell.packages.${compiler}.haskell-language-server
              nixpkgs-fmt
              ormolu
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree {
            kindly-functors = hsPkgs.kindly-functors;
          };

          defaultPackage = packages.kindly-functors;
        });
}
