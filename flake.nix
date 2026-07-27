{
  description = "Kindly Functors";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixos-26.05;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    let
      ghcVersion = "9103";
      compiler = "ghc${ghcVersion}";
      overlay = import ./overlay.nix;
      overlays = [ overlay ];
    in
    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs { inherit system overlays; };
        in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              cabal-install
              haskell.compiler.${compiler}
              haskell.packages.${compiler}.haskell-language-server
              hlint
              just
              nixpkgs-fmt
              ormolu
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree
            {
              kindly-functors = pkgs.haskellPackages.kindly-functors;
            } // {
            default = pkgs.haskellPackages.kindly-functors;
          };
        }) // {
      overlays.default = overlay;
    };
}
