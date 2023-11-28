{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.ghcjs
    pkgs.haskell.compiler.ghc810
    pkgs.haskell.packages.ghcjs.ghcjs-base
    pkgs.haskell.packages.ghc810.cabal-install
  ];
}
