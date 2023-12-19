{ nixpkgs ? import <nixpkgs> {}, }:

let
  pkgs = import ./nixpkgs.nix { nixpkgs = nixpkgs; } ;

  inherit (pkgs.haskell.packages) ghcjs;
  drv = ghcjs.callCabal2nix "chandlr" ./. {};

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      pkgs.haskell.packages.ghc.cabal-install
      nixpkgs.haskellPackages.miso-from-html
    ];
  });

in

  if pkgs.lib.inNixShell then env else drv
