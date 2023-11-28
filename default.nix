{ pkgs ? import ./nixpkgs.nix }:

let
  inherit (pkgs.haskell.packages) ghcjs;
  drv = ghcjs.callCabal2nix "chandlr" ./. {};

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      pkgs.haskell.packages.ghc.cabal-install
    ];
  });

in

  if pkgs.lib.inNixShell then env else drv
