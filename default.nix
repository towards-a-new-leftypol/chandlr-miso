{ new_pkgs ? import <nixpkgs> {}, }:

with (import (builtins.fetchTarball {
  url = "https://github.com/dmjio/miso/archive/refs/tags/1.8.tar.gz";
}) {});

let

  drv = pkgs.haskell.packages.ghcjs.callCabal2nix "chandlr" ./. {};

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      pkgs.haskellPackages.cabal-install
      new_pkgs.haskellPackages.miso-from-html
    ];
  });

in

  if pkgs.lib.inNixShell then env else drv
