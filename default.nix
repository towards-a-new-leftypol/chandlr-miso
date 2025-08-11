{ pkgs ? import <nixpkgs> {}, }:

let

  drv = pkgs.haskellPackages.callCabal2nix "chandlr" ./. {};

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      pkgs.zlib
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.miso-from-html
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.haskell-language-server # this doesn't work since old pkgs doesn't contain this package
    ];
  });

in

if pkgs.lib.inNixShell then env else drv
