{ pkgs ? import ./nixpkgs.nix }:

let
  inherit (pkgs.haskell.packages) ghcjs;
  drv = ghcjs.callCabal2nix "chandlr" ./. {};

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      pkgs.haskellPackages.cabal-install
    ];
  });

  # pkgs = import ./nixpkgs.nix { nixpkgs = nixpkgs; };
  # chandlr_pkg = pkgs.haskell.packages.ghcjs.callCabal2nix "chandlr" ./. {};
  # #chandlr = nixpkgs.haskell.packages.ghcjs.callPackage chandlr_pkg {};
  # chandlr = pkgs.callPackage chandlr_pkg {};
in

  if pkgs.lib.inNixShell then env else drv
