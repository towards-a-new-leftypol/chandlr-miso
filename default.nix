{ pkgs ? import <nixpkgs> {}, }:

let
  nixpkgs = pkgs;
  servant-miso-router = import ./nix-support/servant-miso-router.nix { inherit nixpkgs; };
  servant-miso-html = import ./nix-support/servant-miso-html.nix { inherit nixpkgs; };

  drv = pkgs.haskellPackages.callCabal2nix "chandlr" ./. {
    servant-miso-router = servant-miso-router;
    #servant-miso-html = servant-miso-html;
  };

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
