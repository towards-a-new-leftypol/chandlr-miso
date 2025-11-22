{ pkgs ? import <nixpkgs> {}, }:

let
  nixpkgs = pkgs;
  servant-miso-router = import ./nix-support/servant-miso-router.nix { inherit nixpkgs; };
  servant-miso-html = import ./nix-support/servant-miso-html.nix { inherit nixpkgs; };
  haskell = pkgs.haskell.packages.ghc912;

  drv = haskell.callCabal2nix "chandlr" ./. {
    servant-miso-router = servant-miso-router;
    #servant-miso-html = servant-miso-html;
  };

  env = drv.env.overrideAttrs (oldAttrs: {
    buildInputs = oldAttrs.buildInputs ++ [
      pkgs.zlib
      #haskell.cabal-install
      pkgs.haskellPackages.cabal-install
      haskell.miso-from-html
      haskell.hlint
      haskell.haskell-language-server
    ];
  });

in

if pkgs.lib.inNixShell then env else drv
