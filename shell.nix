{ nixpkgs ? import <nixpkgs> {} }: 
let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ghcjs-base, cabal-install
      }:
      mkDerivation {
        pname = "chandlr";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base
          ghcjs-base
        ];
        testHaskellDepends = [ cabal-install ];
        license = "unknown";
        #hydraPlatforms = stdenv.lib.platforms.none;
      };

  haskellPackages = pkgs.haskell.packages.ghcjs;

  drv = haskellPackages.callPackage f {
    cabal-install = haskellPackages.cabal-install;
  };

in

  if pkgs.lib.inNixShell then drv.env else drv
