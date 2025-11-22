{ nixpkgs ? import <nixpkgs> {} }:

let
  haskellPackages = nixpkgs.haskellPackages;

  src = nixpkgs.fetchFromGitHub {
    owner = "dmjio";
    repo = "miso";
    rev = "f5bbda3e62dd8278a1537a10abcf023123187509";
    sha256 = "sha256-Ki1c1dj6K2tV2xCo/hHzQZRXy+tfC+V9+P/s97uTsfo=";
  };

  miso = haskellPackages.callCabal2nix "miso" src {};
in

  miso
