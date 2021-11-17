{ compiler ? "ghc8107"
, pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/931ab058daa7e4cd539533963f95e2bb0dbd41e6.tar.gz") { }
}:

# This file is used for cabal, haddock, and style CI.

let
  haskellDeps = ps: with ps; [
    cabal-install
    cabal-fmt
    hlint
  ];

  haskellOtherDeps = [ pkgs.haskellPackages.ormolu ];

  ghc = pkgs.haskell.packages.${compiler}.ghcWithPackages haskellDeps;

  otherDeps = with pkgs; [
    nixpkgs-fmt
    zlib
  ];
in
pkgs.mkShell {
  buildInputs =
    [ ghc ]
    ++ haskellOtherDeps
    ++ otherDeps;
}
