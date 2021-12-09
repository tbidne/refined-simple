{ compilerVersion }:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/8a308775674e178495767df90c419425474582a1.tar.gz") { };
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
pkgs.mkShell {
  buildInputs =
    [
      pkgs.cabal-install
      compiler.ghc
    ];
}
