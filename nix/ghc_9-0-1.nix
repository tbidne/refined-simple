{}:

let
  pkgs = import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/8a308775674e178495767df90c419425474582a1.tar.gz") { };
  compilerVersion = "ghc901";
  compiler = pkgs.haskell.packages."${compilerVersion}";
in
compiler.developPackage {
  name = "refined-simple";
  root = ../.;
  returnShellEnv = true;
  modifier = drv:
    pkgs.haskell.lib.addBuildTools drv (with pkgs.haskellPackages; [
      cabal-install
      pkgs.zlib
    ]);
}
