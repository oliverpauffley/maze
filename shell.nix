with import <nixpkgs> { };
let
  haskellPackages = pkgs.haskellPackages;

  maze = haskellPackages.callPackage ./default.nix {};

in pkgs.mkShell {
  buildInputs = [haskellPackages.cabal-install maze];
}
