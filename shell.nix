{ pkgs ? import ./nix/pkgs.nix, ... }:
pkgs.haskellPackages.shellFor {
  packages = ps : [ ps.agda-search ];
  buildInputs = [
        pkgs.cabal-install
        ];
}
