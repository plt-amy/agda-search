{ pkgs ? import <nixpkgs> { }, ... }:
pkgs.haskellPackages.callCabal2nix "agda-search" ./. { }
