{pkgs ? import ./pkgs.nix }:
let
  std-lib = pkgs.agdaPackages.standard-library;

  # the annoying part is that nix already has prebuild agdai files.
  # but I can't figure out how to make the custom program use
  # those files.
  # it just overwrites :s
  std-lib-with-all = pkgs.runCommand "create-all" {} ''
  set -xe
  mkdir -p $out
  cp -R ${std-lib}/src/* $out/.
  cp ${./All.agda} $out/All.agda
  '';

in
pkgs.runCommand "search-index.db" {} ''
    set -xe
    cp -R ${std-lib-with-all}/* ./
    chmod 700 -R ./*
    "${pkgs.haskellPackages.agda-search}"/bin/agda-search ./ ./All.agda $out --command createdb
  ''
