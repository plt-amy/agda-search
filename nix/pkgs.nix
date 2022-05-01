import ./pin.nix {
  config = {

    packageOverrides = pkgs: {

        haskell = pkgs.lib.recursiveUpdate pkgs.haskell {
        packageOverrides = hpNew: hpOld: {
              agda-search = hpNew.callPackage ../default.nix {};
            };
        };
    };
  };
}
