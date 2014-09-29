let pkgs = import <nixpkgs> {};

    callPackage = pkgs.lib.callPackageWith patchedPkgs;

    patchedPkgs = pkgs // rec {

        haskellPackages = import ./overrides.nix;

        openexr = callPackage ./native/openexr.nix {};
        ilmbase = callPackage ./native/ilmbase.nix {};
    };

in patchedPkgs
