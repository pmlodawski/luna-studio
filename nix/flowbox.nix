let
    pkgs = import <nixpkgs> {};

    patchedPkgs = import ./packages.nix;

    callPackage = pkgs.lib.callPackageWith patchedPkgs;

    flowboxPackages = {
        flowboxAws = callPackage ../libs/aws/default.nix {};

        flowboxBus = callPackage ../libs/bus/default.nix {};

        flowboxConfig = callPackage ../libs/config/default.nix {};

        flowboxGraphics = callPackage ../libs/data/graphics/default.nix {};

        flowboxRpc   = callPackage ../libs/rpc/default.nix {};

        flowboxUtils = callPackage ../libs/utils/default.nix {};

        lunaCore = callPackage ../libs/luna/core/default.nix {};

        lunaDistribution = callPackage ../libs/luna/distribution/default.nix {};

        lunaInitializer = callPackage ../libs/luna/initializer/default.nix {};

        lunaParser = callPackage ../libs/luna/parser/default.nix {};

        lunaProtobuf = callPackage ../libs/luna/protobuf/default.nix {};

        numConversion = callPackage ../libs/num-conversion/default.nix {};

        openexr = callPackage ../libs/data/codec/exr/default.nix {};
    };

in flowboxPackages

