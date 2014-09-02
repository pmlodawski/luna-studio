let
    pkgs = import <nixpkgs> {};
    callPackage = pkgs.callPackage;
    flowboxPackages = {
        inherit (pkgs) haskellPackages;

        flowboxAws = callPackage ../libs/aws/default.nix {
            flowbox = flowboxPackages;
        };
        flowboxBus = callPackage ../libs/bus/default.nix {
            flowbox = flowboxPackages;
        };
        flowboxConfig = callPackage ../libs/config/default.nix {
            flowbox = flowboxPackages;
        };
        flowboxGraphics = callPackage ../libs/data/graphics/default.nix {
            flowbox = flowboxPackages;
        };
        flowboxRpc   = callPackage ../libs/rpc/default.nix {
            flowbox = flowboxPackages;
        };
        flowboxUtils = callPackage ../libs/utils/default.nix {};
        lunaCore = callPackage ../libs/luna/core/default.nix {
            flowbox = flowboxPackages;
        };
        lunaDistribution = callPackage ../libs/luna/distribution/default.nix {
            flowbox = flowboxPackages;
        };
        lunaInitializer = callPackage ../libs/luna/initializer/default.nix {
            flowbox = flowboxPackages;
        };
        lunaParser = callPackage ../libs/luna/parser/default.nix {
            flowbox = flowboxPackages;
        };
        lunaProtobuf = callPackage ../libs/luna/protobuf/default.nix {
            flowbox = flowboxPackages;
        };
        numConversion = callPackage ../libs/num-conversion/default.nix {};
        openexr = callPackage ../libs/data/codec/exr/default.nix {};
    };
in flowboxPackages

