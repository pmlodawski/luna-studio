let pkgs = import <nixpkgs> {};

    vanillaHaskellPackages = removeAttrs pkgs.haskellPackages [
                "accelerate" "accelerateIo" "accelerateCuda"
                "cuda" "protocolBuffers" "bmp" "binary"
                "protocolBuffersDescriptor"
    ];

    haskellOverrides = with vanillaHaskellPackages; vanillaHaskellPackages // rec {
        inherit callPackage;

        accelerate = callPackage ./haskell/accelerate.nix {};

        accelerateCuda = callPackage ./haskell/accelerateCuda.nix {
            accelerate = accelerate;
            cuda = cuda;
        };

        accelerateIo = callPackage ./haskell/accelerateIo.nix {
            accelerate = accelerate;
            bmp = bmp;
        };

        algebraic = callPackage ./haskell/algebraic.nix {
            accelerate = accelerate;
        };

        awsSdk = callPackage ./haskell/awsSdk.nix {
            inherit simpleConfig;
        };

        bmp = callPackage ./haskell/bmp.nix {
            binary = null;
        };

        cubicbezier = callPackage ./haskell/cubicbezier.nix {
            inherit integration;
        };

        cuda = callPackage ./haskell/cuda.nix {
            nvidia_x11 = pkgs.linuxPackages.nvidia_x11;
        };

        imagemagick = callPackage ./haskell/imagemagick.nix {
            ImageMagick = pkgs.imagemagick;
        };

        integration = callPackage ./haskell/integration.nix {};

        linearAccelerate = callPackage ./haskell/linearAccelerate.nix {
            accelerate = accelerate;
        };

        pretty = callPackage ./haskell/pretty.nix {};

        protocolBuffers = callPackage ./haskell/protocol-buffers.nix {};

        protocolBuffersDescriptor = callPackage ./haskell/protocol-buffers-descriptor.nix {
            protocolBuffers = protocolBuffers;
        };

        simpleConfig = callPackage ./haskell/simpleConfig.nix {};
    };

in haskellOverrides
