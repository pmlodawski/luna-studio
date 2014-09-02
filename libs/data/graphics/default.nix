{ haskellPackages ? (import <nixpkgs> {}).haskellPackages,
  flowbox
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "flowbox-graphics";
  version = "0.15.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  noHaddock = true;
  buildDepends = with flowbox; with haskellPackages; [
    accelerate accelerateCuda accelerateIo algebraic bmp cubicbezier
    either errors flowboxUtils genericsSop imagemagick JuicyPixels lens liftedBase
    linear linearAccelerate monadControl mtl profunctors QuickCheck
    resourcet split systemFilepath transformers vector
  ];
  buildInputs = with haskellPackages; [ cabalInstall ghc ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
