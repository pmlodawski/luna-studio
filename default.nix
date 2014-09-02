{ haskellPackages ? (import <nixpkgs> {}).haskellPackages,
  flowbox
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "flowbox-aws";
  version = "0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = with flowbox; with haskellPackages; [
    aws awsSdk conduit conduitExtra either errors exceptions filepath
    flowboxRpc flowboxUtils httpConduit iproute MissingH monadLoops mtl
    network postgresqlSimple protocolBuffers protocolBuffersDescriptor
    resourcet SHA simpleConfig text time transformers
  ];
  buildInputs = with haskellPackages; [ cabalInstall_1_20_0_3 ghc ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
