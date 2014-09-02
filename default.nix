{ haskellPackages ? (import <nixpkgs> {}).haskellPackages,
  flowbox }:

let inherit (haskellPackages) cabalInstall_1_20_0_3;
in

haskellPackages.cabal.mkDerivation (self: {
  pname = "flowboxRpc";
  version = "0.1";
  src = ./.;
  buildDepends = with flowbox; with haskellPackages; [
    either errors flowboxUtils mtl protocolBuffers
    protocolBuffersDescriptor transformers zeromq4Haskell
  ];
  enableSplitObjs = false;
  buildTools = [ cabalInstall_1_20_0_3 ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
