{ haskellPackages ? import ../../../nix/overrides.nix,
  flowbox ? import ../../../nix/flowbox.nix
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "luna-protobuf";
  version = "0.1";
  src = ./.;
  buildDepends = with flowbox; with haskellPackages; [
    flowboxUtils lunaCore protocolBuffers protocolBuffersDescriptor
  ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
