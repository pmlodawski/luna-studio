{ haskellPackages ? (import <nixpkgs> {}).haskellPackages,
  flowbox
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "flowbox-config";
  version = "0.1";
  src = ./.;
  buildDepends = with flowbox; with haskellPackages; [
    configurator either flowboxUtils transformers
  ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
