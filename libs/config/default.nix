{ haskellPackages ? import ../../nix/overrides.nix,
  flowbox ? import ../../nix/flowbox.nix
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
