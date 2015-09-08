{ haskellPackages ? import ../../nix/overrides.nix }:

haskellPackages.cabal.mkDerivation (self: {
  pname = "num-conversion";
  version = "0.1";
  src = ./.;
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
