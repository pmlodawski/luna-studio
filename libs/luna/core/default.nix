{ haskellPackages ? import ../../../nix/overrides.nix,
  flowbox ? import ../../../nix/flowbox.nix
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "luna-core";
  version = "0.1";
  src = ./.;
  buildDepends = with flowbox; with haskellPackages; [
    fgl flowboxUtils lens MissingH
  ];
  buildTools = with haskellPackages; [ hspec ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
