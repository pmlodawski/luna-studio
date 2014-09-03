{ haskellPackages ? import ../../../nix/flowbox.nix,
  flowbox ? import ../../../nix/flowbox.nix
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "luna-initializer";
  version = "0.1";
  src = ./.;
  buildDepends = with flowbox; with haskellPackages; [
    flowboxConfig flowboxUtils MissingH
  ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
