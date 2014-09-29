{ haskellPackages ? import ../../../nix/overrides.nix,
  flowbox ? import ../../../nix/flowbox.nix
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "luna-parser";
  version = "0.1";
  src = ./.;
  buildDepends = with flowbox; with haskellPackages; [
    concatenative flowboxUtils lunaCore mtl parsec text
  ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
