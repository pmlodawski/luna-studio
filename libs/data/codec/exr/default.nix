{ haskellPackages ? (import <nixpkgs> {}).haskellPackages,
  pkgs ? (import <nixpkgs> {})
}:

haskellPackages.cabal.mkDerivation (self: {
  pname = "openexr";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  buildDepends = with haskellPackages; [
    accelerate accelerateIo repa split
  ];
  configureFlags = "--with-gcc=g++";
  extraLibraries = [ pkgs.zlib ];
  pkgconfigDepends = [ pkgs.openexr ];
  meta = {
    license = self.stdenv.lib.licenses.unfree;
    platforms = self.ghc.meta.platforms;
  };
})
