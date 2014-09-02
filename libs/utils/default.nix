{ haskellPackages ? import ../../nix/overrides.nix }:

let inherit (haskellPackages) cabalInstall_1_20_0_3;
in
haskellPackages.cabal.mkDerivation (self: {
    pname = "flowboxUtils";
    version = "0.1";
    src = ./.;
    doCheck = false;
    buildDepends = with haskellPackages; [
        aeson ansiTerminal
        dataDefault dlist either errors fgl genericDeriving
        hslogger lens MissingH mmorph monadLoops mtl optparseApplicative pretty
        prettyShow process protocolBuffers split text transformers
    ];
    buildTools = [ cabalInstall_1_20_0_3 ];
    enableSplitObjs = false;
})
