let
    config = {allowBroken = true; allowUnfree = true;};
    nixpkgs = import ./nixpkgs.nix { inherit config; };
    overlays = [(import ./gRPC-haskell/release.nix).overlay ];
    awakePkgs = import ./nixpkgs.nix { inherit config overlays; };
    grpcPkg = awakePkgs;
    grakn-haskell = grpcPkg.haskellPackages.callPackage ./default.nix {};
in {
    build = grpcPkg.usesGRPC grakn-haskell;
}
