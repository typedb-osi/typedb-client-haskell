let
    config = {allowBroken = true; allowUnfree = true;};
    nixpkgs = import ./nixpkgs.nix { inherit config; };
    grpcPkg = (import ./gRPC-haskell/release.nix).awakePkgs;
    grakn-haskell = grpcPkg.haskellPackages.callPackage ./default.nix {};
in {
    build = grpcPkg.usesGRPC grakn-haskell;
}
