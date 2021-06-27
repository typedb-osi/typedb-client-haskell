let
    config = {allowBroken = true; allowUnfree = true;};
    nixpkgs = import ./nixpkgs.nix { inherit config; };
    #unstable = import ./unstable.nix { inherit config; };
    normalHaskPkgs = nixpkgs.haskellPackages;
    #overlays = [(import ./gRPC-haskell/release.nix).overlay ];
    awakePkgs = import ./nixpkgs.nix { inherit config overlays; };
    grpcPkg = awakePkgs;
    grakn-haskell = grpcPkg.haskellPackages.callPackage ./default.nix { };
    grpc_awake_src=fetchGit {
                    url="https://github.com/awakesecurity/gRPC-haskell";
                    
                };
    overlays= [(import "${grpc_awake_src}/release.nix").overlay ];
in {
    build = grpcPkg.usesGRPC grakn-haskell;
}
