let
    config = {allowBroken = true; allowUnfree = true;};
    nixpkgs = import ./nixpkgs.nix { inherit config; };
    #unstable = import ./unstable.nix { inherit config; };
    normalHaskPkgs = nixpkgs.haskellPackages;
    #overlays = [(import ./gRPC-haskell/release.nix).overlay ];
    awakePkgs = import ./nixpkgs.nix { inherit config overlays; };
    grpcPkg = awakePkgs;
    grakn-haskell = grpcPkg.haskellPackages.callPackage ./typedb.nix { };
    grpc_awake_src=nixpkgs.fetchgit {
                    url="https://github.com/awakesecurity/gRPC-haskell";
                    rev="28e9e68f3b6ba85358982a93dcc1433139524585";
                    sha256="0k9k7gmyjvng1sml2zhvc91g5ilrd3qwzpzv3zrx4v2bxj7s554p";
                    
                };
    overlays= [(import "${grpc_awake_src}/release.nix").overlay ];
in {
    build = grpcPkg.usesGRPC grakn-haskell;
}
