let typedb_client_env = (import ./release.nix).build;
    cabal = (import ./nixpkgs.nix{}).haskellPackages.Cabal;
in
with import <nixpkgs>{};
stdenv.mkDerivation{
  name = "typedb-client";
  version = "0.0.0.1";
  src = ./.;
  buildInputs = [typedb_client_env ];
  buildPhase = ''
        echo "hi"
        cabal update && cabal build
      '';
}
