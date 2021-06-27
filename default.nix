let typedb_client_env = (import ./release.nix).build.env;
    cabal = (import ./nixpkgs.nix{}).haskellPackages.Cabal_3_2_0_0;
in
with import <nixpkgs>{};
stdenv.mkDerivation{
  name = "typedb-client";
  version = "0.0.0.1";
  src = ./.;
  buildPhase = ''
        ${cabal}/bin/cabal update && ${cabal}/bin/cabal build
      '';
}
