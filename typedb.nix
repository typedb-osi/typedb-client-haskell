{ mkDerivation, async, base, bytestring, containers, deepseq
, grpc-haskell, optparse-generic, proto3-suite, proto3-wire, stdenv
, text, vector
}:
mkDerivation {
  pname = "typedb-client";
  version = "0.0.0.1";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    async base bytestring containers deepseq grpc-haskell
    optparse-generic proto3-suite proto3-wire text vector
  ];
  executableHaskellDepends = [ async base ];
  description = "TypeDB Haskell driver using gRPC-haskell by awakenetworks.";
  license = stdenv.lib.licenses.asl20;
}
