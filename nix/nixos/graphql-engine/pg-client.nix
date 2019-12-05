{ mkDerivation, aeson, aeson-casing, attoparsec, base, bytestring
, criterion, fetchgit, file-embed, hashable, hashtables, hasql
, hasql-pool, hasql-transaction, monad-control, mtl
, postgresql-binary, postgresql-libpq, resource-pool, retry
, scientific, stdenv, template-haskell, text, text-builder, th-lift
, th-lift-instances, time, transformers-base, uuid, vector
}:
mkDerivation {
  pname = "pg-client";
  version = "0.1.0";
  src = fetchgit {
    url = "https://github.com/hasura/pg-client-hs.git";
    sha256 = "11ajkz1kbjl5jbcbj0mxzf9vzhjn536d9a60dnl56dl6xvas0ndb";
    rev = "bd21e66d8197af381a6c0b493e22d611ed1fa386";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson aeson-casing attoparsec base bytestring hashable hashtables
    monad-control mtl postgresql-binary postgresql-libpq resource-pool
    retry scientific template-haskell text text-builder th-lift
    th-lift-instances time transformers-base uuid vector
  ];
  testHaskellDepends = [ base ];
  benchmarkHaskellDepends = [
    base bytestring criterion file-embed hashable hasql hasql-pool
    hasql-transaction mtl postgresql-libpq text text-builder
  ];
  homepage = "https://github.com/hasura/platform";
  license = stdenv.lib.licenses.bsd3;
}
