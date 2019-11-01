{ mkDerivation, aeson, attoparsec, base, bytestring, containers
, criterion, fetchgit, filepath, hedgehog, hpack, prettyprinter
, protolude, regex-tdfa, scientific, stdenv, template-haskell, text
, text-builder, th-lift-instances, unordered-containers, vector
}:
mkDerivation {
  pname = "graphql-parser";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/hasura/graphql-parser-hs.git";
    sha256 = "1kar4fp6az0b9xr4zw3z4v5lhyq5nvh138zi6wbwyyds0d6h6bdc";
    rev = "f3d9b645efd9adb143e2ad4c6b73bded1578a4e9";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson attoparsec base bytestring containers filepath hedgehog
    prettyprinter protolude regex-tdfa scientific template-haskell text
    text-builder th-lift-instances unordered-containers vector
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    aeson attoparsec base bytestring containers filepath hedgehog
    prettyprinter protolude regex-tdfa scientific template-haskell text
    text-builder th-lift-instances unordered-containers vector
  ];
  benchmarkHaskellDepends = [
    aeson attoparsec base bytestring containers criterion filepath
    hedgehog prettyprinter protolude regex-tdfa scientific
    template-haskell text text-builder th-lift-instances
    unordered-containers vector
  ];
  prePatch = "hpack";
  homepage = "https://github.com/hasura/graphql-parser-hs#readme";
  license = stdenv.lib.licenses.bsd3;
}
