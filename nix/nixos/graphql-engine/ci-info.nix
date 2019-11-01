{ mkDerivation, aeson, aeson-casing, base, fetchgit, hashable
, hpack, stdenv, template-haskell, text, th-lift-instances
, unordered-containers
}:
mkDerivation {
  pname = "ci-info";
  version = "0.1.0.0";
  src = fetchgit {
    url = "https://github.com/hasura/ci-info-hs.git";
    sha256 = "02qizvdpij1gaj24f6d271110sp0gwfkr4qvgxqx4r27hr7fw55n";
    rev = "ad6df731584dc89b72a6e131687d37ef01714fe8";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    aeson aeson-casing base hashable template-haskell text
    th-lift-instances unordered-containers
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "https://github.com/hasura/ci-info-hs#readme";
  license = stdenv.lib.licenses.mit;
}
