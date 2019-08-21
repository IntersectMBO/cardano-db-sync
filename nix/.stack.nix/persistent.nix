{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { nooverlap = false; };
    package = {
      specVersion = "1.10";
      identifier = { name = "persistent"; version = "2.10.2"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>, Greg Weber <greg@gregweber.info>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/persistent";
      url = "";
      synopsis = "Type-safe, multi-backend data serialization.";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/persistent>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.aeson)
          (hsPkgs.attoparsec)
          (hsPkgs.base64-bytestring)
          (hsPkgs.blaze-html)
          (hsPkgs.bytestring)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.fast-logger)
          (hsPkgs.http-api-data)
          (hsPkgs.monad-logger)
          (hsPkgs.mtl)
          (hsPkgs.path-pieces)
          (hsPkgs.resource-pool)
          (hsPkgs.resourcet)
          (hsPkgs.scientific)
          (hsPkgs.silently)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.unliftio-core)
          (hsPkgs.unordered-containers)
          (hsPkgs.vector)
          ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.aeson)
            (hsPkgs.attoparsec)
            (hsPkgs.base64-bytestring)
            (hsPkgs.blaze-html)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.hspec)
            (hsPkgs.http-api-data)
            (hsPkgs.path-pieces)
            (hsPkgs.scientific)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.unordered-containers)
            (hsPkgs.vector)
            ];
          };
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/persistent";
      rev = "c85c1b3eabd81251beb0e358257a9e97c0e3c888";
      sha256 = "1flszbqkmm9rgrd9qgfm2689hs0i40lw34ll6djpwjvy4v9fyldl";
      });
    postUnpack = "sourceRoot+=/persistent; echo source root reset to \$sourceRoot";
    }