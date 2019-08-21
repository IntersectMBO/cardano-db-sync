{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "persistent-template"; version = "2.7.2"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>, Greg Weber <greg@gregweber.info>";
      author = "Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/persistent";
      url = "";
      synopsis = "Type-safe, non-relational, multi-backend persistence.";
      description = "Hackage documentation generation is not reliable. For up to date documentation, please see: <http://www.stackage.org/package/persistent-template>.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.persistent)
          (hsPkgs.aeson)
          (hsPkgs.bytestring)
          (hsPkgs.containers)
          (hsPkgs.http-api-data)
          (hsPkgs.monad-control)
          (hsPkgs.monad-logger)
          (hsPkgs.path-pieces)
          (hsPkgs.template-haskell)
          (hsPkgs.text)
          (hsPkgs.transformers)
          (hsPkgs.unordered-containers)
          ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.persistent)
            (hsPkgs.persistent-template)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.hspec)
            (hsPkgs.QuickCheck)
            (hsPkgs.text)
            ];
          };
        };
      benchmarks = {
        "persistent-th-bench" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.persistent)
            (hsPkgs.persistent-template)
            (hsPkgs.criterion)
            (hsPkgs.deepseq)
            (hsPkgs.deepseq-generics)
            (hsPkgs.file-embed)
            (hsPkgs.text)
            (hsPkgs.template-haskell)
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
    postUnpack = "sourceRoot+=/persistent-template; echo source root reset to \$sourceRoot";
    }