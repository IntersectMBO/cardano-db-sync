{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "persistent-postgresql"; version = "2.10.0"; };
      license = "MIT";
      copyright = "";
      maintainer = "Michael Snoyman <michael@snoyman.com>";
      author = "Felipe Lessa, Michael Snoyman <michael@snoyman.com>";
      homepage = "http://www.yesodweb.com/book/persistent";
      url = "";
      synopsis = "Backend for the persistent library using postgresql.";
      description = "Based on the postgresql-simple package";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.persistent)
          (hsPkgs.aeson)
          (hsPkgs.blaze-builder)
          (hsPkgs.bytestring)
          (hsPkgs.conduit)
          (hsPkgs.containers)
          (hsPkgs.monad-logger)
          (hsPkgs.postgresql-simple)
          (hsPkgs.postgresql-libpq)
          (hsPkgs.resourcet)
          (hsPkgs.resource-pool)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.unliftio-core)
          ];
        };
      tests = {
        "test" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.persistent)
            (hsPkgs.persistent-postgresql)
            (hsPkgs.persistent-qq)
            (hsPkgs.persistent-template)
            (hsPkgs.persistent-test)
            (hsPkgs.aeson)
            (hsPkgs.bytestring)
            (hsPkgs.containers)
            (hsPkgs.fast-logger)
            (hsPkgs.HUnit)
            (hsPkgs.hspec)
            (hsPkgs.hspec-expectations)
            (hsPkgs.monad-logger)
            (hsPkgs.QuickCheck)
            (hsPkgs.quickcheck-instances)
            (hsPkgs.resourcet)
            (hsPkgs.text)
            (hsPkgs.time)
            (hsPkgs.transformers)
            (hsPkgs.unliftio-core)
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
    postUnpack = "sourceRoot+=/persistent-postgresql; echo source root reset to \$sourceRoot";
    }