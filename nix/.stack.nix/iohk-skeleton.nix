{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = { development = false; };
    package = {
      specVersion = "2.2";
      identifier = { name = "iohk-skeleton"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Input Output (Hong Kong) Ltd.";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "Template project with reference CI setup";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = { depends = [ (hsPkgs.base) (hsPkgs.cardano-prelude) ]; };
      exes = {
        "iohk-skeleton" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-prelude)
            (hsPkgs.iohk-skeleton)
            ];
          };
        };
      tests = {
        "unit" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-prelude)
            (hsPkgs.iohk-skeleton)
            (hsPkgs.hspec)
            (hsPkgs.text)
            ];
          };
        };
      benchmarks = {
        "iohk-skeleton-bench" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-prelude)
            (hsPkgs.criterion)
            (hsPkgs.iohk-skeleton)
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././.; }