{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-explorer-core"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "(c) 2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-explorer";
      url = "";
      synopsis = "A block explorer for the Cardano chain";
      description = "Code for the Cardano Block Explorer that is shared between the\ncardano-explorer-db-node and the cardano-explorer web application.";
      buildType = "Simple";
      };
    components = {
      "library" = { depends = [ (hsPkgs.base) ]; };
      tests = {
        "test" = {
          depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer-core) ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cardano-explorer-core; }