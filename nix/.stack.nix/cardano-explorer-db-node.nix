{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-explorer-db-node"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "(c) 2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-explorer";
      url = "";
      synopsis = "The DB node for the Cardano Block Explorer";
      description = "A Cardano node that follows the Cardano chain and inserts data from the\nchain into a PostgresQL database.";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer-core) ];
        };
      exes = {
        "cardano-explorer-db-node" = {
          depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer-db-node) ];
          };
        };
      tests = {
        "test" = {
          depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer-db-node) ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cardano-explorer-db-node; }