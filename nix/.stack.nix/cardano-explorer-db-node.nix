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
        depends = [
          (hsPkgs.base)
          (hsPkgs.cardano-explorer-core)
          (hsPkgs.Cabal)
          ];
        };
      exes = {
        "cardano-explorer-db-node" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.cardano-explorer-db-node)
            (hsPkgs.cardano-shell)
            (hsPkgs.optparse-applicative)
            (hsPkgs.cardano-node)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.cardano-prelude)
            (hsPkgs.text)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.network)
            (hsPkgs.ouroboros-network)
            (hsPkgs.io-sim-classes)
            (hsPkgs.cborg)
            (hsPkgs.typed-protocols)
            (hsPkgs.bytestring)
            (hsPkgs.typed-protocols-cbor)
            (hsPkgs.serialise)
            ];
          };
        };
      tests = {
        "test" = {
          depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer-db-node) ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cardano-explorer-db-node; }