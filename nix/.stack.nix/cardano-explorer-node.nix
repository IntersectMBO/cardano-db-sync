{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-explorer-node"; version = "0.1.0.0"; };
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
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-binary)
          (hsPkgs.cardano-crypto)
          (hsPkgs.cardano-crypto-wrapper)
          (hsPkgs.cardano-explorer-db)
          (hsPkgs.cardano-ledger)
          (hsPkgs.cardano-node)
          (hsPkgs.cardano-prelude)
          (hsPkgs.cardano-shell)
          (hsPkgs.cborg)
          (hsPkgs.containers)
          (hsPkgs.cryptonite)
          (hsPkgs.extra)
          (hsPkgs.formatting)
          (hsPkgs.optparse-applicative)
          (hsPkgs.ouroboros-consensus)
          (hsPkgs.io-sim-classes)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.memory)
          (hsPkgs.monad-logger)
          (hsPkgs.network)
          (hsPkgs.ouroboros-network)
          (hsPkgs.persistent)
          (hsPkgs.text)
          (hsPkgs.typed-protocols)
          (hsPkgs.reflection)
          (hsPkgs.serialise)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.typed-protocols-cbor)
          ];
        };
      exes = {
        "cardano-explorer-node" = {
          depends = [
            (hsPkgs.base)
            (hsPkgs.bytestring)
            (hsPkgs.cardano-crypto-wrapper)
            (hsPkgs.cardano-explorer-node)
            (hsPkgs.cardano-ledger)
            (hsPkgs.cardano-node)
            (hsPkgs.cardano-prelude)
            (hsPkgs.cardano-binary)
            (hsPkgs.cborg)
            (hsPkgs.formatting)
            (hsPkgs.cardano-shell)
            (hsPkgs.ouroboros-consensus)
            (hsPkgs.io-sim-classes)
            (hsPkgs.iohk-monitoring)
            (hsPkgs.network)
            (hsPkgs.optparse-applicative)
            (hsPkgs.ouroboros-network)
            (hsPkgs.reflection)
            (hsPkgs.serialise)
            (hsPkgs.text)
            (hsPkgs.typed-protocols)
            (hsPkgs.typed-protocols-cbor)
            ];
          };
        };
      tests = {
        "test" = {
          depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer-node) ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cardano-explorer-node; }