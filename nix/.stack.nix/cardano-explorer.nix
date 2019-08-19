{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-explorer"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "(c) 2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "IOHK Engineering Team";
      homepage = "https://github.com/input-output-hk/cardano-explorer";
      url = "";
      synopsis = "A Block Explorer for the Cardano network";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.QuickCheck)
          (hsPkgs.aeson)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-crypto-class)
          (hsPkgs.cardano-explorer-db)
          (hsPkgs.cardano-ledger)
          (hsPkgs.constraints)
          (hsPkgs.deepseq)
          (hsPkgs.esqueleto)
          (hsPkgs.formatting)
          (hsPkgs.generics-sop)
          (hsPkgs.hashable)
          (hsPkgs.memory)
          (hsPkgs.monad-logger)
          (hsPkgs.mtl)
          (hsPkgs.persistent)
          (hsPkgs.persistent-postgresql)
          (hsPkgs.servant)
          (hsPkgs.servant-server)
          (hsPkgs.text)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.warp)
          ];
        };
      exes = {
        "cardano-explorer" = {
          depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer) (hsPkgs.Cabal) ];
          };
        "cardano-mock-explorer" = {
          depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer) ];
          };
        };
      tests = {
        "test" = { depends = [ (hsPkgs.base) (hsPkgs.cardano-explorer) ]; };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cardano-explorer; }