{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-explorer-db-test"; version = "0.1.0.0"; };
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
      "library" = {
        depends = [
          (hsPkgs.aeson)
          (hsPkgs.base)
          (hsPkgs.base16-bytestring)
          (hsPkgs.bytestring)
          (hsPkgs.cardano-explorer-db)
          (hsPkgs.conduit)
          (hsPkgs.conduit-extra)
          (hsPkgs.containers)
          (hsPkgs.contra-tracer)
          (hsPkgs.directory)
          (hsPkgs.esqueleto)
          (hsPkgs.extra)
          (hsPkgs.fast-logger)
          (hsPkgs.filepath)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.monad-logger)
          (hsPkgs.persistent)
          (hsPkgs.persistent-postgresql)
          (hsPkgs.persistent-template)
          (hsPkgs.postgresql-simple)
          (hsPkgs.resourcet)
          (hsPkgs.scientific)
          (hsPkgs.text)
          (hsPkgs.template-haskell)
          (hsPkgs.time)
          (hsPkgs.transformers)
          (hsPkgs.unix)
          ];
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.././cardano-explorer-db/test; }