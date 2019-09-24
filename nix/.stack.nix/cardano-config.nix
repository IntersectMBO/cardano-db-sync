{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "cardano-config"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "";
      maintainer = "operations@iohk.io";
      author = "IOHK";
      homepage = "";
      url = "";
      synopsis = "";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs.base)
          (hsPkgs.cardano-shell)
          (hsPkgs.cardano-prelude)
          (hsPkgs.generic-monoid)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.lobemo-backend-aggregation)
          (hsPkgs.lobemo-backend-editor)
          (hsPkgs.lobemo-backend-ekg)
          (hsPkgs.lobemo-backend-monitoring)
          (hsPkgs.lobemo-scribe-systemd)
          (hsPkgs.optparse-applicative)
          (hsPkgs.safe-exceptions)
          (hsPkgs.stm)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "bbdc33dfca84873b82ea1c0f1364e0386e39e350";
      sha256 = "11v9na2awdgdzm95qc17aiy7z218sxsvmmwfzjhpmlx56cmn1k0p";
      });
    postUnpack = "sourceRoot+=/cardano-config; echo source root reset to \$sourceRoot";
    }