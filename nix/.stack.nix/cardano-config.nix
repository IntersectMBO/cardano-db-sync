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
          (hsPkgs.iohk-monitoring)
          (hsPkgs.lobemo-backend-aggregation)
          (hsPkgs.lobemo-backend-editor)
          (hsPkgs.lobemo-backend-ekg)
          (hsPkgs.lobemo-backend-monitoring)
          (hsPkgs.lobemo-scribe-systemd)
          (hsPkgs.ouroboros-consensus)
          (hsPkgs.async)
          (hsPkgs.generic-monoid)
          (hsPkgs.optparse-applicative)
          (hsPkgs.safe-exceptions)
          (hsPkgs.stm)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "8128dceddbb9074f5b25ca01728613cac4360298";
      sha256 = "16fj9wjq6c21152fj14y5jxsiymp49dp4x67pzvqhsb7yyb819p9";
      });
    postUnpack = "sourceRoot+=/cardano-config; echo source root reset to \$sourceRoot";
    }