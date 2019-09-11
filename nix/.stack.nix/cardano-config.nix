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
          (hsPkgs.optparse-applicative)
          (hsPkgs.safe-exceptions)
          (hsPkgs.stm)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/cardano-node";
      rev = "f4608724f24a923543c0b5f00c1833f76028888c";
      sha256 = "093rcbkls3jpas3q7dh8nnafgm5w564mns3jbsmr8rlix67sq3xz";
      });
    postUnpack = "sourceRoot+=/cardano-config; echo source root reset to \$sourceRoot";
    }