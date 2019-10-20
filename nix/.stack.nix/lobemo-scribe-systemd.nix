{ system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "2.0";
      identifier = { name = "lobemo-scribe-systemd"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 IOHK";
      maintainer = "operations@iohk.io";
      author = "Alexander Diemand";
      homepage = "https://github.com/input-output-hk/iohk-monitoring-framework";
      url = "";
      synopsis = "provides a backend for logging to systemd/journal";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = ([
          (hsPkgs.base)
          (hsPkgs.iohk-monitoring)
          (hsPkgs.aeson)
          (hsPkgs.bytestring)
          (hsPkgs.katip)
          (hsPkgs.text)
          (hsPkgs.template-haskell)
          (hsPkgs.unordered-containers)
          ] ++ (if system.isWindows
          then [ (hsPkgs.Win32) ]
          else [ (hsPkgs.unix) ])) ++ (pkgs.lib).optionals (system.isLinux) [
          (hsPkgs.hsyslog)
          (hsPkgs.libsystemd-journal)
          ];
        };
      };
    } // {
    src = (pkgs.lib).mkDefault (pkgs.fetchgit {
      url = "https://github.com/input-output-hk/iohk-monitoring-framework";
      rev = "ba6e4eaf61903d2bd4d745215bae29d35a58d0b6";
      sha256 = "1bz7j9z7kca7n6lin9ng1di7b3k3sj7w42d73mxy499x8fvdymmf";
      });
    postUnpack = "sourceRoot+=/plugins/scribe-systemd; echo source root reset to \$sourceRoot";
    }