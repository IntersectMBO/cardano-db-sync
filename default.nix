{ system ? builtins.currentSystem
, crossSystem ? null
# allows to cutomize haskellNix (ghc and profiling, see ./nix/haskell.nix)
, config ? {}
# override scripts with custom configuration
, customConfig ? {}
# allows to override dependencies of the project without modifications,
# eg. to test build against local checkout of nixpkgs and iohk-nix:
# nix build -f default.nix cardano-db-sync --arg sourcesOverride '{
#   iohk-nix = ../iohk-nix;
# }'
, sourcesOverride ? {}
# pinned version of nixpkgs augmented with overlays (iohk-nix and our packages).
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let
  customConfig' = if customConfig ? services then customConfig else {
    services.cardano-db-sync = customConfig;
  };

  packages = self: {
    haskellPackages = recRecurseIntoAttrs
      # the Haskell.nix package set, reduced to local packages.
      (selectProjectPackages cardanoDbSyncHaskellPackages);

    # NixOS tests
    #nixosTests = import ./nix/nixos/tests {
    #  inherit pkgs;
    #};

    scripts = self.callPackage ./nix/scripts.nix {
      customConfig = customConfig';
      syncPackages = self;
    };

    dockerImage = let
      stateDir = "/data";
      defaultConfig = rec {
        services.cardano-db-sync = {
          socketPath = lib.mkDefault (stateDir + "/node.socket");
          postgres.generatePGPASS = false;
        };
      };
      customConfig'' = mkMerge [ defaultConfig customConfig' ];
    in self.callPackage ./nix/docker.nix {
      scripts = self.scripts.override {
        customConfig = customConfig'';
      };
      extendedScripts = self.scripts.override {
        customConfig = mkMerge [
          customConfig''
          { services.cardano-db-sync = {
              extended = true;
            };
          }
        ];
      };
    };

    # Grab the executable component of our package.
    inherit (self.haskellPackages.cardano-db-sync.components.exes)
      cardano-db-sync;
    inherit (self.haskellPackages.cardano-db-sync-extended.components.exes)
      cardano-db-sync-extended;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks self.haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
};
in pkgs.lib.makeScope pkgs.newScope packages
