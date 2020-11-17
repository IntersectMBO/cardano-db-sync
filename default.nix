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
, pkgs ? import ./nix { inherit system crossSystem config sourcesOverride gitrev; }
, gitrev ? pkgs.iohkNix.commitIdFromGitRepoOrZero ./.git
}:
with pkgs; with commonLib;
let
  customConfig' = if customConfig ? services then customConfig else {
    services.cardano-db-sync = customConfig;
  };

  haskellPackages = recRecurseIntoAttrs
      # the Haskell.nix package set, reduced to local packages.
      (selectProjectPackages cardanoDbSyncHaskellPackages);

  scripts = callPackage ./nix/scripts.nix {
    customConfig = customConfig';
  };

  rewrite-static = _: p: if (pkgs.stdenv.hostPlatform.isDarwin) then
    pkgs.runCommandCC p.name {
      nativeBuildInputs = [ pkgs.haskellBuildUtils.package pkgs.buildPackages.binutils pkgs.buildPackages.nix ];
    } ''
      cp -R ${p} $out
      chmod -R +w $out
      rewrite-libs $out/bin $out/bin/*
    '' else if (pkgs.stdenv.hostPlatform.isMusl) then
    pkgs.runCommandCC p.name { } ''
      cp -R ${p} $out
      chmod -R +w $out
      $STRIP $out/bin/*
    '' else p;

  packages = {
    inherit haskellPackages cardano-db-sync cardano-db-sync-extended cardano-node scripts;

    inherit (haskellPackages.cardano-db-sync.identifier) version;

    exes = mapAttrsRecursiveCond (as: !(isDerivation as)) rewrite-static (collectComponents' "exes" haskellPackages);

    dockerImage = let
      defaultConfig = rec {
        services.cardano-db-sync = {
          socketPath = lib.mkDefault ("/node-ipc/node.socket");
          postgres.generatePGPASS = false;
        };
      };
      customConfig'' = mkMerge [ defaultConfig customConfig' ];
    in callPackage ./nix/docker.nix {
      scripts = scripts.override {
        customConfig = customConfig'';
      };
      extendedScripts = scripts.override {
        customConfig = mkMerge [
          customConfig''
          { services.cardano-db-sync = {
              extended = true;
            };
          }
        ];
      };
    };

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;

      hlint = callPackage iohkNix.tests.hlint {
        src = ./. ;
      };

      stylish-haskell = callPackage iohkNix.tests.stylish-haskell {
        src = ./. ;
      };
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
};
in packages
