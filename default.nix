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

  haskellPackages = recRecurseIntoAttrs
    # the Haskell.nix package set, reduced to local packages.
    (selectProjectPackages cardanoDbSyncHaskellPackages);

  scripts = callPackage ./nix/scripts.nix { inherit customConfig; };

  # NixOS tests
  #nixosTests = import ./nix/nixos/tests {
  #  inherit pkgs;
  #};

  dockerImages = let
    stateDir = "/data";
    defaultConfig = rec {
      _file = toString ./default.nix;
      services.cardano-db-sync.socketPath = stateDir + "/node.socket";
    };
    customConfig' = defaultConfig // customConfig;
  in pkgs.callPackage ./nix/docker.nix {
    inherit (self) cardano-db-sync;
    inherit (self) cardano-db-sync-extended;
    scripts = callPackage ./nix/scripts.nix { customConfig = customConfig'; };
  };

  self = {
    inherit haskellPackages
      scripts
      dockerImages
      #nixosTests
    ;

    # Grab the executable component of our package.
    inherit (haskellPackages.cardano-db-sync.components.exes)
      cardano-db-sync;
    inherit (haskellPackages.cardano-db-sync-extended.components.exes)
      cardano-db-sync-extended;

    # `tests` are the test suites which have been built.
    tests = collectComponents' "tests" haskellPackages;
    # `benchmarks` (only built, not run).
    benchmarks = collectComponents' "benchmarks" haskellPackages;

    checks = recurseIntoAttrs {
      # `checks.tests` collect results of executing the tests:
      tests = collectChecks haskellPackages;
    };

    shell = import ./shell.nix {
      inherit pkgs;
      withHoogle = true;
    };
};
in self
