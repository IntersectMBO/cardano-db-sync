############################################################################
# cardano-explorer Nix build
#
# fixme: document top-level attributes and how to build them
#
############################################################################

{ system ? builtins.currentSystem
, crossSystem ? null
, config ? {}
# Import IOHK common nix lib
, iohkLib ? import ./lib.nix { inherit system crossSystem config; }
# Use nixpkgs pin from iohkLib
, pkgs ? iohkLib.pkgs
, customConfig ? {}
}:

let
  sources = import ./nix/sources.nix;
  haskell_nix = pkgs.fetchgit (builtins.removeAttrs (builtins.fromJSON (builtins.readFile "${sources.iohk-nix}/pins/haskell-nix.json")) [ "date" ]);
  haskell = pkgs.callPackage haskell_nix {
    hackageSourceJSON = ./nix/hackage-nix.json;
  };
  src = iohkLib.cleanSourceHaskell ./.;
  util = pkgs.callPackage ./nix/util.nix {};

  # Example of using a package from iohk-nix
  # TODO: Declare packages required by the build.
  inherit (iohkLib.rust-packages.pkgs) jormungandr;

  # Import the Haskell package set.
  haskellPackages = import ./nix/pkgs.nix {
    inherit pkgs haskell src;
    # Pass in any extra programs necessary for the build as function arguments.
    # Provide cross-compiling secret sauce
    inherit (iohkLib.nix-tools) iohk-extras iohk-module;
  };

in {
  inherit pkgs iohkLib src haskellPackages;
  inherit (haskellPackages.cardano-explorer-webapi.identifier) version;

  # Grab the executable component of our package.
  inherit (haskellPackages.cardano-explorer-webapi.components.exes) cardano-explorer-webapi;

  cardano-sl-core = haskellPackages.cardano-explorer-db.components.library;
  cardano-explorer-node = haskellPackages.cardano-explorer-node.components.exes.cardano-explorer-node;
  cardano-explorer-db-tool = haskellPackages.cardano-explorer-db.components.exes.cardano-explorer-db-tool;

  tests = util.collectComponents "tests" util.isIohkSkeleton haskellPackages;
  benchmarks = util.collectComponents "benchmarks" util.isIohkSkeleton haskellPackages;

  scripts = pkgs.callPackage ./nix/scripts.nix {
    inherit iohkLib customConfig;
  };

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = haskellPackages.shellFor {
    name = "iohk-skeleton-shell";
    # TODO: List all local packages in the project.
    packages = ps: with ps; [
      cardano-explorer-webapi
    ];
    # These programs will be available inside the nix-shell.
    buildInputs =
      with pkgs.haskellPackages; [ hlint stylish-haskell weeder ghcid lentil ];
  };

  # Example of a linting script used by Buildkite.
  checks.lint-fuzz = pkgs.callPackage ./nix/check-lint-fuzz.nix {};

  # Attrset of PDF builds of LaTeX documentation.
  docs = pkgs.callPackage ./docs/default.nix {};
}
