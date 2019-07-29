############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################

{ pkgs

# haskell.nix
, haskell

# Filtered sources of this project
, src

# Customisations for cross-compiling
, iohk-extras ? {}
, iohk-module ? {}

}:

let
  # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  # Grab the compiler name from stack-to-nix output.
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-explorer.src = src + "/cardano-explorer";
        # packages.another-package = src + /another-package;
      }

      # Add dependencies
      {
        # How to set environment variables for builds
        # packages.iohk-skeleton.preBuild = "export NETWORK=testnet";

        # How to add program depdendencies for benchmarks
        # TODO: remove if not applicable
        packages.iohk-skeleton.components.benchmarks.iohk-skeleton-bench = {
          build-tools = [ pkgs.makeWrapper ];
          postInstall = ''
            makeWrapper \
              $out/iohk-skeleton-*/iohk-skeleton-bench \
              $out/bin/iohk-skeleton-bench \
              --prefix PATH : {cowsay}/bin
          '';
        };
        # fixme: Workaround for https://github.com/input-output-hk/haskell.nix/issues/207
        packages.iohk-skeleton.components.all.postInstall = pkgs.lib.mkForce "";
      }

      # Misc. build fixes for dependencies
      {
        # Cut down iohk-monitoring deps
        # Note that this reflects flags set in stack.yaml.
        # There is an open ticket to automate this in stack-to-nix.
        # https://github.com/input-output-hk/haskell.nix/issues/141
        packages.iohk-monitoring.flags = {
          disable-ekg = true;
          disable-examples = true;
          disable-graylog = true;
          disable-gui = true;
          disable-prometheus = true;
          disable-systemd = true;
        };

        # Katip has Win32 (>=2.3 && <2.6) constraint
        packages.katip.doExactConfig = true;
      }

      # The iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.
      iohk-module
    ];

    pkg-def-extras = [
      # iohk-extras contains package overrides and patches per ghc version.
      iohk-extras.${compiler}
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
