############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-explorer ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? { config = { allowUnfree = false; inHydra = true; }; }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling
, supportedCrossSystems ? [ "x86_64-linux" ]

# A Hydra option
, scrubJobs ? true

# Import IOHK common nix lib
# TODO: remove line below, and uncomment the next line
, iohkLib ? import ./nix/iohk-common.nix {}
}:

with (import iohkLib.release-lib) {
  inherit (import ./nix/iohk-common.nix {}) pkgs;

  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-explorer;
  gitrev = cardano-explorer.rev;
};

with pkgs.lib;

let
  testsSupportedSystems = [ "x86_64-linux" ];
  collectTests = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    native = mapTestOn (packagePlatforms project);
    "${mingwW64.config}" = mapTestOnCross mingwW64 (packagePlatformsCross project);
  }
  // {
    # This aggregate job is what IOHK Hydra uses to update
    # the CI status in GitHub.
    required = mkRequiredJob (
      collectTests jobs.native.tests ++
      collectTests jobs.native.benchmarks ++
      # TODO: Add your project executables to this list
      [ jobs.native.cardano-explorer.x86_64-linux
      ]
    );
  }
  # Build the shell derivation in Hydra so that all its dependencies
  # are cached.
  // mapTestOn (packagePlatforms { inherit (project) shell; });

in jobs
