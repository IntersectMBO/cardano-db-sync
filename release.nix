############################################################################
#
# Hydra release jobset.
#
# The purpose of this file is to select jobs defined in default.nix and map
# them to all supported build platforms.
#
############################################################################

# The project sources
{ cardano-db-sync ? { outPath = ./.; rev = "abcdef"; }

# Function arguments to pass to the project
, projectArgs ? {
    inherit sourcesOverride;
    config = { allowUnfree = false; inHydra = true; };
    gitrev = cardano-db-sync.rev;
  }

# The systems that the jobset will be built for.
, supportedSystems ? [ "x86_64-linux" "x86_64-darwin" ]

# The systems used for cross-compiling (default: linux)
, supportedCrossSystems ? [ (builtins.head supportedSystems) ]

# A Hydra option
, scrubJobs ? true

# Dependencies overrides
, sourcesOverride ? {}

# Import pkgs, including IOHK common nix lib
, pkgs ? import ./nix { inherit sourcesOverride; }

}:

with (import pkgs.iohkNix.release-lib) {
  inherit pkgs;
  inherit supportedSystems supportedCrossSystems scrubJobs projectArgs;
  packageSet = import cardano-db-sync;
  gitrev = cardano-db-sync.rev;
};

with pkgs.lib;

let
  dockerImageArtifact = let
    image = (pkgsFor (builtins.head  supportedSystems)).dockerImage;
  in
    pkgs.runCommand "${image.name}-hydra" {} ''
      mkdir -pv $out/nix-support/
      cat <<EOF > $out/nix-support/hydra-build-products
      file dockerimage-${image.name} ${image}
      EOF
    '';

  # restrict supported systems to a subset where tests (if exist) are required to pass:
  testsSupportedSystems = intersectLists supportedSystems [ "x86_64-linux" ];
  # Recurse through an attrset, returning all derivations in a list matching test supported systems.
  collectJobs' = ds: filter (d: elem d.system testsSupportedSystems) (collect isDerivation ds);
  # Adds the package name to the derivations for windows-testing-bundle.nix
  # (passthru.identifier.name does not survive mapTestOn)
  collectJobs = ds: concatLists (
    mapAttrsToList (packageName: package:
      map (drv: drv // { inherit packageName; }) (collectJobs' package)
    ) ds);

  nonDefaultBuildSystems = tail supportedSystems;
  # Paths or prefixes of paths of derivations to build only on the default system (ie. linux on hydra):
  onlyBuildOnDefaultSystem = [
    ["checks" "hlint"] ["checks" "stylish-haskell"] ["dockerImage"]
  ];
  # Paths or prefix of paths for which musl64 are disabled:
  noMusl64Build = [ ["shell"] ["checks"] ["tests"] ["benchmarks"] ["haskellPackages"] ]
    ++ onlyBuildOnDefaultSystem;

  # Remove build jobs for which cross compiling does not make sense.
  filterProject = noBuildList: mapAttrsRecursiveCond (a: !(isDerivation a)) (path: value:
    if (isDerivation value && (any (p: take (length p) path == p) noBuildList)) then null
    else value
  ) project;

  inherit (systems.examples) mingwW64 musl64;

  jobs = {
    inherit dockerImageArtifact;
    cardano-db-sync-linux = import ./nix/binary-release.nix {
      inherit pkgs project;
      platform = "linux";
      exes = filter (p: p.system == "x86_64-linux") (collect isDerivation jobs.musl64.exes);
    };
    cardano-db-sync-macos = import ./nix/binary-release.nix {
      inherit pkgs project;
      platform = "macos";
      exes = filter (p: p.system == "x86_64-darwin") (collect isDerivation jobs.native.exes);
    };
    native =
      let filteredBuilds = mapAttrsRecursiveCond (a: !(isList a)) (path: value:
        if (any (p: take (length p) path == p) onlyBuildOnDefaultSystem) then filter (s: !(elem s nonDefaultBuildSystems)) value else value)
        (packagePlatforms project);
      in (mapTestOn (__trace (__toJSON filteredBuilds) filteredBuilds));
    musl64 = mapTestOnCross musl64 (packagePlatformsCross (filterProject noMusl64Build));
  } // (mkRequiredJob (concatLists [
      (collectJobs jobs.native.checks)
      (collectJobs jobs.native.benchmarks)
      (collectJobs jobs.native.exes)
      [
       	jobs.native.cardano-node.x86_64-linux
        jobs.cardano-db-sync-linux
        jobs.cardano-db-sync-macos
        jobs.dockerImageArtifact
      ]
    ]));

in jobs
