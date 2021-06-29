# our packages overlay
final: prev: with final;
let
  compiler = config.haskellNix.compiler or "ghc8104";
in {
  src = haskell-nix.haskellLib.cleanGit {
    src = ../.;
    name = "cardano-db-sync";
  };

  schema = haskell-nix.haskellLib.cleanGit {
    src = ../.;
    subDir = "schema";
    name = "cardano-db-sync-schema";
  };

  cardanoDbSyncProject = callPackage ./haskell.nix {
    inherit compiler;
  };

  cardanoDbSyncHaskellPackages = cardanoDbSyncProject.hsPkgs;

  # Grab the executable component of our package.
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync.components.exes)
      cardano-db-sync;
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync-extended.components.exes)
      cardano-db-sync-extended;
  inherit (cardanoDbSyncHaskellPackages.cardano-db-tool.components.exes)
    cardano-db-tool;
  inherit (cardanoDbSyncHaskellPackages.cardano-node.components.exes)
      cardano-node;

  cabal = haskell-nix.tool compiler "cabal" {
    version = "latest";
    inherit (cardanoDbSyncProject) index-state;
  };

  hlint = haskell-nix.tool compiler "hlint" {
    version = "3.2.7";
    inherit (cardanoDbSyncProject) index-state;
  };

  stylish-haskell = haskell-nix.tool compiler "stylish-haskell" {
    version = "latest";
    inherit (cardanoDbSyncProject) index-state;
  };

  # systemd can't be statically linked:
  postgresql = prev.postgresql.override {
    enableSystemd = stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isMusl;
  };

  scripts = import ./scripts.nix { inherit pkgs; };

  dockerImage = let
    defaultConfig = rec {
      services.cardano-db-sync = {
        socketPath = lib.mkDefault ("/node-ipc/node.socket");
        postgres.generatePGPASS = false;
      };
    };
  in callPackage ./docker.nix {
    scripts = import ./scripts.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
    extendedScripts = import ./scripts.nix {
      inherit pkgs;
      customConfigs = [
        defaultConfig customConfig
        { services.cardano-db-sync.extended = true; }
      ];
    };
  };
}
