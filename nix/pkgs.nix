# our packages overlay
final: prev:
with final;
let compiler = config.haskellNix.compiler or "ghc8104";
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

  cardanoDbSyncProject = callPackage ./haskell.nix { inherit compiler; };

  cardanoDbSyncHaskellPackages = cardanoDbSyncProject.hsPkgs;

  # Grab the executable component of our package.
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync.components.exes)
    cardano-db-sync;
  inherit (cardanoDbSyncHaskellPackages.cardano-db-tool.components.exes)
    cardano-db-tool;
  inherit (cardanoDbSyncHaskellPackages.cardano-node.components.exes)
    cardano-node;
  inherit (cardanoDbSyncHaskellPackages.cardano-smash-server.components.exes)
    cardano-smash-server;
  cardano-smash-server-no-basic-auth = (cardanoDbSyncProject.appendModule {
    modules = [{packages.cardano-smash-server.flags.disable-basic-auth = true;}];
  }).hsPkgs.cardano-smash-server.components.exes.cardano-smash-server;

  cabal = haskell-nix.tool compiler "cabal" {
    version = "latest";
  };

  hlint = haskell-nix.tool compiler "hlint" {
    version = "3.2.7";
  };

  stylish-haskell = haskell-nix.tool compiler "stylish-haskell" {
    version = "latest";
  };

  # systemd can't be statically linked:
  postgresql = prev.postgresql.override {
    enableSystemd = stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isMusl;
  };

  scripts = import ./scripts.nix { inherit pkgs; };

  dockerImage = let
    defaultConfig = rec {
      services.cardano-db-sync = {
        restoreSnapshot = lib.mkDefault "$RESTORE_SNAPSHOT";
        socketPath = lib.mkDefault ("/node-ipc/node.socket");
        postgres.generatePGPASS = false;
      };
    };
  in callPackage ./docker.nix {
    scripts = import ./scripts.nix {
      inherit pkgs;
      customConfigs = [ defaultConfig customConfig ];
    };
  };
}
