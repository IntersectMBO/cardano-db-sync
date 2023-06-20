# our packages overlay
final: prev:
with final;
let compiler = cardanoDbSyncProject.args.compiler-nix-name;
in {

  cardanoDbSyncHaskellPackages = cardanoDbSyncProject.hsPkgs;

  cardano-smash-server-no-basic-auth = (cardanoDbSyncProject.appendModule {
    modules = [{packages.cardano-smash-server.flags.disable-basic-auth = true;}];
  }).exes.cardano-smash-server;

  haskell-language-server = haskell-nix.tool compiler "haskell-language-server" {
    src = haskell-nix.sources."hls-1.10";
  };

  hlint = haskell-nix.tool compiler "hlint" {
    version = "3.2.7";
  };

  # systemd can't be statically linked:
  postgresql = (final.postgresql_11
    .overrideAttrs (_: { dontDisableStatic = stdenv.hostPlatform.isMusl; }))
    .override {
      enableSystemd = stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isMusl;
      gssSupport = stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isMusl;
    };

  python3 = prev.python3.override {
    packageOverrides = python-final: python-prev: {
      uvloop = python-prev.uvloop.overrideAttrs (_: { doInstallCheck = false; });
    };
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
