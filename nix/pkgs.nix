# our packages overlay
pkgs: super: with pkgs;
  let
    compiler = config.haskellNix.compiler or "ghc8102";
    src = haskell-nix.haskellLib.cleanGit {
      src = ../.;
      name = "cardano-db-sync";
    };
  in {
    inherit src;

    cardanoDbSyncHaskellPackages = callPackage ./haskell.nix {
      inherit compiler gitrev src;
    };

  # Grab the executable component of our package.
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync.components.exes)
      cardano-db-sync;
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync-extended.components.exes)
      cardano-db-sync-extended;
  inherit (cardanoDbSyncHaskellPackages.cardano-node.components.exes)
      cardano-node;

  inherit ((haskell-nix.hackage-package {
    name = "hlint";
    version = "3.1.6";
    compiler-nix-name = compiler;
    inherit (cardanoDbSyncHaskellPackages) index-state;
  }).components.exes) hlint;

  inherit ((haskell-nix.hackage-package {
    name = "stylish-haskell";
    version = "0.12.2.0";
    compiler-nix-name = compiler;
    inherit (cardanoDbSyncHaskellPackages) index-state;
  }).components.exes) stylish-haskell;

  # systemd can't be statically linked:
  postgresql = super.postgresql.override {
    enableSystemd = stdenv.hostPlatform.isLinux && !stdenv.hostPlatform.isMusl;
  };
}
