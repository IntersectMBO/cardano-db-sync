# our packages overlay
pkgs: _: with pkgs;
  let
    compiler = config.haskellNix.compiler or "ghc865";
  in {
    cardanoDbSyncHaskellPackages = callPackage ./haskell.nix {
      inherit compiler;
    };

  # Grab the executable component of our package.
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync.components.exes)
      cardano-db-sync;
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync-extended.components.exes)
      cardano-db-sync-extended;

  inherit ((haskell-nix.hackage-package {
    name = "hlint";
    version = "3.1.6";
    compiler-nix-name = compiler;
    inherit (cardanoDbSyncHaskellPackages) index-state;
  }).components.exes) hlint;
}
