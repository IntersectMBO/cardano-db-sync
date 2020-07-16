# our packages overlay
pkgs: _: with pkgs; {
  cardanoDbSyncHaskellPackages = callPackage ./haskell.nix {};

  # Grab the executable component of our package.
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync.components.exes)
      cardano-db-sync;
  inherit (cardanoDbSyncHaskellPackages.cardano-db-sync-extended.components.exes)
      cardano-db-sync-extended;
}
