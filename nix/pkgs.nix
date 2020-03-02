# our packages overlay
pkgs: _: {
  cardanoDbSyncHaskellPackages = pkgs.callPackage ./haskell.nix {};
}
