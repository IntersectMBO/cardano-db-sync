{ haskellPackages, haskell }:

haskellPackages.override {
  overrides = self: super: {
    ci-info = self.callPackage ./ci-info.nix {};
    graphql-engine = haskell.lib.dontHaddock (haskell.lib.dontCheck (self.callPackage ./graphql-engine.nix {}));
    graphql-parser = self.callPackage ./graphql-parser.nix {};
    pg-client = self.callPackage ./pg-client.nix {};
    shakespeare = self.callHackageDirect { pkg = "shakespeare"; ver = "2.0.22"; sha256 = "1d7byyrc2adyxrgcrlxyyffpr4wjcgcnvdb8916ad6qpqjhqxx72"; } {};
    stm-hamt = haskell.lib.doJailbreak (haskell.lib.unmarkBroken super.stm-hamt);
    superbuffer = haskell.lib.doJailbreak (haskell.lib.unmarkBroken super.superbuffer);
    Spock-core = haskell.lib.unmarkBroken super.Spock-core;
    stm-containers = haskell.lib.unmarkBroken super.stm-containers;
  };
}
