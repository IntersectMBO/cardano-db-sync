{
  extras = hackage:
    {
      packages = {} // {
        cardano-explorer-core = ./cardano-explorer-core.nix;
        cardano-explorer-db-node = ./cardano-explorer-db-node.nix;
        cardano-explorer = ./cardano-explorer.nix;
        };
      compiler.version = "8.6.5";
      compiler.nix-name = "ghc865";
      };
  resolver = "lts-13.28";
  compiler = "ghc-8.6.5";
  }