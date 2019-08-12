{
  extras = hackage:
    {
      packages = {
        "hedgehog" = (((hackage.hedgehog)."1.0").revisions).default;
        "esqueleto" = (((hackage.esqueleto)."3.0.0").revisions).default;
        "persistent" = (((hackage.persistent)."2.10.0").revisions).default;
        "persistent-postgresql" = (((hackage.persistent-postgresql)."2.10.0").revisions).default;
        "persistent-template" = (((hackage.persistent-template)."2.7.2").revisions).default;
        } // {
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