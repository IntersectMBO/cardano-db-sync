{ sources ? import ../sources.nix }:
with
  { overlay = self: super:
      { inherit (import sources.niv {}) niv;
        packages = self.callPackages ./packages.nix { cardano-graphql-src = sources.cardano-graphql; };
        node = super.nodejs-12_x;
        inherit (import sources.yarn2nix { pkgs = self; }) yarn2nix mkYarnModules mkYarnPackage;
      };
  };
import sources.nixpkgs
  { overlays = [ overlay ] ; config = {}; }
