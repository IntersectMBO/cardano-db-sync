{
  description = "cardano-db-sync";

  inputs = {
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    utils.url = "github:numtide/flake-utils";
    customConfig = {
      url = "path:./custom-config";
    };
  };

  outputs = { self, iohkNix, haskellNix, nixpkgs, utils, customConfig, ... }:
    let
      inherit (haskellNix.internal) config;
      inherit (nixpkgs) lib;
      inherit (lib) systems mapAttrs recursiveUpdate mkDefault optionalAttrs nameValuePair
        attrNames getAttrs head;
      inherit (utils.lib) eachSystem mkApp flattenTree;
      inherit (iohkNix.lib) prefixNamesWith collectExes;

      supportedSystems = import ./supported-systems.nix;
      defaultSystem = head supportedSystems;

      overlays = [
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.crypto
        iohkNix.overlays.cardano-lib
        iohkNix.overlays.utils
        (final: prev: {
          customConfig = recursiveUpdate
            (import ./custom-config final.customConfig)
            customConfig.outputs;
          gitrev = self.rev or "dirty";
          commonLib = lib
            // iohkNix.lib;
        })
        (import ./nix/pkgs.nix)
      ];

    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system overlays config; };

        devShell = import ./shell.nix { inherit pkgs; };

        flake = pkgs.cardanoDbSyncProject.flake {};

        muslFlake = (import nixpkgs { inherit system overlays config;
          crossSystem = systems.examples.musl64;
        }).cardanoDbSyncProject.flake {};

        scripts = flattenTree pkgs.scripts;

        checkNames = attrNames flake.checks;

         checks =
          # checks run on default system only;
          optionalAttrs (system == defaultSystem) {
            hlint = pkgs.callPackage pkgs.hlintCheck {
              inherit (pkgs.cardanoDbSyncProject.projectModule) src;
            };
            stylish-haskell = pkgs.callPackage pkgs.stylishHaskellCheck {
              inherit (pkgs.cardanoDbSyncProject.projectModule) src;
            };
          };

        exes = collectExes flake.packages;
        exeNames = attrNames exes;
        lazyCollectExe = p: getAttrs exeNames (collectExes p);

        packages = {
          inherit (devShell) devops;
          inherit (pkgs) cardano-db-sync cardano-node
            dockerImage;
        }
        // exes
        // (prefixNamesWith "static/"
              (mapAttrs pkgs.rewriteStatic (lazyCollectExe
                (if system == "x86_64-darwin" then flake else muslFlake).packages)))
        // scripts
        # Add checks to be able to build them individually
        // (prefixNamesWith "checks/" checks);

      in recursiveUpdate flake {

        inherit packages checks;

        legacyPackages = pkgs;

        # Built by `nix build .`
        defaultPackage = flake.packages."cardano-db-sync:exe:cardano-db-sync";

        # Run by `nix run .`
        defaultApp = flake.apps."cardano-db-sync:exe:cardano-db-sync";

        # This is used by `nix develop .` to open a devShell
        inherit devShell;

        apps = {
          repl = mkApp {
            drv = pkgs.writeShellScriptBin "repl" ''
              confnix=$(mktemp)
              echo "builtins.getFlake (toString $(git rev-parse --show-toplevel))" >$confnix
              trap "rm $confnix" EXIT
              nix repl $confnix
          '';
          };
          cardano-node = { type = "app"; program = pkgs.cardano-node.exePath; };
        } # nix run .#<exe>
        // (collectExes flake.apps);

      }
    ) // {
      overlay = final: prev: with self.legacyPackages.${final.system}; {
        inherit cardano-db-sync cardano-node
            dockerImage;
      };
      nixosModules = {
        cardano-db-sync = { pkgs, lib, ... }: {
          imports = [ ./nix/nixos/cardano-db-sync-service.nix ];
          services.cardano-db-sync.dbSyncPkgs = lib.mkDefault self.legacyPackages.${pkgs.system};
        };
      };
    };
}
