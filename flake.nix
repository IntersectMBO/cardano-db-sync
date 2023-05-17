{
  description = "cardano-db-sync";

  inputs = {
    nixpkgs.follows = "haskellNix/nixpkgs-unstable";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    utils.url = "github:numtide/flake-utils";
    iohkNix = {
      url = "github:input-output-hk/iohk-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:input-output-hk/flake-compat/fixes";
      flake = false;
    };
    CHaP = {
      url = "github:input-output-hk/cardano-haskell-packages?ref=repo";
      flake = false;
    };
    # Custom user config (default: empty), eg.:
    # { outputs = {...}: {
    #   # Cutomize listeming port of node scripts:
    #   nixosModules.cardano-node = {
    #     services.cardano-node.port = 3002;
    #   };
    # };
    customConfig.url = "github:input-output-hk/empty-flake";
    tullia.url = "github:input-output-hk/tullia";
    std.follows = "tullia/std";
  };

  outputs = { self, iohkNix, haskellNix, CHaP, nixpkgs, utils, tullia, std, flake-compat, ... }@inputs:
    let
      inherit (haskellNix) config;
      inherit (nixpkgs) lib;
      inherit (utils.lib) eachSystem flattenTree;
      inherit (iohkNix.lib) prefixNamesWith;

      supportedSystems = import ./supported-systems.nix;

      inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };

      customConfig =
        lib.recursiveUpdate (import ./nix/custom-config.nix customConfig)
        inputs.customConfig;

      overlays = [
        # crypto needs to come before hasell.nix. 
        # FIXME: _THIS_IS_BAD_
        iohkNix.overlays.crypto
        haskellNix.overlay
        iohkNix.overlays.haskell-nix-extra
        iohkNix.overlays.utils
        iohkNix.overlays.cardano-lib
        (final: prev: {
          inherit flake-compat customConfig;
          gitrev = self.rev or "dirty";
          commonLib = lib // iohkNix.lib;
          # cardanoLib = rec {
          #   inherit (cardano-world.${final.system}.cardano) environments;
          #   forEnvironments = f: lib.mapAttrs
          #     (name: env: f (env // { inherit name; }))
          #     environments;
          # };
          schema = ./schema;
          ciJobs = self.ciJobs.${final.system};
        })
        (import ./nix/pkgs.nix)
        self.overlay
        # I _do not_ understand why we need it _here_, and having it haskell.nix
        # does not work.
        (final: prev: prev.lib.optionalAttrs prev.stdenv.hostPlatform.isMusl {
          # this is needed because postgresql links against libicu
          # which we build only statically (for musl), and that then
          # needs -lstdc++ as well.
          postgresql = prev.postgresql.overrideAttrs (old: {
            NIX_LDFLAGS = "-lstdc++";
          });
        })
      ];

    in eachSystem supportedSystems (system:
      let
        pkgs = import nixpkgs { inherit system overlays config; };

        inherit (pkgs.stdenv) hostPlatform;

        project = pkgs.cardanoDbSyncProject;

        hlint = pkgs.callPackage pkgs.hlintCheck {
          inherit (project.args) src;
        };

        nixosTests = import ./nix/nixos/tests {
          inherit pkgs;
        };

        checks = project.flake'.checks // {
          inherit hlint;
        } // lib.optionalAttrs hostPlatform.isLinux (prefixNamesWith "nixosTests/" nixosTests);

        # This is used by `nix develop` to open a devShell
        devShells.default = project.shell;

        scripts = flattenTree pkgs.scripts;

        packages = project.exes // scripts // {
          # Built by `nix build .`
          default = project.exes.cardano-db-sync;
          inherit (pkgs) dockerImage cardano-node cardano-cli cardano-smash-server-no-basic-auth;
        } # Add checks to be able to build them individually:
        // (prefixNamesWith "checks/" checks);

        ciJobs = lib.recursiveUpdate (project.flake {
          crossPlatforms = p:
            lib.optional hostPlatform.isLinux p.musl64;
        }).ciJobs ({
          packages = {
            inherit scripts;
            inherit (pkgs) cardano-node cardano-smash-server-no-basic-auth checkCabalProject;
          };
        } // lib.optionalAttrs hostPlatform.isLinux {
          inherit (pkgs) dockerImage;
          checks = {
            inherit nixosTests;
            inherit hlint;
          };
          cardano-db-sync-linux = import ./nix/binary-release.nix {
            inherit pkgs project;
            inherit (packages.default.identifier) version;
            platform = "linux";
            exes = lib.collect lib.isDerivation project.projectCross.musl64.exes;
          };
        } // lib.optionalAttrs hostPlatform.isMacOS {
          cardano-db-sync-macos = import ./nix/binary-release.nix {
            inherit pkgs project;
            inherit (packages.default.identifier) version;
            platform = "macos";
            exes = lib.collect lib.isDerivation project.exes;
          };
        });

      in {

        inherit checks devShells packages;

        apps = {
          checkCabalProject = { type = "app"; program = "${pkgs.checkCabalProject}"; };
        };

        legacyPackages = pkgs;

        ciJobs =
          let
            nonRequiredPaths = [
              ".*musl\\.devShells\\..*"
            ];
          in
          pkgs.callPackages iohkNix.utils.ciJobsAggregates
            {
              inherit ciJobs;
              nonRequiredPaths = map (r: p: builtins.match r p != null) nonRequiredPaths;
            } // ciJobs;

        hydraJobs = ciJobs;

      } // tullia.fromSimple system (import ./nix/tullia.nix)) // {

        # allows precise paths (avoid fallbacks) with nix build/eval:
        outputs = self;

        overlay = final: prev:
            {
              cardanoDbSyncProject = (import ./nix/haskell.nix {
                inherit (final) haskell-nix;
                inherit inputMap;
              }).appendModule customConfig.haskellNix;
            inherit ((import flake-compat {
          pkgs = final;
          inherit (final.cardanoDbSyncProject.hsPkgs.cardano-node) src;
        }).defaultNix.packages.${final.system}) cardano-node cardano-cli;
            inherit (final.cardanoDbSyncProject.exes) cardano-db-sync cardano-smash-server cardano-db-tool;
          };
        nixosModules = {
          cardano-db-sync = { pkgs, lib, ... }: {
            imports = [ ./nix/nixos/cardano-db-sync-service.nix ];
            services.cardano-db-sync.dbSyncPkgs =
              lib.mkDefault self.legacyPackages.${pkgs.system};
          };
        };
      };

  nixConfig = {
    extra-substituters = [ "https://cache.iog.io" ];
    extra-trusted-public-keys = [ "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=" ];
  };
}
