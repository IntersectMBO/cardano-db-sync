# ###########################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ haskell-nix
  # Map from URLs to input, for custom hackage sources
, inputMap
, defaultCompiler
, extraCompilers ? []
}:
let

  inherit (haskell-nix) haskellLib;

  preCheck = ''
    echo pre-check
    initdb --encoding=UTF8 --locale=en_US.UTF-8 --username=postgres $NIX_BUILD_TOP/db-dir
    postgres -D $NIX_BUILD_TOP/db-dir -c listen_addresses="" -k $TMP &
    PSQL_PID=$!
    echo $PSQL_PID > postgres.pid
    sleep 10
    if (echo '\q' | psql -h $TMP postgres postgres); then
      echo "PostgreSQL server is verified to be started."
    else
      echo "Failed to connect to local PostgreSQL server."
      exit 2
    fi
    ls -ltrh $NIX_BUILD_TOP
    DBUSER=$(whoami)
    DBNAME=$DBUSER
    export PGPASSFILE=$NIX_BUILD_TOP/pgpass
    echo "$TMP:5432:$DBUSER:$DBUSER:*" > $PGPASSFILE
    cp -vir ${../schema} ../schema
    chmod 600 $PGPASSFILE
    psql -h $TMP postgres postgres <<EOF
      create role $DBUSER with createdb login password '$DBPASS';
      alter user $DBUSER with superuser;
      create database $DBNAME with owner = $DBUSER;
      \\connect $DBNAME
      ALTER SCHEMA public   OWNER TO $DBUSER;
    EOF
  '';

  postCheck = ''
    echo post-check
    DBNAME=$(whoami)
    NAME=db_schema.sql
    mkdir -p $out/nix-support
    echo "Dumping schema to db_schema.sql"
    pg_dump -h $TMP -s $DBNAME > $out/$NAME
    kill $(cat postgres.pid)
    echo "Adding to build products..."
    echo "file binary-dist $out/$NAME" > $out/nix-support/hydra-build-products
  '';

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  project = haskell-nix.cabalProject' ({ pkgs, lib, config, ...}: {
    inherit inputMap;
    src = ../.;
    compiler-nix-name = lib.mkDefault defaultCompiler;
    flake.variants = lib.genAttrs extraCompilers (x: {compiler-nix-name = x;});

    shell = {
      name = "cabal-dev-shell";

      # These programs will be available inside the nix-shell.
      nativeBuildInputs = with pkgs.pkgsBuildBuild; [
        haskell-nix.cabal-install.${config.compiler-nix-name}
        ghcid
        haskell-language-server
        hlint
        nix
        pkgconfig
        sqlite-interactive
        tmux
        git
      ] ++ (with haskellPackages; [
        weeder
      ] ++ lib.optionals (config.compiler-nix-name != defaultCompiler) [
        # Tool(s) that require GHC 9.2+
        fourmolu
      ]);

      withHoogle = lib.mkDefault true;
    };
    modules = let
      rawProject = haskell-nix.cabalProject' (builtins.removeAttrs config [ "modules" ]);
      projectPackages = haskellLib.selectProjectPackages rawProject.hsPkgs;
      # deduce package names from the cabal project to avoid hard-coding them:
      projectPackagesNames = lib.attrNames projectPackages;
    in [
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;

        packages.plutus-ledger.doHaddock = false;
      }
      {
        packages = lib.genAttrs projectPackagesNames (name: {
          configureFlags = [ "--ghc-option=-Wall" "--ghc-option=-Werror" ];
        });
      }
      {
        packages.cardano-db.components.tests.test-db = {
            # Postgres 12+ won't build with musl
            build-tools = [ pkgs.pkgsBuildHost.postgresql_12 ];
            inherit preCheck;
            inherit postCheck;
          };
      }
      {
        packages.cardano-chain-gen.components.tests.cardano-chain-gen = {
            # Postgres 12+ won't build with musl
            build-tools = [ pkgs.pkgsBuildHost.postgresql_12 ];
            inherit preCheck;
            inherit postCheck;
          };
      }
      {
        packages.cardano-db-sync.components.exes.cardano-db-sync = {
          # todo, this shrinks the docker image by ~100mb
          #dontStrip = false;
        };
      }
      # Musl libc fully static build
      ({ pkgs, ... }:
        lib.mkIf pkgs.stdenv.hostPlatform.isMusl {
          # Haddock not working and not needed for cross builds
          doHaddock = false;
        })
      ({ pkgs, ... }:
        lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
          # systemd can't be statically linked
          packages.cardano-config.flags.systemd =
            !pkgs.stdenv.hostPlatform.isMusl;
          packages.cardano-node.flags.systemd =
            !pkgs.stdenv.hostPlatform.isMusl;
        })
      {
        packages.cardano-db-sync.package.extraSrcFiles = [ "../schema/*.sql" ];
        packages.cardano-chain-gen.package.extraSrcFiles =
          [ "../schema/*.sql" ];
        packages.cardano-db.package.extraSrcFiles = ["../config/pgpass-testnet"];
        packages.cardano-db.components.tests.test-db.extraSrcFiles =
          [ "../config/pgpass-mainnet" ];
        packages.cardano-db.components.tests.schema-rollback.extraSrcFiles = [ "src/Cardano/Db/Schema.hs" "src/Cardano/Db/Delete.hs" ];
      }
      ({ pkgs, ... }: {
        packages = lib.genAttrs [ "cardano-config" "cardano-db" ] (_: {
          components.library.build-tools =
            [ pkgs.pkgsBuildBuild.gitMinimal ];
        });
      })
      ({ pkgs, ... }: {
        # Use the VRF fork of libsodium when building cardano-node
        packages = {
          cardano-crypto-praos.components.library.pkgconfig = lib.mkForce [
            [ pkgs.libsodium-vrf ]
          ];
          cardano-crypto-class.components.library.pkgconfig = lib.mkForce [
            [ pkgs.libsodium-vrf pkgs.secp256k1 ]
          ];
        };
      })
    ];
  });

in project.appendOverlays [
  haskellLib.projectOverlays.projectComponents
  (final: prev: let inherit (final.pkgs) lib gitrev setGitRevForPaths; in {
    profiled = final.appendModule {
      modules = [{
        enableLibraryProfiling = true;
        enableProfiling = true;
      }];
    };
    # add passthru and gitrev to hsPkgs:
    hsPkgs = lib.mapAttrsRecursiveCond (v: !(lib.isDerivation v))
      (path: value:
        if (lib.isAttrs value)
        then lib.recursiveUpdate value {
            # Also add convenient passthru to some alternative compilation configurations:
            passthru = {
              profiled = lib.getAttrFromPath path final.profiled.hsPkgs;
            };
          }
        else value)
      (setGitRevForPaths gitrev [
        "cardano-db-sync.components.exes.cardano-db-sync"
        "cardano-smash-server.components.exes.cardano-smash-server"
        "cardano-db-tool.components.exes.cardano-db-tool"] prev.hsPkgs);
  })
]
