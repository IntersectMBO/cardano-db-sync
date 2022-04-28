# ###########################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib, stdenv, haskell-nix, buildPackages, src, config ? { }
  # GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc8107"
  # Enable profiling
, profiling ? config.haskellNix.profiling or false
  # Version info, to be passed when not building from a git work tree
, gitrev }:
let

  projectPackages = lib.attrNames (haskell-nix.haskellLib.selectProjectPackages
    (haskell-nix.cabalProject' {
      inherit src;
      compiler-nix-name = compiler;
    }).hsPkgs);

  preCheck = ''
    echo pre-check
    initdb --encoding=UTF8 --locale=en_US.UTF-8 --username=postgres $NIX_BUILD_TOP/db-dir
    postgres -D $NIX_BUILD_TOP/db-dir -k $TMP &
    PSQL_PID=$!
    sleep 10
    if (echo '\q' | psql -h $TMP postgres postgres); then
      echo "PostgreSQL server is verified to be started."
    else
      echo "Failed to connect to local PostgreSQL server."
      exit 2
    fi
    ls -ltrh $NIX_BUILD_TOP
    DBUSER=nixbld
    DBNAME=nixbld
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
    DBNAME=nixbld
    NAME=db_schema.sql
    mkdir -p $out/nix-support
    echo "Dumping schema to db_schema.sql"
    pg_dump -h $TMP -s $DBNAME > $out/$NAME
    echo "Adding to build products..."
    echo "file binary-dist $out/$NAME" > $out/nix-support/hydra-build-products
  '';

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject' {
    inherit src;
    compiler-nix-name = compiler;
    modules = [
      {
        # Stamp executables with the git revision
        packages = lib.genAttrs [ "cardano-db-sync" "cardano-smash-server" "cardano-db-tool" ] (name: {
          components.exes.${name}.postInstall = ''
            ${setGitRev}
          '';
        });
      }
      {
        # Packages we wish to ignore version bounds of.
        # This is similar to jailbreakCabal, however it
        # does not require any messing with cabal files.
        packages.katip.doExactConfig = true;

        # split data output for ekg to reduce closure size
        packages.ekg.components.library.enableSeparateDataOutput = true;
        enableLibraryProfiling = profiling;

        packages.plutus-ledger.doHaddock = false;
      }
      {
        packages = lib.genAttrs projectPackages (name: {
          configureFlags = [ "--ghc-option=-Wall" "--ghc-option=-Werror" ];
        });
      }
      {
        packages.cardano-db.components.tests.test-db = {
            build-tools = [ buildPackages.postgresql ];
            inherit preCheck;
            inherit postCheck;
          };
      }
      {
        packages.cardano-chain-gen.components.tests.cardano-chain-gen = {
            build-tools = [ buildPackages.postgresql ];
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
        lib.mkIf stdenv.hostPlatform.isMusl (let
          # Module options which adds GHC flags and libraries for a fully static build
          fullyStaticOptions = {
            enableShared = false;
            enableStatic = true;
            configureFlags = [
              "--ghc-option=-optl=-lssl"
              "--ghc-option=-optl=-lcrypto"
              "--ghc-option=-optl=-L${pkgs.openssl.out}/lib"
            ];
          };
        in {
          packages = lib.genAttrs projectPackages (name: fullyStaticOptions);

          # Haddock not working and not needed for cross builds
          doHaddock = false;
        }))
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
      }
      ({ pkgs, ... }: {
        packages = lib.genAttrs [ "cardano-config" "cardano-db" ] (_: {
          components.library.build-tools =
            [ pkgs.buildPackages.buildPackages.gitMinimal ];
        });
      })
      ({ pkgs, ... }: {
        # Use the VRF fork of libsodium when building cardano-node
        packages =
          lib.genAttrs [ "cardano-crypto-praos" "cardano-crypto-class" ] (_: {
            components.library.pkgconfig =
              lib.mkForce [ [ pkgs.libsodium-vrf ] ];
          });
      })
    ];
  };
  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  setGitRev = ''
    ${buildPackages.haskellBuildUtils}/bin/set-git-rev "${gitrev}" $out/bin/*'';
in pkgSet
