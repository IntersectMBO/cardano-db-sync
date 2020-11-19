############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################
{ lib
, stdenv
, haskell-nix
, buildPackages
, config ? {}
# GHC attribute name
, compiler ? config.haskellNix.compiler or "ghc8102"
# Enable profiling
, profiling ? config.haskellNix.profiling or false
, postgresql
# Version info, to be passed when not building from a git work tree
, gitrev ? null
}:
let
  preCheck = ''
    echo pre-check
    initdb --encoding=UTF8 --locale=en_US.UTF-8 --username=postgres $NIX_BUILD_TOP/db-dir
    postgres -D $NIX_BUILD_TOP/db-dir -k /tmp &
    PSQL_PID=$!
    sleep 10
    if (echo '\q' | psql -h /tmp postgres postgres); then
      echo "PostgreSQL server is verified to be started."
    else
      echo "Failed to connect to local PostgreSQL server."
      exit 2
    fi
    ls -ltrh $NIX_BUILD_TOP
    DBUSER=nixbld
    DBNAME=nixbld
    export PGPASSFILE=$NIX_BUILD_TOP/pgpass
    echo "/tmp:5432:$DBUSER:$DBUSER:*" > $PGPASSFILE
    cp -vir ${../schema} ../schema
    chmod 600 $PGPASSFILE
    psql -h /tmp postgres postgres <<EOF
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
    pg_dump -h /tmp -s $DBNAME > $out/$NAME
    echo "Adding to build products..."
    echo "file binary-dist $out/$NAME" > $out/nix-support/hydra-build-products
  '';

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell-nix.cabalProject {
    src = haskell-nix.haskellLib.cleanGit {
      src = ../.;
      name = "cardano-db-sync";
    };
    compiler-nix-name = compiler;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-db-sync.src = haskell-nix.haskellLib.cleanGit {
          src = ../.;
          subDir = "cardano-db-sync";
          name = "cardano-db-sync";
        };
        packages.cardano-db-sync-extneded.src = haskell-nix.haskellLib.cleanGit {
          src = ../.;
          subDir = "cardano-db-sync-extended";
          name = "cardano-db-sync";
        };
      }
      {
        # Stamp executables with the git revision
        packages = lib.genAttrs ["cardano-db-sync" "cardano-db-sync-extended"] (name: {
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
        packages.cardano-db-sync.configureFlags = [ "--ghc-option=-Wall" "--ghc-option=-Werror" ];
        packages.cardano-db-sync-extended.configureFlags = [ "--ghc-option=-Wall" "--ghc-option=-Werror" ];
        enableLibraryProfiling = profiling;
      }
      {
        packages.cardano-db.components.tests.test-db = {
          build-tools = [ postgresql ];
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
    ];
  };
  # setGitRev is a postInstall script to stamp executables with
  # version info. It uses the "gitrev" argument, if set. Otherwise,
  # the revision is sourced from the local git work tree.
  setGitRev = ''${haskellBuildUtils}/bin/set-git-rev "${gitrev'}" $out/bin/*'';
  # package with libsodium:
  gitrev' = if (gitrev == null)
    then buildPackages.commonLib.commitIdFromGitRepoOrZero ../.git
    else gitrev;
  haskellBuildUtils = buildPackages.haskellBuildUtils.package;
in
  pkgSet
