############################################################################
# Builds Haskell packages with Haskell.nix
############################################################################

{ pkgs

# haskell.nix
, haskell

# Filtered sources of this project
, src

# Customisations for cross-compiling
, iohk-extras ? {}
, iohk-module ? {}

}:

let
  preCheck = ''
    echo pre-check
    initdb --encoding=UTF8 --locale=en_US.UTF-8 --username=postgres $NIX_BUILD_TOP/db-dir
    postgres -D $NIX_BUILD_TOP/db-dir &
    PSQL_PID=$!
    sleep 10
    if (echo '\q' | psql postgres postgres); then
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
    psql postgres postgres <<EOF
      create role $DBUSER with createdb login password '$DBPASS';
      alter user $DBUSER with superuser;
      create database $DBNAME with owner = $DBUSER;
      \\connect $DBNAME
      ALTER SCHEMA public   OWNER TO $DBUSER;
    EOF
  '';
  # our packages
  stack-pkgs = import ./.stack.nix/default.nix;

  # Grab the compiler name from stack-to-nix output.
  compiler = (stack-pkgs.extras {}).compiler.nix-name;

  # This creates the Haskell package set.
  # https://input-output-hk.github.io/haskell.nix/user-guide/projects/
  pkgSet = haskell.mkStackPkgSet {
    inherit stack-pkgs;
    modules = [
      # Add source filtering to local packages
      {
        packages.cardano-explorer.src = src + "/cardano-explorer";
        # packages.another-package = src + /another-package;
        packages.ekg.components.library.enableSeparateDataOutput = true;
      }

      # setup a psql server for the tests
      {
        packages = {
          cardano-explorer-db.components.tests.test-db = {
            build-tools = [ pkgs.postgresql ];
            inherit preCheck;
          };
          cardano-explorer.components.tests.test = {
            build-tools = [ pkgs.postgresql ];
            inherit preCheck;
          };
        };
      }

      # The iohk-module will supply us with the necessary
      # cross compilation plumbing to make Template Haskell
      # work when cross compiling.
      iohk-module
    ];

    pkg-def-extras = [
      # iohk-extras contains package overrides and patches per ghc version.
      iohk-extras.${compiler}
    ];
  };

in
  pkgSet.config.hsPkgs // { _config = pkgSet.config; }
