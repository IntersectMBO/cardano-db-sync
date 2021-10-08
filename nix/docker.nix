############################################################################
# Docker image builder
#
# To build and load into the Docker engine:
#
#   docker load -i $(nix-build -A dockerImage --no-out-link)
#
# To launch with pre-loaded configuration, using the NETWORK env.
#
#    docker run \
#      -v $PATH_TO/node-ipc:/node-ipc \
#      -v $PATH_TO/pgpass:/configuration/pgpass \
#      -v $PWD/data:/var/lib/cdbsync \
#      -e NETWORK=mainnet|testnet \
#
# Provide an (almost*) complete command otherwise:
#
#   docker run \
#     -v $PWD/config/mainnet-config:/configuration/configuration.yaml
#     -v $PWD/node-ipc:/node-ipc \
#     -v $PWD/config/pgpass:/pgpass \
#     -v $PWD/data:/data \
#     -e PGPASSFILE=/pgpass
#     inputoutput/cardano-db-sync run \
#      --config /configuration/configuration.yaml \
#      --socket-path /node-ipc/node.socket \
#      --state-dir /data
#
#   * --schema-dir is set within the script
#
#  To launch the extended service include -e EXTENDED=true
#  To download and restore a snapshot include -e RESTORE_SNAPSHOT=https://update-cardano-mainnet.iohk.io/cardano-db-sync/db-sync-snapshot-schema-10-block-6014140-x86_64.tgz
#  See the latest releases for a recent snapshot https://github.com/input-output-hk/cardano-db-sync/releases
#  See the docker-compose.yml for demonstration of using Docker secrets instead of mounting a pgpass
#
#
############################################################################

{ cardanoLib
, dockerTools

# The main contents of the image.
, cardano-db-sync
, scripts
, extendedScripts

# Get the current commit
, gitrev

# Other things to include in the image.
, bashInteractive
, cacert
, coreutils
, curl
, findutils
, glibcLocales
, gnutar
, gzip
, iana-etc
, iproute
, iputils
, socat
, utillinux
, writeScript
, writeScriptBin
, runCommand
, runtimeShell
, lib
, libidn
, libpqxx
, postgresql

, dbSyncRepoName ? "inputoutput/cardano-db-sync"
}:

let

  env-shim = runCommand "env-shim" {} ''
    mkdir -p $out/usr/bin
    ln -s ${coreutils}/bin/env $out/usr/bin/env
  '';

  # Layer of tools which aren't going to change much between versions.
  baseImage = dockerTools.buildImage {
    name = "base-env";
    config.Env = [
      "NIX_SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt"
    ];
    contents = [
      bashInteractive   # Provide the BASH shell
      cacert            # X.509 certificates of public CA's
      coreutils         # Basic utilities expected in GNU OS's
      curl              # CLI tool for transferring files via URLs
      env-shim          # Make /usr/bin/env available
      findutils         # GNU find
      gnutar            # GNU tar
      glibcLocales      # Locale information for the GNU C Library
      gzip              # Gnuzip
      iana-etc          # IANA protocol and port number assignments
      iproute           # Utilities for controlling TCP/IP networking
      iputils           # Useful utilities for Linux networking
      libidn            # Library for internationalized domain names
      libpqxx           # A C++ library to access PostgreSQL databases
      postgresql        # A powerful, open source object-relational database system
      socat             # Utility for bidirectional data transfer
      utillinux         # System utilities for Linux
    ];
    runAsRoot = ''
      #!${runtimeShell}
      ${dockerTools.shadowSetup}
      mkdir -p /root
    '';
  };

  # The applications, without configuration, for which the target container is being built
  dockerWithoutConfig = dockerTools.buildImage {
    name = "docker-without-config";
    fromImage = baseImage;
    contents = [
      cardano-db-sync
    ];
  };

  dbSyncDockerImage = let
    clusterStatements = lib.concatStringsSep "\n" (lib.mapAttrsToList (env: script: let
      dbSyncScript = script.db-sync;
      dbSyncExtendedScript = extendedScripts.${env}.db-sync;
    in ''
        elif [[ "$NETWORK" == "${env}" ]]; then
            echo "Connecting to network: ${env}"
            if [[ ! -z "''${EXTENDED}" ]] && [[ "''${EXTENDED}" == true ]]
            then
              exec ${dbSyncExtendedScript}/bin/${dbSyncExtendedScript.name}
            else
              exec ${dbSyncScript}/bin/${dbSyncScript.name}
            fi
            echo "Cleaning up"
        ''
    ) scripts);
    genPgPass = writeScript "gen-pgpass" ''
      #!${runtimeShell}
      SECRET_DIR=$1
      echo $SECRET_DIR
      echo "Generating PGPASS file"
      POSTGRES_DB=''${POSTGRES_DB:-$(< ''${SECRET_DIR}/postgres_db)}
      POSTGRES_USER=''${POSTGRES_USER:-$(< ''${SECRET_DIR}/postgres_user)}
      POSTGRES_PASSWORD=''${POSTGRES_PASSWORD:-$(< ''${SECRET_DIR}/postgres_password)}
      echo "''${POSTGRES_HOST}:''${POSTGRES_PORT}:''${POSTGRES_DB}:''${POSTGRES_USER}:''${POSTGRES_PASSWORD}" > /configuration/pgpass
      chmod 0600 /configuration/pgpass
    '';
    entry-point = writeScriptBin "entry-point" ''
      #!${runtimeShell}
      mkdir -p /configuration
      if [ ! -f /configuration/pgpass ]
      then
        ${genPgPass} /run/secrets
      fi
      export PGPASSFILE=/configuration/pgpass
      # set up /tmp (override with TMPDIR variable)
      mkdir -p -m 1777 /tmp
      if [[ -z "$NETWORK" ]]; then
        echo "Connecting to network specified in configuration.yaml"
        if [[ ! -z "''${EXTENDED}" ]] && [[ "''${EXTENDED}" == true ]]
        then
          DBSYNC=${cardano-db-sync-extended}/bin/cardano-db-sync-extended
        else
          DBSYNC=${cardano-db-sync}/bin/cardano-db-sync
        fi

        set -euo pipefail
        ${scripts.mainnet.db-sync.passthru.service.restoreSnapshotScript}

        exec $DBSYNC \
          --schema-dir ${../schema} \
          --state-dir ${scripts.mainnet.db-sync.passthru.service.stateDir} $@
      ${clusterStatements}
      else
        echo "Managed configuration for network "$NETWORK" does not exist"
      fi
    '';
  in dockerTools.buildImage {
    name = dbSyncRepoName;
    fromImage = dockerWithoutConfig;
    tag = gitrev;
    created = "now";   # Set creation date to build time. Breaks reproducibility
    contents = [ entry-point ];
    config = {
      EntryPoint = [ "${entry-point}/bin/entry-point" ];
    };
  };
in dbSyncDockerImage
