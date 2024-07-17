{ # Package set passed to NixOS module
  pkgs

# Tools for building images
, buildEnv, dockerTools, runCommand, runtimeShell, writeScript, writeScriptBin
, evalService, cardanoLib

# Image dependencies

, bashInteractive, cacert, cardano-cli, cardano-db-sync, cardano-db-tool
, cardano-smash-server, coreutils, curl, findutils, getconf, glibcLocales
, gnused, gnutar, gzip, jq, iana-etc, iproute, iputils, lib, libidn, libpqxx
, postgresql_14, socat, utillinux
}:

let
  concatMapAttrs = f: attrs:
    lib.concatStringsSep "\n" (lib.mapAttrsToList f attrs);

  env-shim = runCommand "env-shim" { } ''
    mkdir -p $out/usr/bin
    ln -s ${coreutils}/bin/env $out/usr/bin/env
  '';

  baseImage = dockerTools.buildImage {
    name = "cardano-db-sync-base-env";
    config.Env = [ "NIX_SSL_CERT_FILE=${cacert}/etc/ssl/certs/ca-bundle.crt" ];

    copyToRoot = buildEnv {
      name = "base-image-env";
      paths = [
        bashInteractive # Provide the BASH shell
        cacert # X.509 certificates of public CAs
        coreutils # Basic utilities expected in GNU OSs
        curl # CLI tool for transferring files via URLs
        env-shim # Make /usr/bin/env available
        findutils # GNU find
        getconf # get num cpus
        glibcLocales # Locale information for the GNU C Library
        gnused # GNU sed
        gnutar # GNU tar
        gzip # Gnuzip
        jq # JSON processor
        iana-etc # IANA protocol and port number assignments
        iproute # Utilities for controlling TCP/IP networking
        iputils # Useful utilities for Linux networking
        libidn # Library for internationalized domain names
        libpqxx # A C++ library to access PostgreSQL databases
        postgresql_14 # A powerful, open source object-relational database system
        socat # Utility for bidirectional data transfer
        utillinux # System utilities for Linux
        cardano-cli # tool for interacting with cardano-node
        cardano-db-tool # utilities for creating database snapshots
      ];
    };

    runAsRoot = ''
      #!${runtimeShell}
      ${dockerTools.shadowSetup}
      mkdir -p /root
    '';
  };

  # Contains software binaries, but no application configuration or entrypoint
  dbSyncBaseImage = dockerTools.buildImage {
    name = "cardano-db-sync-env";
    fromImage = baseImage;
    copyToRoot = buildEnv {
      name = "cardano-db-sync-image-bin-env";
      paths = [ cardano-db-sync ];
    };
  };

  # Entrypoint for the final image layer
  dbSyncEntrypoint =
    let
      clusterStatements =
        concatMapAttrs
          (env: script:
            let
              dbSyncScript = script.db-sync;
            in ''
              elif [[ "$NETWORK" == "${env}" ]]; then
                echo "Connecting to network: ${env}"
                exec ${dbSyncScript}/bin/${dbSyncScript.name}
                echo "Cleaning up"
              '')
            scripts;

      scripts = cardanoLib.forEnvironments (env: mkScript (mkService env));

      mkScript = service: lib.recurseIntoAttrs {
        db-sync = pkgs.writeScriptBin "cardano-db-sync-${service.cluster}" ''
          #!${runtimeShell}
          set -euo pipefail
          ${service.script} $@
        '' // {
          passthru = { inherit service; };
        };
      };

      mkService = env: evalService {
        inherit pkgs;
        customConfigs = [defaultConfig];
        serviceName = "cardano-db-sync";

        modules = [
          ./nixos/cardano-db-sync-service.nix

          {
            services.cardano-db-sync = {
              postgres.user = lib.mkDefault "*";
              environment = lib.mkDefault env;
              cluster = lib.mkDefault env.name;
              dbSyncPkgs = lib.mkDefault pkgs;
            };
          }
        ];
      };

      defaultConfig = {
        services.cardano-db-sync = {
          restoreSnapshot = lib.mkDefault "$RESTORE_SNAPSHOT";
          socketPath = lib.mkDefault ("/node-ipc/node.socket");
          postgres.generatePGPASS = false;
        };
      };

    in
      writeScriptBin "entrypoint" ''
        #!${runtimeShell}

        ${genPgpass}
        export PGPASSFILE=/configuration/pgpass

        ${setupTmp}

        if [[ -z "$NETWORK" ]]; then
          echo "Connecting to network specified in configuration.yaml"
          DBSYNC=${cardano-db-sync}/bin/cardano-db-sync

          set -euo pipefail
          ${scripts.mainnet.db-sync.passthru.service.restoreSnapshotScript}

          if [[ "''${DISABLE_LEDGER:-N}" == "Y" ]]; then
            LEDGER_OPTS="--disable-ledger"
          else
            LEDGER_OPTS="--state-dir ${scripts.mainnet.db-sync.passthru.service.stateDir}"
          fi

          exec $DBSYNC --schema-dir ${../schema} ''${LEDGER_OPTS} $@

        ${clusterStatements}
        else
          echo "Managed configuration for network "$NETWORK" does not exist"
        fi
      '';

  # Contains software binaries, but no application configuration or entrypoint
  smashBaseImage = dockerTools.buildImage {
    name = "cardano-db-sync-env";
    fromImage = baseImage;
    copyToRoot = buildEnv {
      name = "cardano-smash-server-image-bin-env";
      paths = [ cardano-smash-server ];
    };
  };

  smashEntrypoint =
    let
      clusterStatements =
        concatMapAttrs
          (env: script:
            let
              smashScript = script.smash;
            in ''
              elif [[ "$NETWORK" == "${env}" ]]; then
                echo "Connecting to network: ${env}"
                exec ${smashScript}/bin/${smashScript.name}
                echo "Cleaning up"
            '')
          scripts;

      scripts = cardanoLib.forEnvironments (env: mkScript (mkService env));

      mkScript = service: lib.recurseIntoAttrs {
        smash = pkgs.writeScriptBin "smash-${service.environment.name}" ''
          #!${runtimeShell}
          set -euo pipefail
          ${service.script} $@
        '';
      } // {
        passthru = { inherit service; };
      };

      mkService = env: evalService {
        inherit pkgs;
        customConfigs = [defaultConfig];
        serviceName = "smash";

        modules = [
          ./nixos/smash-service.nix

          {
            services.smash = {
              postgres.user = lib.mkDefault "*";
              environment = lib.mkDefault env;
              dbSyncPkgs = lib.mkDefault pkgs;
              package = lib.mkDefault pkgs.cardano-smash-server;
              admins = lib.mkDefault "/configuration/admins.csv";
            };
          }
        ];
      };

      defaultConfig = {
        services.smash = {
          socketPath = lib.mkDefault "/node-ipc/node.socket";
          postgres.generetaePGPass = false;
        };
      };

    in
      writeScriptBin "entrypoint" ''
        #!${runtimeShell}

        ${genPgpass}
        export PGPASSFILE=/configuration/pgpass

        ${setupTmp}

        # Add an admin csv
        SMASH_USER="''${SMASH_USER:-smash-admin}"
        SMASH_PASSWORD="''${SMASH_PASSWORD:-smash-password}"
        echo "''${SMASH_USER},''${SMASH_PASSWORD}" > /configuration/admins.csv

        if [[ -z "$NETWORK" ]]; then
          echo "Connecting to network specified in configuration.yaml"
          set -euo pipefail
          SMASH=${cardano-smash-server}/bin/cardano-smash-server

          exec $SMASH $@

        ${clusterStatements}
        else
          echo "Managed configuration for network "$NETWORK" does not exist"
          exec $@
        fi
      '';

  # Scripts supporting entrypoint
  genPgpass = writeScript "gen-pgpass" ''
    #!${runtimeShell}
    mkdir -p /configuration
    if [ -f /configuration/pgpass ]; then
       # No need to generate pgpass
      exit 0
    fi

    SECRET_DIR=/run/secrets
    echo $SECRET_DIR
    echo "Generating PGPASS file"
    POSTGRES_DB=''${POSTGRES_DB:-$(< ''${SECRET_DIR}/postgres_db)}
    POSTGRES_USER=''${POSTGRES_USER:-$(< ''${SECRET_DIR}/postgres_user)}
    POSTGRES_PASSWORD=''${POSTGRES_PASSWORD:-$(< ''${SECRET_DIR}/postgres_password)}
    echo "''${POSTGRES_HOST}:''${POSTGRES_PORT}:''${POSTGRES_DB}:''${POSTGRES_USER}:''${POSTGRES_PASSWORD}" > /configuration/pgpass
    chmod 0600 /configuration/pgpass
  '';

  # set up /tmp (override with TMPDIR variable)
  setupTmp = "mkdir -p -m 1777 /tmp";

in {
  cardano-db-sync-docker = dockerTools.buildImage {
    name = "cardano-db-sync";
    fromImage = dbSyncBaseImage;
    tag = "latest";
    copyToRoot = buildEnv {
      name = "cardano-db-sync-entrypoint";
      paths = [ dbSyncEntrypoint bashInteractive ];
    };
    config = { Entrypoint = [ "${dbSyncEntrypoint}/bin/entrypoint" ]; };
  };

  cardano-smash-server-docker = dockerTools.buildImage {
    name = "cardano-smash-server";
    fromImage = smashBaseImage;
    tag = "latest";
    copyToRoot = buildEnv {
      name = "cardano-smash-server-entrypoint";
      paths = [ smashEntrypoint bashInteractive ];
    };
    config = { Entrypoint = [ "${smashEntrypoint}/bin/entrypoint" ]; };
  };
}
