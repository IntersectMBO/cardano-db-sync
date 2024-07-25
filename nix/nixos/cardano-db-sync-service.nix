{ config, lib, pkgs, ... }:

let
  cfg = config.services.cardano-db-sync;
  self = cfg.dbSyncPkgs;
  envConfig = cfg.environment;
  configFile = __toFile "db-sync-config.json"
    (__toJSON (cfg.explorerConfig // cfg.logConfig));
  stateDirBase = "/var/lib/";
  schemaVersion = lib.versions.majorMinor cfg.package.version;
in {
  options = {
    services.cardano-db-sync = {
      enable = lib.mkEnableOption "enable the cardano-db-sync service";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      profiling = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Enable GHC profiling.
        '';
      };
      rtsArgs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default =
          if cfg.profiling then
            [ "-p" "-hc" "-L200"]
          else
            [ ];
        apply = args:
          if (args != []) then
            ["+RTS"] ++ args ++ ["-RTS"]
          else
            [];
        description = ''Extra CLI args, to be surrounded by "+RTS"/"-RTS"'';
      };
      takeSnapshot = lib.mkOption {
        type = lib.types.enum [ "never" "once" "always" ];
        default = "never";
        description = ''
          Take snapshot before starting cardano-db-sync,
                    "once" (skip if there is one already),
                    "always" (removing previous snapshot),
                    or "never".
                  '';
      };
      restoreSnapshot = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Restore a snapshot before starting cardano-db-sync,
          if the snasphot file given by the option exist.
          Snapshot file is deleted after restore.
        '';
      };
      restoreSnapshotSha = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          SHA256 checksum of the snapshot to restore
        '';
      };
      restoreSnapshotSigKey = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = ''
          Key ID for verifying the snaspshot signature.
          (Signature check disabled if null)
        '';
      };
      restoreSnapshotScript = lib.mkOption {
        type = lib.types.str;
        internal = true;
        default = lib.optionalString (cfg.restoreSnapshot != null) ''
          ${lib.optionalString (lib.hasPrefix "$" cfg.restoreSnapshot) ''
            if [ ! -z "''${${
              lib.removePrefix "$" cfg.restoreSnapshot
            }:-}" ]; then''}
          SNAPSHOT_BASENAME="$(basename "${cfg.restoreSnapshot}")"
          RESTORED_MARKER="$SNAPSHOT_BASENAME.restored"
          if [ ! -f "$RESTORED_MARKER" ]; then
            if [[ "${cfg.restoreSnapshot}" =~ ^https://.* ]]; then
              echo "Downloading snapshot ${cfg.restoreSnapshot} ..."
              ${pkgs.curl}/bin/curl -LOC - "${cfg.restoreSnapshot}"

              ${
                if (cfg.restoreSnapshotSha != null) then
                  ''
                    echo "${cfg.restoreSnapshotSha} $SNAPSHOT_BASENAME" > "$SNAPSHOT_BASENAME.sha256sum"''
                else
                  ''
                    ${pkgs.curl}/bin/curl -LO "${cfg.restoreSnapshot}.sha256sum"''
              }
              ${pkgs.coreutils}/bin/sha256sum -c "$SNAPSHOT_BASENAME.sha256sum"

              ${
                lib.optionalString (cfg.restoreSnapshotSigKey != null) ''
                  ${pkgs.curl}/bin/curl -LO "${cfg.restoreSnapshot}.asc"
                  ${pkgs.gnupg}/bin/gpg --keyserver keys.openpgp.org --recv-keys ${cfg.restoreSnapshotSigKey}
                  ${pkgs.gnupg}/bin/gpg --verify "$SNAPSHOT_BASENAME.asc" "$SNAPSHOT_BASENAME"
                ''
              }

              SNAPSHOT="$SNAPSHOT_BASENAME"
            else
              SNAPSHOT="${cfg.restoreSnapshot}"
            fi
            rm -f ${cfg.stateDir}/*.lstate
            ${
              ../../scripts/postgresql-setup.sh
            } --restore-snapshot "$SNAPSHOT" ${cfg.stateDir}
            touch $RESTORED_MARKER
            rm -f $SNAPSHOT{,.sha256sum,.asc}
          fi
          ${lib.optionalString (lib.hasPrefix "$" cfg.restoreSnapshot) "fi"}
        '';
      };
      # FIXME: is my assumption that this is required correct?
      pgpass = lib.mkOption {
        internal = true;
        type = lib.types.path;
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = self.cardanoLib.environments.${cfg.cluster};
      };
      cluster = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "cluster name";
      };
      explorerConfig = lib.mkOption {
        type = lib.types.attrs;
        default = cfg.environment.dbSyncConfig;
      };
      logConfig = lib.mkOption {
        type = lib.types.attrs;
        default = {};
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };
      stateDir = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = "/var/lib/cexplorer";
      };
      disableLedger = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = ''
          Disables the leger state. Drastically reduces memory usage
          and it syncs faster, but some data are missing.
        '';
      };
      dbSyncPkgs = lib.mkOption {
        type = lib.types.attrs;
        default = import ../. { };
        defaultText = "cardano-db-sync pkgs";
        description = ''
          The cardano-db-sync packages and library that should be used.
          Main usage is sharing optimization:
          reduce eval time when service is instantiated multiple times.
        '';
      };
      package = lib.mkOption {
        type = lib.types.package;
        default =
          if cfg.profiling then
            self.cardano-db-sync-profiled
          else
            self.cardano-db-sync;
      };
      postgres = {
        generatePGPASS = lib.mkOption {
          type = lib.types.bool;
          default = true;
          description = "generate pgpass";
        };
        socketdir = lib.mkOption {
          type = lib.types.str;
          default = "/run/postgresql";
          description = "the path to the postgresql socket";
        };
        port = lib.mkOption {
          type = lib.types.int;
          # FIXME: set the correct port
          default = 5432;
          description = "the postgresql port";
        };
        database = lib.mkOption {
          type = lib.types.str;
          default = "cdbsync";
          description = "the postgresql database to use";
        };
        user = lib.mkOption {
          type = lib.types.str;
          default = "cexplorer";
          description = "the postgresql user to use";
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.cardano-db-sync =
      let exec = "${cfg.package}/bin/${cfg.package.exeName}";
          cardano-cli-check = "cardano-cli query tip " +
            (if (cfg.environment.nodeConfig.RequiresNetworkMagic == "RequiresNoMagic" )
              then "--mainnet"
              else "--testnet-magic $(jq '.networkMagic' ${cfg.environment.nodeConfig.ShelleyGenesisFile})");
          dbSyncCommand = ''
            if [[ "''${DISABLE_LEDGER:-N}" == "Y" ]]; then
              LEDGER_OPTS="--disable-ledger"
            else
              LEDGER_OPTS="--state-dir ${cfg.stateDir}"
            fi

            if [[ -z "''${DB_SYNC_CONFIG:-}" ]]; then
              exec ${exec} \
                  --config ${configFile} \
                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                  --schema-dir ${self.schema} \
                  ${toString cfg.rtsArgs} \
                  ''${LEDGER_OPTS} \
                  ''${EXTRA_DB_SYNC_ARGS:-}
            else
              exec ${exec} \
                  --config "$DB_SYNC_CONFIG" \
                  --socket-path "$CARDANO_NODE_SOCKET_PATH" \
                  --schema-dir ${self.schema} \
                  ${toString cfg.rtsArgs} \
                  ''${LEDGER_OPTS} \
                  ''${EXTRA_DB_SYNC_ARGS:-}
            fi
          '';
    in {
      pgpass = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${
          toString cfg.postgres.port
        }:${cfg.postgres.database}:${cfg.postgres.user}:*";
      script = pkgs.writeShellScript "cardano-db-sync" ''
        set -euo pipefail

        ${if (cfg.socketPath == null) then ''
          if [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ]
                  then
                    echo "You must set \$CARDANO_NODE_SOCKET_PATH"
                    exit 1
                  fi'' else
          ''export CARDANO_NODE_SOCKET_PATH="${cfg.socketPath}"''}

        ${lib.optionalString cfg.postgres.generatePGPASS ''
          cp ${cfg.pgpass} ./pgpass
          chmod 0600 ./pgpass
          export PGPASSFILE=$(pwd)/pgpass
        ''}

        ${if (cfg.disableLedger) then
            "export DISABLE_LEDGER=Y"
          else ""}

        ${cfg.restoreSnapshotScript}

        if [[ -n "''${WAIT_FOR_NODE_SYNC:-}" ]]
        then
          until [ -S $CARDANO_NODE_SOCKET_PATH ]; do
            echo Waiting for $CARDANO_NODE_SOCKET_PATH
            sleep 10
          done
          # from scripts/postgresql-setup.sh
          export PGHOST=$(cut -d ":" -f 1 "''${PGPASSFILE}")
          export PGPORT=$(cut -d ":" -f 2 "''${PGPASSFILE}")
          export PGDATABASE=$(cut -d ":" -f 3 "''${PGPASSFILE}")
          user=$(cut -d ":" -f 4 "''${PGPASSFILE}")
          if [ "$user" != "*" ]; then
            export PGUSER=$user
          fi;
          DB_MAX_BLOCK=$(psql -h $PGHOST $PGDATABASE -U $PGUSER -t -c 'select max (block_no) from block;')
          NODE_CUR_BLOCK=0
          while [ $NODE_CUR_BLOCK -lt $DB_MAX_BLOCK ]; do
            NODE_STATUS="$(${cardano-cli-check} 2>/dev/null || true)"
            NODE_CUR_BLOCK="$(jq -e -r '.block' <<<"$NODE_STATUS" 2>/dev/null || true)"
            echo "Waiting... Sync progress at $NODE_CUR_BLOCK /$DB_MAX_BLOCK"
            sleep 10
          done
        fi

        mkdir -p log-dir
        ${dbSyncCommand}'';
    };
    systemd.services.cardano-db-sync = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      path = with pkgs; [
        self.cardanoDbSyncHaskellPackages.cardano-db-tool.components.exes.cardano-db-tool
        config.services.postgresql.package
        netcat
        bash
        gnutar
        gzip
      ];
      preStart = ''
        for x in {1..60}; do
          nc -z localhost ${toString cfg.postgres.port} && sleep 2 && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
      '';
      serviceConfig = {
        ExecStart = cfg.script;
        DynamicUser = true;
        WorkingDirectory = cfg.stateDir;
        StateDirectory = lib.removePrefix stateDirBase cfg.stateDir;
        TimeoutStopSec = "2h";
        SupplementaryGroups = "cardano-node";
      };
      postStop = lib.optionalString (cfg.takeSnapshot != "never") ''
        # Only take snapshot after service exited cleanly.
        if [ "$SERVICE_RESULT" = "success" ]; then
          EXISTING_SNAPSHOTS=()
          for f in db-sync-snapshot-schema-${schemaVersion}*.tgz; do
            if [ -f $f ]; then
              echo "A snapshot already exist for this version: $f"
              EXISTING_SNAPSHOTS+=( $f )
            fi
          done
          ${
            lib.optionalString (cfg.takeSnapshot == "once") ''
              if [ ''${#EXISTING_SNAPSHOTS[@]} -eq 0 ]; then
            ''
          }
            export PGPASSFILE=$(pwd)/pgpass
            set +e
            SNAPSHOT_SCRIPT=$( (yes phrase ||:) | cardano-db-tool prepare-snapshot --state-dir ${cfg.stateDir} | tail -n 1)
            res=$?
            set -e
            SNAPSHOT="$(echo $SNAPSHOT_SCRIPT | cut -d " " -f3)"
            if [ $res -eq 0 ]; then
              if [ ! -f "$SNAPSHOT.tgz" ]; then
                ${
                  ../../scripts/postgresql-setup.sh
                } ''${SNAPSHOT_SCRIPT#*scripts/postgresql-setup.sh}
                for s in ''${EXISTING_SNAPSHOTS[@]}; do
                  rm $s
                done
              else
                >&2 echo "A snapshot already exist for same schema/block, skipping snapshot creation."
              fi
            else
              >&2 echo "State does not permit to take snapshot, proceeding with normal startup."
            fi
          ${lib.optionalString (cfg.takeSnapshot == "once") "fi"}
        else
          >&2 echo "Service did not exited cleanly. Skipping snapshot creation."
        fi
      '';
    };

    assertions = [{
      assertion = lib.hasPrefix stateDirBase cfg.stateDir;
      message =
        "The option services.cardano-db-sync.stateDir should have ${stateDirBase} as a prefix!";
    }];
  };
}
