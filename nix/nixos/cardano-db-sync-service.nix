{ config, lib, pkgs, ... }:

let
  cfg = config.services.cardano-db-sync;
  self = cfg.dbSyncPkgs;
  envConfig = cfg.environment;
  configFile = __toFile "db-sync-config.json" (__toJSON (cfg.explorerConfig // cfg.logConfig));
  stateDirBase = "/var/lib/";
  majorVersion = lib.versions.major cfg.package.version;
in {
  options = {
    services.cardano-db-sync = {
      enable = lib.mkEnableOption "enable the cardano-db-sync service";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      extended = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "use cardano-db-sync-extended";
      };
      takeSnapshot = lib.mkOption {
        type = lib.types.enum [ "never" "once" "always" ];
        default = "never";
        description = ''Take snapshot before starting cardano-db-sync,
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
        ${lib.optionalString (lib.hasPrefix "$" cfg.restoreSnapshot) ''if [ ! -z "''${${lib.removePrefix "$" cfg.restoreSnapshot}:-}" ]; then''}
        SNAPSHOT_BASENAME="$(basename "${cfg.restoreSnapshot}")"
        RESTORED_MARKER="$SNAPSHOT_BASENAME.restored"
        if [ ! -f "$RESTORED_MARKER" ]; then
          if [[ "${cfg.restoreSnapshot}" =~ ^https://.* ]]; then
            echo "Downloading snapshot ${cfg.restoreSnapshot} ..."
            ${pkgs.curl}/bin/curl -LOC - "${cfg.restoreSnapshot}"

            ${if (cfg.restoreSnapshotSha != null)
            then  ''echo "${cfg.restoreSnapshotSha} $SNAPSHOT_BASENAME" > "$SNAPSHOT_BASENAME.sha256sum"''
            else  ''${pkgs.curl}/bin/curl -LO "${cfg.restoreSnapshot}.sha256sum"''
            }
            ${pkgs.coreutils}/bin/sha256sum -c "$SNAPSHOT_BASENAME.sha256sum"

            ${lib.optionalString (cfg.restoreSnapshotSigKey != null) ''
              ${pkgs.curl}/bin/curl -LO "${cfg.restoreSnapshot}.asc"
              ${pkgs.gnupg}/bin/gpg --keyserver keys.openpgp.org --recv-keys ${cfg.restoreSnapshotSigKey}
              ${pkgs.gnupg}/bin/gpg --verify "$SNAPSHOT_BASENAME.asc" "$SNAPSHOT_BASENAME"
            ''}

            SNAPSHOT="$SNAPSHOT_BASENAME"
          else
            SNAPSHOT="${cfg.restoreSnapshot}"
          fi
          rm -f *.lstate
          ${../../scripts/postgresql-setup.sh} --restore-snapshot "$SNAPSHOT" ./
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
        default = cfg.environment.explorerConfig;
      };
      logConfig = lib.mkOption {
        type = lib.types.attrs;
        default = self.cardanoLib.defaultExplorerLogConfig;
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };
      stateDir = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = "/var/lib/${cfg.user}";
      };
      dbSyncPkgs = lib.mkOption {
        type = lib.types.attrs;
        default = import ../. {};
        defaultText = "cardano-db-sync pkgs";
        description = ''
          The cardano-db-sync packages and library that should be used.
          Main usage is sharing optimization:
          reduce eval time when service is instantiated multiple times.
        '';
      };
      package = lib.mkOption {
        type = lib.types.package;
        default = if cfg.extended then self.cardano-db-sync-extended else self.cardano-db-sync;
      };
      user = lib.mkOption {
        type = lib.types.str;
        default = "cdbsync";
        description = "the user to run as";
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
          default = cfg.user;
          description = "the postgresql user to use";
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.cardano-db-sync = let
      exec = if cfg.extended then "${cfg.package}/bin/cardano-db-sync-extended" else "${cfg.package}/bin/cardano-db-sync";
    in {
      pgpass = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${toString cfg.postgres.port}:${cfg.postgres.database}:${cfg.postgres.user}:*";
      script = pkgs.writeShellScript "cardano-db-sync" ''
        set -euo pipefail

        ${if (cfg.socketPath == null) then ''if [ -z "''${CARDANO_NODE_SOCKET_PATH:-}" ]
        then
          echo "You must set \$CARDANO_NODE_SOCKET_PATH"
          exit 1
        fi'' else "export CARDANO_NODE_SOCKET_PATH=\"${cfg.socketPath}\""}

        ${lib.optionalString cfg.postgres.generatePGPASS ''
        cp ${cfg.pgpass} ./pgpass
        chmod 0600 ./pgpass
        export PGPASSFILE=$(pwd)/pgpass
        ''}

        ${lib.optionalString (cfg.takeSnapshot != "never")  ''
        EXISTING_SNAPSHOTS=()
        for f in db-sync-snapshot-schema-${majorVersion}*; do
          if [ -f $f ]; then
            echo "A snapshot already exist for this version: $f"
            EXISTING_SNAPSHOTS+=( $f )
          fi
        done
        ${lib.optionalString (cfg.takeSnapshot == "once") ''
        if [ ''${#EXISTING_SNAPSHOTS[@]} -eq 0 ]; then
        ''}
          set +e
          SNAPSHOT_SCRIPT=$( (yes phrase ||:) | cardano-db-tool prepare-snapshot --state-dir ./ | tail -n 1)
          res=$?
          set -e
          if [ $res -eq 0 ]; then
            ${../../scripts/postgresql-setup.sh} ''${SNAPSHOT_SCRIPT#*scripts/postgresql-setup.sh}
            for s in ''${EXISTING_SNAPSHOTS[@]}; do
              rm $s
            done
          else
            >&2 echo "State does not permit to take snapshot, proceeding with normal startup."
          fi
        ${lib.optionalString (cfg.takeSnapshot == "once") ''
        fi''}
        ''}

        ${cfg.restoreSnapshotScript}

        mkdir -p log-dir
        exec ${exec} \
          --config ${configFile} \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --schema-dir ${self.schema or (self.src + "/schema")} \
          --state-dir ${cfg.stateDir}
      '';
    };
    users = {
      users."${cfg.postgres.user}" = {
        createHome = true;
        home = "/var/lib/${cfg.user}";
        group = cfg.user;
      };
      groups.cexplorer = {};
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
        for x in {1..10}; do
          nc -z localhost ${toString cfg.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
      '';
      serviceConfig = {
        ExecStart        = cfg.script;
        User             = cfg.postgres.user;
        WorkingDirectory = cfg.stateDir;
        StateDirectory   = lib.removePrefix stateDirBase cfg.stateDir;
      };
    };

    assertions = [
      {
        assertion = lib.hasPrefix stateDirBase cfg.stateDir;
        message = "The option services.cardano-db-sync.stateDir should have ${stateDirBase} as a prefix!";
      }
    ];
  };
}
