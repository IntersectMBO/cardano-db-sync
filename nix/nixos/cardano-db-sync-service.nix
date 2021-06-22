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
        type = lib.types.bool;
        default = false;
        description = "Take snapshot before starting cardano-db-sync";
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

        ${lib.optionalString cfg.takeSnapshot ''
        for f in db-sync-snapshot-schema-${majorVersion}*; do
          if [ -f $f ]; then
            echo "Skipping snapshot: already exist for this version: $f"
            SKIP_SNAPSHOT=1
          fi
        done
        if [ -z ''${SKIP_SNAPSHOT:-} ]; then
          SNAPSHOT_SCRIPT=$(cardano-db-tool prepare-snapshot --state-dir ./ | tail -n 1)
          ${../../scripts/postgresql-setup.sh} ''${SNAPSHOT_SCRIPT#*scripts/postgresql-setup.sh}
        fi
        ''}

        ${lib.optionalString (cfg.restoreSnapshot != null) ''
        if [ -f ${cfg.restoreSnapshot} ]; then
          ${../../scripts/postgresql-setup.sh} --restore-snapshot ${cfg.restoreSnapshot} ./
          rm ${cfg.restoreSnapshot}
        fi
        ''}

        mkdir -p log-dir
        exec ${exec} \
          --config ${configFile} \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --schema-dir ${../../schema} \
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
        self.cardano-db-tool
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
