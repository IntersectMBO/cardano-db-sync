{ config, lib, pkgs, ... }:

let
  cfg = config.services.cardano-db-sync;
  self = config.internal.syncPackages;
  envConfig = cfg.environment;
  explorerConfig = {
    inherit (envConfig.nodeConfig) RequiresNetworkMagic;
    GenesisHash = envConfig.genesisHash;
    NetworkName = cfg.cluster;
  } // cfg.logConfig;
  configFile = __toFile "config.json" (__toJSON explorerConfig);
in {
  options = {
    internal = lib.mkOption {
      type = lib.types.attrs;
      internal = true;
      default = { syncPackages = import ../../. {}; };
    };
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
      # FIXME: is my assumption that this is required correct?
      pgpass = lib.mkOption {
        internal = true;
        type = lib.types.path;
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = pkgs.iohkNix.cardanoLib.environments.${cfg.cluster};
      };
      cluster = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "cluster name";
      };
      logConfig = lib.mkOption {
        type = lib.types.attrs;
        default = pkgs.iohkNix.cardanoLib.defaultExplorerLogConfig;
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
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
        ${if (cfg.socketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
        then
          echo "You must set \$CARDANO_NODE_SOCKET_PATH"
          exit 1
        fi'' else "export CARDANO_NODE_SOCKET_PATH=\"${cfg.socketPath}\""}
        export PATH=${lib.makeBinPath [ pkgs.postgresql ]}:$PATH

        ${lib.optionalString cfg.postgres.generatePGPASS ''
        cp ${cfg.pgpass} ./pgpass
        chmod 0600 ./pgpass
        export PGPASSFILE=$(pwd)/pgpass
        ''}

        mkdir -p log-dir
        exec ${exec} \
          --config ${configFile} \
          --genesis-file ${envConfig.genesisFile} \
          --socket-path "$CARDANO_NODE_SOCKET_PATH" \
          --schema-dir ${../../schema}
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
      path = [ pkgs.netcat ];
      preStart = ''
        for x in {1..10}; do
          nc -z localhost ${toString cfg.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
      '';
      serviceConfig = {
        ExecStart = cfg.script;
        User = cfg.postgres.user;
        WorkingDirectory = "/var/lib/${cfg.user}";
      };
    };
  };
}
