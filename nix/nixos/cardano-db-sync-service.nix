{ config, lib, pkgs, ... }:

let
  cfg = config.services.cardano-db-sync;
  self = import ../.. {};
  envConfig = cfg.environment;
  explorerConfig = {
    inherit (envConfig.nodeConfig) RequiresNetworkMagic GenesisHash;
    NetworkName = cfg.cluster;
  } // cfg.logConfig;
  configFile = __toFile "config.json" (__toJSON explorerConfig);
in {
  options = {
    services.cardano-db-sync = {
      enable = lib.mkEnableOption "enable the cardano-db-sync service";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
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
        default = self.cardano-db-sync;
      };
      postgres = {
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
          default = "cdbsync";
          description = "the postgresql user to use";
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.cardano-db-sync = {
      pgpass = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${toString cfg.postgres.port}:${cfg.postgres.database}:${cfg.postgres.user}:*";
      script = pkgs.writeShellScript "cardano-db-sync" ''
        export PGPASSFILE=${cfg.pgpass}
        exec ${cfg.package}/bin/cardano-db-sync \
          --config ${configFile} \
          --genesis-file ${envConfig.genesisFile} \
          --socket-path ${cfg.socketPath} \
          --schema-dir ${../../schema}
      '';
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
        User = "cdbsync";
      };
    };
  };
}
