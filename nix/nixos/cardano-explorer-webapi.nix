{ config, lib, pkgs, ... }:

let
  cfg = config.services.cardano-explorer-webapi;
  self = import ../.. { };
in {
  options = {
    services.cardano-explorer-webapi = {
      enable = lib.mkEnableOption "enable the cardano-explorer web api";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      pgpass = lib.mkOption {
        type = lib.types.path;
      };
      postgres = {
        socketdir = lib.mkOption {
          type = lib.types.str;
          default = "/run/postgresql";
          description = "the path to the postgresql socket";
        };
        port = lib.mkOption {
          type = lib.types.int;
          default = 5432;
          description = "the postgresql port";
        };
        database = lib.mkOption {
          type = lib.types.str;
          default = "cexplorer";
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
    services.cardano-explorer-webapi = {
      pgpass = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${toString cfg.postgres.port}:${cfg.postgres.database}:${cfg.postgres.user}:*";
      script = pkgs.writeShellScript "cardano-explorer-webapi" ''
        export PGPASSFILE=${cfg.pgpass}
        exec ${self.cardano-explorer-webapi}/bin/cardano-explorer-webapi
      '';
    };
    systemd.services.cardano-explorer-webapi = {
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
        User = "cexplorer";
      };
    };
  };
}
