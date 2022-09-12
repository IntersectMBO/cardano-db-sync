{ config, lib, pkgs, ... }:

let
  inherit (lib) types;
  cfg = config.services.smash;
  inherit (cfg.dbSyncPkgs) cardanoLib;
  smashConfig = cfg.explorerConfig // {
    inherit (cfg.nodeConfig) ByronGenesisFile ShelleyGenesisFile ByronGenesisHash ShelleyGenesisHash Protocol RequiresNetworkMagic;
  };
  configFile = __toFile "config.json" (__toJSON (smashConfig // cfg.logConfig));
in {

  options = {
    services.smash = {
      enable = lib.mkEnableOption "enable the smash server";
      script = lib.mkOption {
        internal = true;
        type = types.package;
      };
      dbSyncPkgs = lib.mkOption {
        type = types.attrs;
        default = import ../.. {};
        defaultText = "db-sync pkgs";
        description = ''
          The db-sync packages and library that should be used.
        '';
        internal = true;
      };
      package = lib.mkOption {
        type = types.package;
        default = (cfg.dbSyncPkgs.cardanoDbSyncProject.appendModule {
          modules = [{packages.cardano-smash-server.flags.disable-basic-auth = cfg.admins == null;}];
        }).hsPkgs.cardano-smash-server.components.exes.cardano-smash-server;
      };
      port = lib.mkOption {
        type = types.int;
        default = 3100;
        description = "http serving port";
      };
      explorerConfig = lib.mkOption {
        type = types.attrs;
        default = cfg.environment.dbSyncConfig;
      };
      nodeConfig = lib.mkOption {
        type = types.attrs;
        default = cfg.environment.nodeConfig;
      };
      environment = lib.mkOption {
        type = types.nullOr types.attrs;
        default = cardanoLib.environments.${cfg.environmentName};
      };
      logConfig = lib.mkOption {
        type = types.attrs;
        default = {};
      };
      environmentName = lib.mkOption {
        type = types.str;
        description = "environment name";
      };
      socketPath = lib.mkOption {
        type = types.nullOr types.path;
        default = null;
      };
      delistedPools = lib.mkOption {
        type = types.listOf types.str;
        default = [];
        description = "List of pool id that should be delisted";
      };
      admins = lib.mkOption {
        type = types.nullOr (types.either types.str types.path);
        default = null;
        description = ''File of <username>,<password> (one per line)
          used to protect secure endpoints (pool delisting and enlisting, ticker reserving, policies fetching)
          with basic auth'';
      };
      postgres = {
        generatePGPASS = lib.mkOption {
          type = types.bool;
          default = true;
          description = "generate pgpass";
        };
        pgpass = lib.mkOption {
          type = types.path;
          default = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${toString cfg.postgres.port}:${cfg.postgres.database}:${cfg.postgres.user}:*";
        };
        socketdir = lib.mkOption {
          type = types.str;
          default = "/run/postgresql";
          description = "the path to the postgresql socket";
        };
        port = lib.mkOption {
          type = types.int;
          default = 5432;
          description = "the postgresql port";
        };
        database = lib.mkOption {
          type = types.str;
          default = "cdbsync";
          description = "the postgresql database to use";
        };
        user = lib.mkOption {
          type = types.str;
          default = "cexplorer";
          description = "the postgresql user to use";
        };
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.smash.script = let
    in pkgs.writeShellScript "smash" ''
      set -euo pipefail

      RUNTIME_DIRECTORY=''${RUNTIME_DIRECTORY:-$(pwd)}

      ${lib.optionalString cfg.postgres.generatePGPASS ''
      cp ${cfg.postgres.pgpass} /$RUNTIME_DIRECTORY/pgpass
      chmod 0600 $RUNTIME_DIRECTORY/pgpass
      export PGPASSFILE=/$RUNTIME_DIRECTORY/pgpass
      ''}

      exec ${cfg.package}/bin/cardano-smash-server \
        --port ${toString cfg.port} \
        --config ${configFile} \
        ${lib.optionalString (cfg.admins != null)
        "--admins ${cfg.admins}"}
    '';
    systemd.services.smash = {
      path = [ pkgs.netcat pkgs.curl ];
      preStart = ''
        for x in {1..60}; do
          nc -z localhost ${toString cfg.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
        sleep 1
      '';
      postStart = ''
        for x in {1..10}; do
          nc -z localhost ${toString cfg.port} && break
          echo loop $x: waiting for smash 2 sec...
          sleep 2
        done
        sleep 1
        for p in ${toString cfg.delistedPools}; do
          echo "Unlisting pool $p."
          curl --silent --header "Content-Type: application/json" --request PATCH \
          --data "{ \"poolId\": \"$p\" }" \
          "http://localhost:${toString cfg.port}/api/v1/delist"
        done
      '';
      serviceConfig = {
        ExecStart = config.services.smash.script;
        DynamicUser = true;
        RuntimeDirectory = "smash";
        StateDirectory = "smash";
      };

      wantedBy = [ "multi-user.target" ];
      after = [ "postgresql.service" "cardano-db-sync.service" ];
      requires = [ "postgresql.service" ];
    };
  };
}
