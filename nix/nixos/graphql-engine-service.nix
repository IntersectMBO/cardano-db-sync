{ lib, pkgs, config, ... }:
let
  cfg = config.services.graphql-engine;
  sources = import ../../nix/sources.nix;

in {
  options = {
    services.graphql-engine = {
      enable = lib.mkEnableOption "graphql engine service";

      host = lib.mkOption {
        type = lib.types.str;
        default = "/var/run/postgresql";
      };

      dbUser = lib.mkOption {
        type = lib.types.str;
        default = "cexplorer";
      };

      password = lib.mkOption {
        type = lib.types.str;
        default = ''""'';
      };

      dbAdminUser = lib.mkOption {
        type = lib.types.str;
        default = "postgres";
      };

      db = lib.mkOption {
        type = lib.types.str;
        default = "cexplorer";
      };

      dbPort = lib.mkOption {
        type = lib.types.int;
        default = 5432;
      };

      enginePort = lib.mkOption {
        type = lib.types.int;
        default = 9999;
      };
    };
  };
  config = let
    nixpkgsHasuraBaseSrc = sources.nixpkgs-hasura-base;
    nixpkgsHasuraAttr = import nixpkgsHasuraBaseSrc {};
    graphqlEngineAttr = nixpkgsHasuraAttr.pkgs.callPackage ./graphql-engine/default.nix {};
    graphqlEngine = graphqlEngineAttr.graphql-engine;
    hasuraDbPerms = pkgs.writeScript "hasuraDbPerms.sql" ''
      CREATE EXTENSION IF NOT EXISTS pgcrypto;
      CREATE SCHEMA IF NOT EXISTS hdb_catalog;
      CREATE SCHEMA IF NOT EXISTS hdb_views;
      ALTER SCHEMA hdb_catalog OWNER TO ${cfg.dbUser};
      ALTER SCHEMA hdb_views OWNER TO ${cfg.dbUser};
      GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO ${cfg.dbUser};
      GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO ${cfg.dbUser};
    '';
    postgresqlIp = if ((__head (pkgs.lib.stringToCharacters cfg.host)) == "/")
                   then "127.0.0.1"
                   else cfg.host;
  in lib.mkIf cfg.enable {
    systemd.services.graphql-engine = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "postgresql.service" ];
      path = with pkgs; [ curl netcat postgresql sudo ];
      preStart = ''
        for x in {1..10}; do
          nc -z ${postgresqlIp} ${toString cfg.dbPort} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
        sudo -u ${cfg.dbAdminUser} -- psql ${cfg.db} < ${hasuraDbPerms}
      '';
      script = ''
        ${graphqlEngine}/bin/graphql-engine \
          --host ${cfg.host} \
          -u ${cfg.dbUser} \
          --password ${cfg.password} \
          -d ${cfg.db} \
          --port ${toString cfg.dbPort} \
          serve \
          --server-port ${toString cfg.enginePort}
      '';
    };
  };
}
