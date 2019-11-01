{ lib, pkgs, config, ... }:
let
  self = import ../.. { };
  cfg = config.services.cardano-graphql;
  sources = import ../../nix/sources.nix;

  nixpkgsHasuraBaseSrc = sources.nixpkgs-hasura-base;
  nixpkgsHasuraAttr = import nixpkgsHasuraBaseSrc {};
  graphqlEngineAttr = nixpkgsHasuraAttr.pkgs.callPackage ./graphql-engine/default.nix {};
  graphqlEngine = graphqlEngineAttr.graphql-engine;

  feBaseSrc = (import /etc/nixos/secrets/fe.nix).path;
  feBaseAttr = import feBaseSrc;
  fe = feBaseAttr.cardano-graphql;

in {
  options = {
    services.cardano-graphql = {
      enable = lib.mkEnableOption "cardano-explorer graphql service";

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

      hasuraIp = lib.mkOption {
        type = lib.types.str;
        default = "127.0.0.1";
      };

      hasuraProtocol = lib.mkOption {
        type = lib.types.str;
        default = "http";
      };
    };
  };
  config = let
    hasuraBaseUri = cfg.hasuraProtocol + "://" + cfg.hasuraIp + ":" + (toString cfg.enginePort) + "/";
    hasuraDbPerms = pkgs.writeScript "hasuraDbPerms.sql" ''
      CREATE EXTENSION IF NOT EXISTS pgcrypto;
      CREATE SCHEMA IF NOT EXISTS hdb_catalog;
      CREATE SCHEMA IF NOT EXISTS hdb_views;
      ALTER SCHEMA hdb_catalog OWNER TO ${cfg.dbUser};
      ALTER SCHEMA hdb_views OWNER TO ${cfg.dbUser};
      GRANT SELECT ON ALL TABLES IN SCHEMA information_schema TO ${cfg.dbUser};
      GRANT SELECT ON ALL TABLES IN SCHEMA pg_catalog TO ${cfg.dbUser};
    '';
    hasuraDbViews = feBaseSrc + "/test/postgres/init/002_views.sql";
    hasuraDbMetadata = feBaseSrc + "/hasura/migrations/metadata.json";
    postgresqlIp = if ((__head (pkgs.lib.stringToCharacters cfg.host)) == "/")
                   then "127.0.0.1"
                   else cfg.host;
  in {
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
    systemd.services.cardano-graphql = {
      wantedBy = [ "multi-user.target" ];
      requires = [ "graphql-engine.service" ];
      environment = {
        HASURA_URI = hasuraBaseUri + "v1/graphql";
      };
      path = with pkgs; [ netcat curl postgresql nodejs-12_x ];
      preStart = ''
        for x in {1..12}; do
          [ $(curl -s -o /dev/null -w "%{http_code}" http://localhost:8100/api/blocks/pages) == "200" ] && break;
          echo loop $x: waiting for cardano exporter tables 10 sec...
          sleep 10
        done
        psql -U ${cfg.dbUser} ${cfg.db} < ${hasuraDbViews} || true

        for x in {1..10}; do
          nc -z ${cfg.hasuraIp} ${toString cfg.enginePort} && break
          echo loop $x: waiting for graphql-engine 2 sec...
          sleep 2
        done
        curl -d'{"type":"replace_metadata", "args":'$(cat ${hasuraDbMetadata})'}' ${hasuraBaseUri}v1/query
      '';
      script = ''
        node --version
        node ${fe}/index.js
      '';
    };
  };
}
