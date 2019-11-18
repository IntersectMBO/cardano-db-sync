{ lib, pkgs, config, ... }:
let
  self = import ../.. { };
  cfg = config.services.cardano-graphql;
  sources = import ../../nix/sources.nix;
in {
  options = {
    services.cardano-graphql = {
      enable = lib.mkEnableOption "cardano-explorer graphql service";

      # To do: remove this option once the repo is public
      localRepoPath = lib.mkOption {
        type = lib.types.str;
        default = "/var/lib/repo/cardano-graphql";
        description = ''
          The local repo path to the cardano-graphql private repository.
          The branch checked out should be `nix-build`.
        '';
      };

      dbUser = lib.mkOption {
        type = lib.types.str;
        default = "cexplorer";
      };

      db = lib.mkOption {
        type = lib.types.str;
        default = "cexplorer";
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
    frontendBaseSrc = cfg.localRepoPath;
    frontendBaseAttr = import frontendBaseSrc;
    frontend = frontendBaseAttr.cardano-graphql;
    hasuraBaseUri = cfg.hasuraProtocol + "://" + cfg.hasuraIp + ":" + (toString cfg.enginePort) + "/";
    hasuraDbViews = frontendBaseSrc + "/test/postgres/init/002_views.sql";
    hasuraDbMetadata = frontendBaseSrc + "/hasura/migrations/metadata.json";
  in lib.mkIf cfg.enable {
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
        node ${frontend}/index.js
      '';
    };
  };
}
