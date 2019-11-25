# WARNING!!! THIS IS BROKEN! DO NOT USE!

{ lib, pkgs, config, ... }:
let
  cfg = config.services.cardano-graphql;
  sources = import ../../nix/sources.nix;
in {
  options = {
    services.cardano-graphql = {
      enable = lib.mkEnableOption "cardano-explorer graphql service";

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
    frontendBaseSrc = sources.cardano-graphql;
    frontend = (import ../cardano-graphql).cardano-graphql;
    hasuraBaseUri = cfg.hasuraProtocol + "://" + cfg.hasuraIp + ":" + (toString cfg.enginePort) + "/";
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
