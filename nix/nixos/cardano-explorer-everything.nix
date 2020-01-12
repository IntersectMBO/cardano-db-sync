{ config, lib, ... }:

let
  cfg = config.services.cardano-explorer;
in {
  options = {
    services.cardano-explorer = {
      enable = lib.mkEnableOption "cardano-explorer full setup";
      cluster = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "cluster name";
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services = {
      postgresql = {
        enable = true;
        enableTCPIP = false;
        extraConfig = ''
          max_connections = 200
          shared_buffers = 2GB
          effective_cache_size = 6GB
          maintenance_work_mem = 512MB
          checkpoint_completion_target = 0.7
          wal_buffers = 16MB
          default_statistics_target = 100
          random_page_cost = 1.1
          effective_io_concurrency = 200
          work_mem = 10485kB
          min_wal_size = 1GB
          max_wal_size = 2GB
        '';
        ensureDatabases = [ "cexplorer" ];
        ensureUsers = [
          {
            name = "cexplorer";
            ensurePermissions = {
              "DATABASE cexplorer" = "ALL PRIVILEGES";
            };
          }
        ];
        identMap = ''
          explorer-users root cexplorer
          explorer-users cexplorer cexplorer
          explorer-users postgres postgres
        '';
        authentication = ''
          local all all ident map=explorer-users
        '';
      };
      cardano-exporter = {
        enable = true;
        cluster = cfg.cluster;
        socketPath = cfg.socketPath;
      };
      cardano-explorer-webapi = {
        enable = true;
      };
      cardano-tx-submit-webapi = {
        enable = true;
        cluster = cfg.cluster;
        socketPath = cfg.socketPath;
      };
    };
  };
}
