{ lib, pkgs, config, ... }:

let
  self = import ../.. { };
  cfg = config.services.cardano-exporter;
  pgpass = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${toString cfg.postgres.port}:${cfg.postgres.database}:${cfg.postgres.user}:*";
in {
  options = {
    services.cardano-exporter = {
      enable = lib.mkEnableOption "enable the cardano-explorer exporter";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      genesisHash = lib.mkOption {
        type = lib.types.str;
      };
      genesisFile = lib.mkOption {
        type = lib.types.path;
      };
      cluster = lib.mkOption {
        type = lib.types.str;
        description = "cluster name, inserted into the names of derivations to aid in debug";
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = null;
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
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
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users = {
        users.cexplorer = {
          createHome = true;
          home = "/var/lib/cexplorer";
        };
        groups.cexplorer = {};
      };
      services.postgresql = {
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
      systemd.services.cardano-explorer-node = {
        serviceConfig = {
          ExecStart = config.services.cardano-exporter.script;
          User = "cexplorer";
          WorkingDirectory = "/var/lib/cexplorer";
        };
        wantedBy = [ "multi-user.target" ];
      };
      services.cardano-exporter = {
        script = pkgs.writeScript "cardano-exporter-${cfg.cluster}" ''
          #!${pkgs.stdenv.shell}

          ${if (cfg.socketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
          then
            echo "You must set \$CARDANO_NODE_SOCKET_PATH"
            exit 1
          fi'' else "export CARDANO_NODE_SOCKET_PATH=${cfg.socketPath}"}

          export PATH=${lib.makeBinPath [ self.cardano-explorer-node self.haskellPackages.cardano-explorer-db.components.exes.cardano-explorer-db-tool pkgs.postgresql ]}:$PATH

          cp ${pgpass} ./pgpass
          chmod 0600 ./pgpass
          export PGPASSFILE=$(pwd)/pgpass
          echo $PGPASSFILE

          mkdir -p log-dir

          exec cardano-explorer-node --log-config ${../../log-configuration.yaml} \
            --genesis-hash ${cfg.genesisHash} \
            --genesis-file ${cfg.genesisFile} \
            --socket-path $CARDANO_NODE_SOCKET_PATH \
            --schema-dir ${../../schema}
        '';
      };
    })
    (lib.mkIf (cfg.environment != null) {
      services.cardano-exporter = {
        inherit (cfg.environment) genesisFile genesisHash;
      };
    })
  ];
}
