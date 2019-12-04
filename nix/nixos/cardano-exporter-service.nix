{ lib, pkgs, config, ... }:

let
  self = import ../.. { };
  localLib = import ../../lib.nix {};
  cfg = config.services.cardano-exporter;
  pgpass = builtins.toFile "pgpass" "${cfg.postgres.socketdir}:${toString cfg.postgres.port}:${cfg.postgres.database}:${cfg.postgres.user}:*";
  envConfig = cfg.environment;
  explorerConfig = {
    inherit (envConfig.nodeConfig) RequiresNetworkMagic GenesisHash;
    NetworkName = cfg.cluster;
  } // cfg.logConfig;
  configFile = __toFile "config.json" (__toJSON explorerConfig);
in {
  options = {
    services.cardano-exporter = {
      enable = lib.mkEnableOption "cardano-explorer exporter";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      cluster = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "cluster name";
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = localLib.cardanoLib.environments.${cfg.cluster};
      };
      logConfig = lib.mkOption {
        type = lib.types.attrs;
        default = localLib.cardanoLib.defaultExplorerLogConfig;
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
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
    services.cardano-explorer-webapi = {
      enable = lib.mkEnableOption "enable the cardano-explorer web api";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users = {
        users.cexplorer = {
          createHome = true;
          home = "/var/lib/cexplorer";
          group = "cexplorer";
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
        after = [ "postgresql.service" ];
      };
      services.cardano-exporter = {
        inherit pgpass;
        script = pkgs.writeScript "cardano-exporter-${explorerConfig.NetworkName}" ''
          #!${pkgs.stdenv.shell}

          ${if (cfg.socketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
          then
            echo "You must set \$CARDANO_NODE_SOCKET_PATH"
            exit 1
          fi'' else "export CARDANO_NODE_SOCKET_PATH=${cfg.socketPath}"}

          export PATH=${lib.makeBinPath [ self.cardano-explorer-node self.haskellPackages.cardano-explorer-db.components.exes.cardano-explorer-db-tool pkgs.postgresql ]}:$PATH

          cp ${cfg.pgpass} ./pgpass
          chmod 0600 ./pgpass
          export PGPASSFILE=$(pwd)/pgpass
          echo $PGPASSFILE

          mkdir -p log-dir

          exec cardano-explorer-node \
            --config ${configFile} \
            --genesis-file ${envConfig.genesisFile} \
            --socket-path $CARDANO_NODE_SOCKET_PATH \
            --schema-dir ${../../schema}
        '';
      };
    })
    (lib.mkIf config.services.cardano-explorer-webapi.enable {
      services.cardano-explorer-webapi.script = pkgs.writeShellScript "cardano-explorer-webapi" ''
        export PGPASSFILE=${config.services.cardano-exporter.pgpass}
        ${self.cardano-explorer-webapi}/bin/cardano-explorer-webapi
      '';
      systemd.services.cardano-explorer-webapi = {
        wantedBy = [ "multi-user.target" ];
        requires = [ "postgresql.service" ];
        preStart = ''
          for x in {1..10}; do
            nc -z localhost ${toString cfg.postgres.port} && break
            echo loop $x: waiting for postgresql 2 sec...
            sleep 2
          done
        '';
        serviceConfig = {
          ExecStart = config.services.cardano-explorer-webapi.script;
          User = "cexplorer";
        };
      };
    })
  ];
}
