{ lib, pkgs, config, ... }:

let
  self = import ./. { };
  cfg = config.services.cardano-exporter;
  socketdir = "/var/run/postgresql";
  port = 5432;
  user = "cexplorer";
  pgpass = builtins.toFile "pgpass" "${socketdir}:${toString port}:${user}:*:*";
in {
  options = {
    services.cardano-exporter = {
      enable = lib.mkEnableOption "enable the cardano-explorer exporter";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      genesisFile = lib.mkOption {
        type = lib.types.path;
      };
      genesisHash = lib.mkOption {
        type = lib.types.str;
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
        type = lib.types.path;
      };
    };
  };
  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      users.users.cexplorer = {
        createHome = true;
        home = "/var/lib/cexplorer";
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
        initialScript = pkgs.writeText "explorerPythonAPI-initScript" ''
          create role cexplorer with createdb login password ''';
          alter user cexplorer with superuser;
          create database cexplorer with owner cexplorer;
          \connect cexplorer
          ALTER SCHEMA public   OWNER TO cexplorer;
        '';
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

          export PATH=${lib.makeBinPath [ self.cardano-explorer-node self.haskellPackages.cardano-explorer-db.components.exes.cardano-explorer-db-manage pkgs.postgresql ]}:$PATH

          export PGPASSFILE=${pgpass}

          mkdir log-dir

          cardano-explorer-db-manage run-migrations --mdir ${./schema} --ldir log-dir

          exec cardano-explorer-node --log-config ${./log-configuration.yaml} \
            --genesis-file ${cfg.genesisFile} \
            --genesis-hash ${cfg.genesisHash} \
            --socket-path ${cfg.socketPath}
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
