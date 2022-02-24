{ pkgs, ... }:
with pkgs; with commonLib;
{
  name = "smash-test";
  nodes = {
    machine = { config, ... }: {
      nixpkgs.pkgs = pkgs;
      environment = {
        systemPackages = with pkgs; [ curl jq ];
        variables = {
          SMASHPGPASSFILE = config.services.smash.postgres.pgpass;
        };
      };
      imports = [
        ../.
        (cardanoDbSyncProject.pkg-set.config.packages.cardano-node.src + "/nix/nixos")
      ];
      services.cardano-db-sync = {
        enable = true;
        cluster = "mainnet";
        dbSyncPkgs = pkgs;
        socketPath = config.services.cardano-node.socketPath;
      };
      services.smash = {
        enable = true;
        environmentName = "mainnet";
        dbSyncPkgs = pkgs;
        postgres = { inherit (config.services.cardano-db-sync.postgres) port database socketdir; };
      };
      services.cardano-node = {
        enable = true;
        environment = "mainnet";
        package = config.services.cardano-db-sync.dbSyncPkgs.cardanoDbSyncProject.hsPkgs.cardano-node.components.exes.cardano-node;
	      topology = cardanoLib.mkEdgeTopology {
          port = 3001;
          edgeNodes = [ "127.0.0.1" ];
        };
      };
      systemd.services.cardano-node.serviceConfig.Restart = lib.mkForce "no";
      systemd.services.cardano-db-sync.serviceConfig.SupplementaryGroups = "cardano-node";
      services.postgresql = {
        enable = true;
        package = postgresql_12;
        enableTCPIP = false;
        ensureDatabases = [ "${config.services.cardano-db-sync.postgres.database}" ];
        initialScript = builtins.toFile "enable-pgcrypto.sql" ''
          \connect template1
          CREATE EXTENSION IF NOT EXISTS pgcrypto SCHEMA pg_catalog;
        '';
        ensureUsers = [
          {
            name = "${config.services.cardano-db-sync.postgres.user}";
            ensurePermissions = {
              "DATABASE ${config.services.cardano-db-sync.postgres.database}" = "ALL PRIVILEGES";
              "ALL TABLES IN SCHEMA information_schema" = "SELECT";
              "ALL TABLES IN SCHEMA pg_catalog" = "SELECT";
            };
          }
          {
            name = "${config.services.smash.postgres.user}";
            ensurePermissions = {
              "DATABASE ${config.services.smash.postgres.database}" = "ALL PRIVILEGES";
              "ALL TABLES IN SCHEMA information_schema" = "SELECT";
              "ALL TABLES IN SCHEMA pg_catalog" = "SELECT";
            };
          }
        ];
        identMap = ''
          users root ${config.services.cardano-db-sync.postgres.user}
          users cardano-db-sync ${config.services.cardano-db-sync.postgres.user}
          users smash ${config.services.smash.postgres.user}
          users postgres postgres
        '';
        authentication = ''
          local all all ident map=users
        '';
      };
    };
  };
  testScript = ''
    start_all()
    machine.wait_for_unit("postgresql.service")
    machine.wait_for_unit("cardano-node.service")
    machine.wait_for_open_port(3001)
    machine.wait_for_unit("cardano-db-sync.service")
    machine.wait_for_unit("smash.service")
    machine.wait_for_open_port(3100)
  '';

}
