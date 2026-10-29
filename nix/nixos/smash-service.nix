{ config, lib, pkgs, ... }:

let
  inherit (lib) types;
  cfg = config.services.smash;
  inherit (cfg.dbSyncPkgs) cardanoLib;
  smashConfig = cfg.explorerConfig // {
    inherit (cfg.nodeConfig) ByronGenesisFile ShelleyGenesisFile ByronGenesisHash ShelleyGenesisHash Protocol RequiresNetworkMagic;
  };
  configFile = __toFile "config.json" (__toJSON (smashConfig // cfg.logConfig));
  startupPython = pkgs.python3.withPackages (ps: [ ps.bech32 ]);
  delistedPoolsFile = pkgs.writeText "smash-delisted-pools.json" (builtins.toJSON cfg.delistedPools);
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
          modules = [{packages.cardano-smash-server.flags.disable-basic-auth = false;}];
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
        description = ''
          Pool IDs to delist at startup (hex or bech32). Existing delists are
          preserved. Requires admins; the first nonempty entry is used for
          authenticated requests to the local server.
        '';
      };
      admins = lib.mkOption {
        type = types.nullOr (types.either types.str types.path);
        default = null;
        description = ''
          File of <username>,<password> (one per line) used to protect
          administrative endpoints with basic auth. The file is loaded as a
          systemd credential, so it may be readable only by root. For secrets,
          use a quoted runtime path rather than a Nix path literal or store
          file. For example, use sops-nix to decrypt a SOPS-encrypted file to
          /run/secrets/smash-admins and set this option to
          config.sops.secrets.smash-admins.path (see doc/smash.md).
          Changes take effect after restarting the service. With null,
          public reads remain available but no administrator can authenticate.
        '';
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
    assertions = [{
      assertion = cfg.delistedPools == [] || cfg.admins != null;
      message = "services.smash.admins is required when services.smash.delistedPools is nonempty.";
    }];
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
        ''--admins "$CREDENTIALS_DIRECTORY/admins"''}
    '';
    systemd.services.smash = {
      path = [ pkgs.netcat ];
      preStart = ''
        for x in {1..60}; do
          nc -z localhost ${toString cfg.postgres.port} && break
          echo loop $x: waiting for postgresql 2 sec...
          sleep 2
        done
        sleep 1
      '';
      postStart = ''
        exec ${startupPython}/bin/python ${./smash-startup.py} \
          --port ${toString cfg.port} --pools ${delistedPoolsFile} \
          ${lib.optionalString (cfg.admins != null) ''--admins "$CREDENTIALS_DIRECTORY/admins"''}
      '';
      serviceConfig = {
        ExecStart = config.services.smash.script;
        DynamicUser = true;
        RuntimeDirectory = "smash";
        StateDirectory = "smash";
        LoadCredential = lib.optional (cfg.admins != null) "admins:${toString cfg.admins}";
      };

      wantedBy = [ "multi-user.target" ];
      after = [ "postgresql.service" "cardano-db-sync.service" ];
      requires = [ "postgresql.service" ];
    };
  };
}
