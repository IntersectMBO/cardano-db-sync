{ pkgs, lib, iohkNix, customConfig }:
let
  mkStartScripts = envConfig: let
    extraModule = {
      services.cardano-db-sync = {
        enable = true;
        postgres.user = "*";
        environment = envConfig;
        cluster = envConfig.name;
      };
      services.cardano-db-sync-extended = {
        enable = true;
        environment = envConfig;
        network = envConfig.name;
      };
    };
    systemdCompat.options = {
      systemd.services = lib.mkOption {};
      services.postgresql = lib.mkOption {};
      users = lib.mkOption {};
    };
    eval = lib.evalModules {
      prefix = [];
      modules = import nixos/module-list.nix ++ [ systemdCompat extraModule customConfig ];
      args = { inherit pkgs; };
    };
  in {
    db-sync = eval.config.services.cardano-db-sync.script;
    db-sync-extended = eval.config.services.cardano-db-sync-extended.script;
  };
in iohkNix.cardanoLib.forEnvironments mkStartScripts
