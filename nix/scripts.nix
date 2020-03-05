{ pkgs, lib, iohkNix, customConfig, syncPackages }:
let
  mkStartScripts = envConfig: let
    systemdCompat.options = {
      systemd.services = lib.mkOption {};
      services.postgresql = lib.mkOption {};
      users = lib.mkOption {};
    };
    eval = let
      extra = {
        internal.syncPackages = syncPackages;
        services.cardano-db-sync = {
          enable = true;
          postgres.user = "*";
          environment = envConfig;
          cluster = envConfig.name;
        };
      };
    in lib.evalModules {
      prefix = [];
      modules = import nixos/module-list.nix ++ [ systemdCompat customConfig extra ];
      args = { inherit pkgs; };
    };
  in {
    db-sync = eval.config.services.cardano-db-sync.script;
  };
in iohkNix.cardanoLib.forEnvironments mkStartScripts
