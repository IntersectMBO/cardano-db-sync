{ iohkLib, customConfig }:
with iohkLib.pkgs.lib;
let
  pkgs = iohkLib.pkgs;
  mkConnectScript = envConfig: let
    extraModule = {
      services.cardano-exporter = {
        enable = true;
        environment = envConfig;
        cluster = envConfig.name;
        postgres.user = "*";
      };
    };
    systemdCompat.options = {
      systemd.services = mkOption {};
      services.postgresql = mkOption {};
      users = mkOption {};
    };
    eval = pkgs.lib.evalModules {
      prefix = [];
      modules = [ ./nixos/cardano-exporter-service.nix systemdCompat extraModule customConfig ];
      args = { inherit pkgs; };
    };
  in eval.config.services.cardano-exporter.script;

  scripts = iohkLib.cardanoLib.forEnvironments (environment: {
    exporter = mkConnectScript environment;
  });
in scripts
