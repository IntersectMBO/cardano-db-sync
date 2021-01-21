{ pkgs, lib, iohkNix, customConfig }:
let
  blacklistedEnvs = [ "selfnode" "shelley_selfnode" "latency-tests" "mainnet-ci" ];
  environments = lib.filterAttrs (k: v: (!builtins.elem k blacklistedEnvs)) iohkNix.cardanoLib.environments;
  mkStartScripts = envConfig: let
    systemdCompat.options = {
      systemd.services = lib.mkOption {};
      services.postgresql = lib.mkOption {};
      assertions = lib.mkOption {};
      users = lib.mkOption {};
    };
    eval = let
      extra = {
        internal.syncPackages = pkgs;
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
in iohkNix.cardanoLib.forEnvironmentsCustom mkStartScripts environments // { inherit environments; }
