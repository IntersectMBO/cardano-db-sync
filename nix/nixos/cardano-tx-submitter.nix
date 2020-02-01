{ config, lib, pkgs, ... }:

# notes:
# this service exposes an http port, and connects to a cardano-node over a unix socket
let
  cfg = config.services.cardano-tx-submit-webapi;
  self = import ../.. { };
  envConfig = cfg.environment;
  localLib = import ../../lib.nix {};
in {
  options = {
    services.cardano-tx-submit-webapi = {
      enable = lib.mkEnableOption "enable the cardano-explorer tx submitter api";
      script = lib.mkOption {
        internal = true;
        type = lib.types.package;
      };
      port = lib.mkOption {
        type = lib.types.port;
        default = 8101;
      };
      socketPath = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
      };
      cluster = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        description = "cluster name";
      };
      environment = lib.mkOption {
        type = lib.types.nullOr lib.types.attrs;
        default = localLib.cardanoLib.environments.${cfg.cluster};
      };
    };
  };
  config = lib.mkIf cfg.enable {
    services.cardano-tx-submit-webapi.script = pkgs.writeShellScript "cardano-tx-submit-webapi" ''
      ${if (cfg.socketPath == null) then ''if [ -z "$CARDANO_NODE_SOCKET_PATH" ]
      then
        echo "You must set \$CARDANO_NODE_SOCKET_PATH"
        exit 1
      fi'' else "export \"CARDANO_NODE_SOCKET_PATH=${cfg.socketPath}\""}
      exec ${self.cardano-tx-submit-webapi}/bin/cardano-tx-submit-webapi --socket-path "$CARDANO_NODE_SOCKET_PATH" \
            --genesis-file ${envConfig.genesisFile} \
            --port ${toString config.services.cardano-tx-submit-webapi.port} \
            --config ${builtins.toFile "tx-submit.json" (builtins.toJSON cfg.environment.txSubmitConfig)}
    '';
    systemd.services.cardano-tx-submit-webapi = {
      serviceConfig = {
        ExecStart = config.services.cardano-tx-submit-webapi.script;
        User = "cexplorer";
        WorkingDirectory = "/var/lib/cexplorer";
      };
      wantedBy = [ "multi-user.target" ];
      after = [ "postgresql.service" "cardano-explorer-node.service" ];
    };
  };
}
