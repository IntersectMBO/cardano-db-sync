{ pkgs, commonLib, chairmanScript, ... }:

let
  sources = import ../../sources.nix;

  chairman-runner = chairmanScript {
    chairman-config = {
      enable = true;
      k = 2160;
      timeout = 300;
      maxBlockNo = 2;
    };
  };

  psql = pkgs.postgresql + "/bin/psql";

  makeSureBlocksAreExported = pkgs.writeShellScript "block-check.sh" ''
    set -xe
    test $(su postgres -c '${pkgs.postgresql}/bin/psql cexplorer -t -c "select count(*) from block;"') -ge 2
  '';
in {
  name = "chairmans-cluster-test";
  nodes = {
    machine = { config, pkgs, ... }: {
      virtualisation.memorySize = 1024 * 12;
      imports = [
        (sources.cardano-node + "/nix/nixos")
        ../cardano-exporter-service.nix
      ];

      services.cardano-cluster = {
        enable = true;
        node-count = 3; # This must match nixos/scripts.nix:mkChairmanScript
      };

      services.cardano-exporter = {
        enable = true;
        environment = {
          inherit (config.services.cardano-node) genesisFile genesisHash;
        };
        cluster = "testnet";
        socketPath = "/run/cardano-node/node-core-0.socket";
      };

      systemd.services = {
        cardano-explorer-node = {
          wants = [ "cardano-node.service" ];
          serviceConfig.PermissionsStartOnly = "true";
          preStart = ''
            for x in {1..24}; do
              [ -S ${config.services.cardano-exporter.socketPath} ] && break
              echo loop $x: waiting for ${config.services.cardano-exporter.socketPath} 5 sec...
              sleep 5
            done
            chgrp cexplorer ${config.services.cardano-exporter.socketPath}
            chmod g+w ${config.services.cardano-exporter.socketPath}
          '';
        };
      };
    };
  };

  testScript = ''
    startAll
    $machine->waitForOpenPort(3001);
    $machine->waitForOpenPort(3002);
    $machine->waitForOpenPort(3003);
    $machine->succeed("${chairman-runner} 2>&1 | systemd-cat --identifier=chairman --priority=crit");
    $machine->succeed("${makeSureBlocksAreExported}");
  '';
}
