let
  sources = ./nix/sources.nix;
  commonLib = import ./lib.nix {};
  accessKeyId = "cardano-deployer";
  inherit (commonLib) pkgs;
  cardano-node = sources.cardano-node-nixops;
  byron-proxy = sources.byron-proxy;
  self = import ./. {};
  mkHost = { genesisFile, genesisHash, name, ... }@env: {
    name = "explorer-${name}";
    value = { options, config, lib, resources, ... }: {
      imports = [
        ./module.nix
        (cardano-node + "/nix/nixos")
        (byron-proxy + "/nix/nixos")
      ];
      deployment = {
        targetEnv = "ec2";
        ec2 = {
          region = "eu-central-1";
          instanceType = "t2.large";
          inherit accessKeyId;
          keyPair = resources.ec2KeyPairs.keypair;
          ebsInitialRootDiskSize = 40;
          securityGroups = lib.mkDefault [ "default" ];
          elasticIPv4 = resources.elasticIPs."eip-${name}";
        };
      };
      networking.firewall.allowedTCPPorts = [ 3001 8100 ];
      systemd.services = {
        cardano-explorer-node = {
          serviceConfig.PermissionsStartOnly = "true";
          preStart = ''
            chgrp cexplorer ${config.services.cardano-exporter.socketPath}
            chmod g+w ${config.services.cardano-exporter.socketPath}
          '';
        };
        cardano-explorer-webapi = {
          wantedBy = [ "multi-user.target" ];
          environment.PGPASSFILE = ./config/pgpass;
          serviceConfig.User = "cexplorer";
          script = ''
            ${self.haskellPackages.cardano-explorer.components.exes.cardano-explorer}/bin/cardano-explorer
          '';
        };
      };
      services = {
        cardano-exporter = {
          enable = true;
          environment = env;
          cluster = name;
          socketPath = "/var/lib/cardano-node/socket/node-core-0.socket";
        };
        byron-proxy = {
          enable = true;
          environment = name;
        };

        cardano-node = let
          ownIp = if options.networking.privateIPv4.isDefined then config.networking.privateIPv4 else "0.0.0.0";
        in {
          enable = true;
          inherit genesisFile genesisHash;
          consensusProtocol = "real-pbft";
          hostAddr = ownIp;
          port = 3001;
          topology = commonLib.cardanoLib.mkEdgeTopology { hostAddr = ownIp; };
          logger.configFile = ./log-configuration.yaml;
        };
      };
    };
  };
  mkIp = { name, ... }: {
    name = "eip-${name}";
    value = {
      inherit accessKeyId;
      region = "eu-central-1";
    };
  };
in {
  network.enableRollback = true;
  resources = {
    ec2KeyPairs.keypair = {
      region = "eu-central-1";
      inherit accessKeyId;
    };
    elasticIPs = builtins.listToAttrs (builtins.attrValues (commonLib.cardanoLib.forEnvironments mkIp));
  };
} // (builtins.listToAttrs (builtins.attrValues (commonLib.cardanoLib.forEnvironments mkHost)))
