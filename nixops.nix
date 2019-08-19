let
  commonLib = import ./lib.nix {};
  accessKeyId = "cardano-deployer";
  inherit (commonLib) pkgs;
  cardano-node = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-node";
    rev = "85989eff83ff8e6df0fef2d1674af3e6c40daa51";
    sha256 = "1cvp8awjfp2fzn0d8s73w9w13plaprfnycjzfnz6hyi0hwgypw2k";
  };
  byron-proxy = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-byron-proxy";
    rev = "78275dbf5bf207485e8b98bc20397491f822999e";
    sha256 = "1877q3gmxib85sa7kfcbwihx58783rgn8hi1grk9cyrn91md3xgc";
  };
  self = import ./. {};
  mkHost = { genesisFile, genesisHash, name, ... }: {
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
      networking.firewall.allowedTCPPorts = [ 3001 ];
      services = {
        cardano-exporter = {
          enable = true;
          environment = commonLib.cardanoLib.environments.mainnet;
          cluster = "mainnet";
          socketPath = "/var/lib/cardano-node/node-core-0.socket";
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
