{ forDockerFile ? false }:

let
  rawNixpkgs = builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/d484f2b7fc0834a068e8ace851faa449a03963f5.tar.gz";
  helperPkgs = import rawNixpkgs { config = {}; overlays = []; };
  patchedNixpkgs = helperPkgs.runCommand "nixpkgs-patched" { patches = [ ./nixpkgs.patch ]; } ''
    cp -r ${rawNixpkgs} $out
    chmod -R +w $out
    cd $out
    patchPhase
  '';
  overlay = self: super: {
    deroot = self.runCommandCC "deroot" {} ''
      mkdir -pv $out/bin/
      g++ ${./deroot.cpp} -o $out/bin/deroot
    '';
  };
in
with import patchedNixpkgs { overlays = [ overlay ]; };

let
  secrets = import ./secrets.nix;
  iohkLib = import ../lib.nix { };
  self = import ../. {};
  targetEnv = iohkLib.cardanoLib.environments.mainnet;
  iohk-ops-src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "iohk-ops";
    rev = "d61dd51f7b961e244ed485cac51d3126f695b250";
    sha256 = "0nzzcgkrsq8m4kcjr430rybindfkys8kn5vdnnzmmhw5c2i33c7x";
  };
  cardano-node-src = fetchFromGitHub {
    owner = "input-output-hk";
    repo = "cardano-node";
    rev = "b475810273bbf158632bb273575df398ecbed240";
    sha256 = "1f7xphv7na7qbzdm1z27aqsdkxn6zgl2hqkkyznv3nhhr311267b";
  };
  customQueries = {
    cexplorerBlockCount = {
      query = "SELECT COUNT(*) AS count FROM block";
      metrics = [
        {
          count = {
            usage = "COUNTER";
            description = "total blocks in table";
          };
        }
      ];
    };
  };
  oauth_configuration = let
    secrets = import ./secrets.nix;
  in {
    services.oauth2_proxy = {
      inherit (secrets) redirectURL;
      cookie.secure = false;
    };
    services.monitoring-services = {
      grafanaCreds = {
        inherit (secrets) user;
        password = "admin";
      };
      oauth = {
        enable = true;
        inherit (secrets) clientID clientSecret emailDomain;
        cookie.secret = "fake";
      };
    };
  };
  noauth_configuration = {
    services.monitoring-services = {
      oauth.enable = false;
      grafanaCreds = {
        user = "admin";
        password = "admin";
      };
    };
  };
  configuration = { config, ... }: {
    imports = [
      (iohk-ops-src + "/modules/monitoring-services.nix")
      ../nix/nixos/cardano-exporter-service.nix
      (cardano-node-src + "/nix/nixos")
      (if builtins.pathExists ./secrets.nix then oauth_configuration else noauth_configuration)
    ];
    services.cardano-node = {
      environment = "mainnet";
      topology = iohkLib.cardanoLib.mkEdgeTopology { edgeHost = iohkLib.cardanoLib.environments.mainnet.edgeHost; edgePort = 7777; };
      enable = true;
    };
    services.cardano-exporter = {
      enable = true;
      inherit (targetEnv) genesisFile genesisHash;
      cluster = "mainnet";
      socketPath = "/run/cardano-node/node-core-0.socket";
    };
    services.postgresql = {
      enable = true;
      authentication = lib.mkBefore ''
        local all postgres trust
      '';
    };
    services.prometheus.exporters.postgres = {
      enable = true;
      runAsLocalSuperUser = true;
      dataSourceName = "user=postgres database=cexplorer host=/run/postgresql sslmode=disable";
      extraFlags = [
        "--disable-default-metrics"
        "--extend.query-path"
        (builtins.toFile "queries.yaml" (builtins.toJSON customQueries))
      ];
    };
    services.nginx = {
      virtualHosts.${config.services.monitoring-services.webhost} = {
        default = true;
        locations."/api/".extraConfig = ''
          proxy_pass http://localhost:8100/api/;
          proxy_set_header Host $host;
          proxy_set_header REMOTE_ADDR $remote_addr;
          proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          proxy_set_header X-Forwarded-Proto $scheme;
        '';
      };
    };
    services.prometheus.scrapeConfigs = [
      {
        job_name = "exporter";
        scrape_interval = "10s";
        metrics_path = "/";
        static_configs = [
          {
            targets = [ "localhost:8080" ];
          }
        ];
      }
      {
        job_name = "postgres";
        scrape_interval = "10s";
        metrics_path = "/metrics";
        static_configs = [
          {
            targets = [ "localhost:9187" ];
          }
        ];
      }
    ];
    nixpkgs.overlays = [ overlay ];
    services.monitoring-services = {
      enable = true;
      enableACME = false;
      enableWireguard = false;
      metrics = true;
      logging = false;
      webhost = "localhost.earthtools.ca";
      grafanaAutoLogin = true;
      monitoredNodes = {
      };
    };
    systemd.services = {
      cardano-explorer-node = {
        serviceConfig.PermissionsStartOnly = "true";
        preStart = ''
          chgrp cexplorer ${config.services.cardano-exporter.socketPath}
          chmod g+w ${config.services.cardano-exporter.socketPath}
        '';
      };
    };
  };
  eval = import "${patchedNixpkgs}/nixos" { inherit configuration; };
  patchedRunit = runit.overrideAttrs (old: {
    patches = old.patches ++ [ ./runit.patch ];
  });
  thing = writeScriptBin "thing" ''
    #!${pkgs.stdenv.shell}

    exec runit
  '';
  mkScript = dir: name: text: writeTextFile {
    name = name;
    text = ''
      #!${stdenv.shell}
      ${text}
    '';
    executable = true;
    destination = "${dir}${name}";
  };
  mkService = name: text: writeTextFile {
    name = name;
    text = ''
      #!${stdenv.shell}
      exec > /var/log/${name}.log
      ${text}
    '';
    executable = true;
    destination = "/etc/service/${name}/run";
  };
  stop = writeScriptBin "stop" ''
    #!${stdenv.shell}
    chmod u+x /etc/runit/stopit
    kill -cont 1
  '';
  one = mkScript "/etc/runit/" "1" ''
    mkdir /root /tmp
    chmod 1777 /tmp

    mkdir -p /var/log/

    touch /etc/runit/stopit
    chmod 0 /etc/runit/stopit
    echo one
    mkdir -p /var/lib/postgresql/9.6
    mkdir -p /run/wrappers/bin /run/postgresql /var/lib/prometheus2 /var/lib/grafana
    chown -R postgres /run/postgresql

    ${lib.optionalString (!forDockerFile) ''
      cat /etc/shadow.orig > /etc/shadow
      chmod 600 /etc/shadow
    ''}
    chown prometheus /var/lib/prometheus2
    chown grafana /var/lib/grafana

    mkdir -p /var/lib/cardano-node /run/cardano-node
    chown cardano-node /var/lib/cardano-node /run/cardano-node

    mkdir -p /var/lib/cexplorer
    chown cexplorer /var/lib/cexplorer
  '';
  users = {
    root = {
      uid = 0;
      shell = "/bin/bash";
      home = "/root";
      gid = 0;
    };
    postgres = {
      uid = 71;
      gid = 71;
      home = "/var/lib/postgresql/9.6";
    };
    grafana = {
      uid = 196;
      home = "/var/lib/grafana";
    };
    nginx = {
      uid = 60;
      gid = 74;
    };
    prometheus = {
      uid = 255;
      gid = 255;
    };
    oauth2_proxy = {
      uid = 1003;
    };
    cardano-node = {
      uid = 1004;
      gid = 72;
    };
    cexplorer = {
      uid = 1005;
      gid = 73;
      home = "/var/lib/cexplorer";
    };
    postgres-exporter = {
      uid = 1006;
    };
    nixbld1 = {
      uid = 1010;
      gid = 75;
      groups = [ "nixbld" ];
    };
  };
  userToPasswd = k: { uid, gid ? 65534, home ? "/var/empty", shell ? "/run/current-system/sw/bin/nologin", groups ? [] }: "${k}:x:${toString uid}:${toString gid}:description:${home}:${shell}";
  userToShadow = k: { ... }: "${k}:!:1::::::";
  userToUseradd = k: { uid, gid ? 65534, groups ? [], ... }: "useradd --uid=${toString uid} --gid=${toString gid} ${lib.optionalString (groups != []) "-G ${lib.concatStringsSep "," groups}"} ${k}";
  passwdContents = lib.concatStringsSep "\n" (lib.attrValues (lib.mapAttrs userToPasswd users));
  shadowContents = lib.concatStringsSep "\n" (lib.attrValues (lib.mapAttrs userToShadow users));
  createAllUsers = lib.concatStringsSep "\n" (lib.attrValues (lib.mapAttrs userToUseradd users));
  groups = {
    root.gid = 0;
    postgres.gid = 71;
    cardano-node.gid = 72;
    cexplorer.gid = 73;
    nginx.gid = 74;
    nixbld.gid = 75;
    prometheus.gid = 255;
  };
  groupToGroup = k: { gid }: "${k}:x:${toString gid}:";
  groupToGroupadd = k: { gid }: "groupadd --gid=${toString gid} ${k}";
  createAllGroups = lib.concatStringsSep "\n" (lib.attrValues (lib.mapAttrs groupToGroupadd groups));
  groupContents = lib.concatStringsSep "\n" (lib.attrValues (lib.mapAttrs groupToGroup groups));
  passwd = runCommand "passwd" {
    inherit passwdContents groupContents shadowContents;
    passAsFile = [ "passwdContents" "groupContents" "shadowContents" ];
    allowSubstitutes = false;
    preferLocalBuild = true;
  } ''
    mkdir -p $out/etc/pam.d

    cat $passwdContentsPath > $out/etc/passwd
    cat $groupContentsPath > $out/etc/group

    cat $shadowContentsPath > $out/etc/shadow.orig

    cat <<EOF > $out/etc/pam.d/sudo
    account required pam_unix.so

    auth sufficient pam_unix.so  likeauth try_first_pass
    auth required pam_deny.so

    password sufficient pam_unix.so nullok sha512

    session required pam_unix.so
    EOF

    cat <<EOF > $out/etc/nsswitch.conf
    passwd:    files mymachines systemd
    group:     files mymachines systemd
    shadow:    files

    hosts:     files mymachines mdns_minimal [NOTFOUND=return] dns mdns myhostname
    networks:  files

    ethers:    files
    services:  files
    protocols: files
    rpc:       files
    EOF
  '';
  dockerFileSetup = writeShellScript "dockerFileSetup" ''
    ${createAllGroups}

    ${createAllUsers}

    exit 0
  '';
  two = mkScript "/etc/runit/" "2" ''
    echo two

    runsvdir -P /etc/service
  '';
  three = mkScript "/etc/runit/" "3" ''
    echo three
  '';
  sleeper = mkService "sleeper" ''
    echo taking a nap
    sleep 120
    echo woke up!
    chmod u+x /etc/runit/stopit
    kill -cont 1
  '';
  web-api = mkService "web-api" ''
    export PGPASSFILE=${eval.config.services.cardano-exporter.pgpass}
    ${self.cardano-explorer}/bin/cardano-explorer
  '';
  wrapService = name: mkService name ''
    exec ${eval.config.systemd.services.${name}.runner}
  '';
  configFiles = buildEnv {
    name = "config-files";
    paths = [
      one two three
      #(wrapService "prometheus-blackbox-exporter")
      #(wrapService "prometheus-node-exporter")
      #sleeper
      web-api
      (wrapService "postgresql")
      (wrapService "prometheus")
      (wrapService "nginx")
      (wrapService "grafana")
      (wrapService "cardano-explorer-node")
      (wrapService "cardano-node")
      (wrapService "prometheus-postgres-exporter")
    ] ++ (lib.optional (builtins.pathExists ./secrets.nix) (wrapService "oauth2_proxy"));
  };
  dockerFileBinaries = buildEnv {
    name = "binaries";
    paths = [
      patchedRunit stop eval.config.services.postgresql.package
      deroot
    ];
  };
  image = dockerTools.buildLayeredImage {
    name = "docker-image";
    tag = "test-image";
    maxLayers = 100;
    contents = [
      patchedRunit configFiles thing coreutils
      bashInteractive
      sudo
      linux-pam
      nettools
      lsof
      iana_etc
      stop
      strace
      procps
      cacert.out
      eval.config.services.postgresql.package
      gnugrep
      less
      curl
      utillinux
      deroot
      passwd
    ];
    config = {
      Cmd = [ "thing" ];
    };
  };
  helper = writeScript "helper" ''
    #!${pkgs.stdenv.shell}
    set -e
    docker load < ${image}
    docker run --rm -i -p 80:80 --tty --cap-add SYS_PTRACE --name test-image --volume explorer-mainnet:/var/ docker-image:test-image
  '';
in {
  inherit image helper configFiles;
  inherit users userToPasswd lib passwd userToUseradd dockerFileSetup dockerFileBinaries;
}
