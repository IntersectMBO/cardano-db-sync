# This file is used by nix-shell.
# It just takes the shell attribute from default.nix.
{ config ? {}
, sourcesOverride ? {}
, withHoogle ? true
, customConfig ? {
    inherit withHoogle;
  }
, pkgs ? import ./nix {
    inherit config sourcesOverride customConfig;
  }
}:
with pkgs;
let
  inherit (pkgs.customConfig) withHoogle;
  commandHelp =
    ''
      echo "
        Commands:
          * nix flake update --update-input <iohkNix|haskellNix> - update input
      "
    '';

  # This provides a development environment that can be used with nix-shell or
  # lorri. See https://input-output-hk.github.io/haskell.nix/user-guide/development/
  shell = cardanoDbSyncProject.shellFor {
    name = "cabal-dev-shell";

    packages = ps: lib.attrValues (haskell-nix.haskellLib.selectProjectPackages ps);

    # These programs will be available inside the nix-shell.
    nativeBuildInputs = with haskellPackages; [
      cabalWrapped
      ghcid
      hlint
      stylish-haskell
      weeder
      nixWrapped
      pkgconfig
      sqlite-interactive
      tmux
      pkgs.git
    ];

    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    inherit withHoogle;
  };

  devops = pkgs.stdenv.mkDerivation {
    name = "devops-shell";
    nativeBuildInputs = [
      nixWrapped
    ];
    shellHook = ''
      echo "DevOps Tools" \
      | ${figlet}/bin/figlet -f banner -c \
      | ${lolcat}/bin/lolcat

      echo "NOTE: you may need to export GITHUB_TOKEN if you hit rate limits with niv flake update:"
      echo '      edit ~/.config/nix/nix.conf and add line `access-tokens = "github.com=23ac...b289"`'
      ${commandHelp}
    '';
  };

in

 shell // { inherit devops; }
