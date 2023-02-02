{ system ? builtins.currentSystem, crossSystem ? null, config ? { }
, customConfig ? { }, sourcesOverride ? { }, gitrev ? null }:
let
  flakeSources = let
    flakeLock = (builtins.fromJSON (builtins.readFile ../flake.lock)).nodes;
    compat = s:
      builtins.fetchGit {
        url = "https://github.com/${s.locked.owner}/${s.locked.repo}.git";
        inherit (s.locked) rev;
        ref = s.original.ref or "master";
      };
  in {
    nixpkgs = compat flakeLock.nixpkgs-unstable;
    "haskell.nix" = compat flakeLock.${flakeLock.root.inputs.haskellNix};
    "iohk-nix" = compat flakeLock.${flakeLock.root.inputs.iohkNix};
    "cardano-world" = compat flakeLock.${flakeLock.root.inputs.cardano-world};
    "flake-compat" = compat flakeLock.${flakeLock.root.inputs.flake-compat};
    "CHaP" = compat flakeLock.${flakeLock.root.inputs.CHaP};
  };
  sources = flakeSources // sourcesOverride;
  iohkNix = import sources.iohk-nix { inherit system; };
  flake-compat = import sources.flake-compat;
  haskellNix = import sources."haskell.nix" {
    inherit system sourcesOverride;
    pkgs = import sources.nixpkgs {
      # In nixpkgs versions older than 21.05, if we don't explicitly pass
      # in localSystem we will hit a code path that uses builtins.currentSystem,
      # which breaks flake's pure evaluation.
      localSystem = { inherit system; };
    };
  };
  nixpkgs = haskellNix.sources.nixpkgs-unstable;
  CHaP = sources.CHaP;
  cardano-world = flake-compat {
    src = sources.cardano-world;
  };

  inputMap = { "https://input-output-hk.github.io/cardano-haskell-packages" = CHaP; };

  # for inclusion in pkgs:
  overlays =
    # Haskell.nix (https://github.com/input-output-hk/haskell.nix)
    haskellNix.nixpkgsArgs.overlays
    # haskell-nix.haskellLib.extra: some useful extra utility functions for haskell.nix
    ++ iohkNix.overlays.haskell-nix-extra ++ iohkNix.overlays.crypto
    # iohkNix: nix utilities and niv:
    ++ iohkNix.overlays.iohkNix ++ iohkNix.overlays.utils
    # our own overlays:
    ++ [
      (pkgs: _:
        with pkgs; {
          gitrev = if gitrev == null then
            iohkNix.commitIdFromGitRepoOrZero ../.git
          else
            gitrev;

          customConfig =
            lib.recursiveUpdate (import ../custom-config pkgs.customConfig)
            customConfig;

          cardanoLib = rec {
            inherit (cardano-world.defaultNix.${pkgs.system}.cardano) environments;
            forEnvironments = f: lib.mapAttrs
              (name: env: f (env // { inherit name; }))
              environments;
          };
          # commonLib: mix pkgs.lib with iohk-nix utils and our own:
          commonLib = lib // cardanoLib // iohk-nix.lib // import ./util.nix {
            inherit haskell-nix;
          }
          # also expose our sources and overlays
            // {
              inherit overlays sources;
            };
        })
      # And, of course, our haskell-nix-ified cabal project:
      (import ./pkgs.nix { inherit inputMap; })
    ];

  pkgs = import nixpkgs {
    inherit crossSystem overlays;
    config = haskellNix.nixpkgsArgs.config // config;

    # In nixpkgs versions older than 21.05, if we don't explicitly pass
    # in localSystem we will hit a code path that uses builtins.currentSystem,
    # which breaks flake's pure evaluation.
    localSystem = { inherit system; };
  };

in pkgs
