{ sources ? import ../../sources.nix, pkgs ? import sources.nixpkgs { }
, cardano-node ? import sources.cardano-node { }
, commonLib ? import ../../lib.nix { }, supportedSystems ? [ "x86_64-linux" ] }:

let
  inherit (pkgs.lib) genAttrs hydraJob;
  forAllSystems = genAttrs supportedSystems;

  makeTest = import (pkgs.path + "/nixos/tests/make-test.nix");

  importTest = fn: args: system:
    let
      imported = import fn;
      test = makeTest imported;
    in test ({ inherit system; } // args);

  callTest = fn: args:
    forAllSystems (system: hydraJob (importTest fn args system));

  chairmanScript = cardano-node.scripts.testnet.chairman;
in rec {
  chairmansCluster =
    callTest ./chairmans-cluster.nix { inherit pkgs commonLib chairmanScript; };

  chairmansClusterDriver = (makeTest (import ./chairmans-cluster.nix) {
    inherit commonLib chairmanScript;
  });
}
