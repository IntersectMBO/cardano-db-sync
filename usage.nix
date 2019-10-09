with import <nixpkgs> {};
let
  self = import ./. {};
in runCommand "usage" {
  buildInputs = [
    self.cardano-explorer-node
    self.haskellPackages.cardano-explorer-db.components.exes.cardano-explorer-db-tool
    self.haskellPackages.cardano-node.components.exes.cardano-node
  ];
  shellHook = ''
    for EXE in cardano-explorer-node cardano-explorer-db-tool cardano-node; do
      source <($EXE --bash-completion-script `type -p $EXE`)
    done
  '';
} ''echo 'use "nix-shell usage.nix" to use the CLI tools' ''
