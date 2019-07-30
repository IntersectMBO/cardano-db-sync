let
  self = import ../. {};
in self.haskellPackages.shellFor {
  name = "cardano-explorer-db-node";
  packages = ps: [ ps.cardano-explorer-db-node ];
  buildInputs = with self.pkgs.haskellPackages; [ hlint stylish-haskell ghcid ];
}
