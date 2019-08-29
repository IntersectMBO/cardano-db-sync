let
  self = import ../. {};
in self.haskellPackages.shellFor {
  name = "cardano-explorer-db";
  packages = ps: [ ps.cardano-explorer-db ];
  buildInputs = with self.pkgs.haskellPackages; [ hlint stylish-haskell ghcid ];
}
