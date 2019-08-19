let
  self = import ../. {};
in self.haskellPackages.shellFor {
  name = "cardano-explorer";
  packages = ps: [ ps.cardano-explorer ];
  buildInputs = with self.pkgs.haskellPackages; [ hlint stylish-haskell ghcid ];
}
