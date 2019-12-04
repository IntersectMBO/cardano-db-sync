let
  self = import ../. {};
in self.haskellPackages.shellFor {
  name = "cardano-explorer-webapi";
  packages = ps: [ ps.cardano-explorer-webapi ];
  buildInputs = with self.pkgs.haskellPackages; [ hlint stylish-haskell ghcid ];
  shellHook = ''
    compare_api() {
      curl http://localhost:8100/api/$1 -o /tmp/compare-local
      curl https://cardanoexplorer.com/api/$1 -o /tmp/compare-remote
      echo "old explorer:"
      ${self.pkgs.jq}/bin/jq < /tmp/compare-remote "$2"
      ${self.pkgs.jq}/bin/jq < /tmp/compare-local "$2"
      ${self.pkgs.haskellPackages.aeson-diff}/bin/json-diff /tmp/compare-remote /tmp/compare-local | jq .
    }
  '';
}
