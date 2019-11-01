{ sources ? import ./nix/sources.nix
, system ? __currentSystem
, iohkLib ? import ./lib.nix { inherit system; }
, pkgs ? iohkLib.pkgs
}:
let
  niv = (import sources.niv {}).niv;
in pkgs.mkShell {
  buildInputs = [ niv ];
}
