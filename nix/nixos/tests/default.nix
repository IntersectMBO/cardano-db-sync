{ pkgs
,
}:

 let
  nixos-lib = import (pkgs.path + "/nixos/lib") { };
in {
  basicTest  = nixos-lib.runTest (import ./services-basic-test.nix { inherit pkgs; });
}
