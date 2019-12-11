{ ... }@args:
# Imports the iohk-nix library.
# The version can be overridden for debugging purposes by setting
# NIX_PATH=iohk_nix=/path/to/iohk-nix
let
  try = builtins.tryEval <iohk_nix>;
in
  if try.success
  then builtins.trace "using host <iohk_nix>" ((import try.value) args)
else let
  sources = import ./nix/sources.nix;
  iohkNix = import sources.iohk-nix args;
  # Note that this repo is using the iohk-nix nixpkgs by default
  # A niv nixpkgs pin can override this with the following line:
  #iohkNix = import sources.iohk-nix ({ nixpkgsOverride = sources.nixpkgs; } // args);
in iohkNix // { inherit sources; }
