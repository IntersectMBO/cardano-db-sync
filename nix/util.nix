{ pkgs ? import <nixpkgs> {} }:

with pkgs.lib;

{
  # TODO: Replace this with a function that identifies packages from your
  # project.
  # As an example, this matches packages called "iohk-skeleton*" and the package
  # called "another-package".
  # fixme: automate this in nix-tools/Haskell.nix.
  isIohkSkeleton = package:
    (hasPrefix "iohk-skeleton" package.identifier.name) ||
    (elem package.identifier.name [ "another-package" ]);

  # fixme: upstream to iohk-nix
  collectComponents = group: packageSel: haskellPackages:
    (mapAttrs (_: package: package.components.${group} // { recurseForDerivations = true; })
     (filterAttrs (name: package: (package.isHaskell or false) && packageSel package) haskellPackages))
    // { recurseForDerivations = true; };
}
