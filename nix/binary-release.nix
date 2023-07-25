# ###########################################################################
# Windows release CARDAN~1.ZIP
#
# This bundles up the windows build and its dependencies,
# and sets up the Hydra build artifact.
#
############################################################################

{ pkgs, project, version, exes, platform }:

let
  lib = pkgs.lib;
  name = "cardano-db-sync-${version}-${platform}";

in pkgs.runCommand name {
  buildInputs = with pkgs.pkgsBuildBuild; [
    haskellBuildUtils bintools nix zip
  ];
} ''
  mkdir -p $out release
  cd release

  cp -n --remove-destination -v ${
    pkgs.lib.concatMapStringsSep " " (exe: "${exe}/bin/*") exes
  } ./
  chmod -R +w .

  ${lib.optionalString (platform == "macos") (lib.concatMapStrings (exe: ''
    rewrite-libs . ${exe}/bin/*
  '') exes)}

  ${if (platform == "win64") then
    "zip -r $out/${name}.zip ."
  else
    "tar -czf $out/${name}.tar.gz ."}
  dist_file=$(ls $out)
  mkdir -p $out/nix-support
  echo "file binary-dist $out/$dist_file" > $out/nix-support/hydra-build-products
''
