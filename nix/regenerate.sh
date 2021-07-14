#!/usr/bin/env bash
set -euo pipefail
cd $(git rev-parse --show-toplevel)

nix_version=$(nix --version | sed 's/.* //;s/p.*//')

if test nix_version -lt 2.4 ; then
  echo "Nix version 2.4 (which may be unstable) or greater required."
  echo "To update the Nix channels: nix-channel --update"
  echo "To switch to unstable Nix: nix-env -iA nixpkgs.nixUnstable"
  echo "To switch to stable Nix: nix-env -iA nixpkgs.nix"
  exit 0
  fi

nix --experimental-features "nix-command flakes" flake lock  --update-input haskellNix
