#!/usr/bin/env bash

# Unoffiical bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail
IFS=$'\n\t'

if test $# -eq 0 ; then
  echo
  echo "Usage: $0 [--require-magic|--require-no-magic] --genesis-hash <hash> --output <filename>"
  echo
  exit 1
  fi

genesis_hash=""
outfile=""
require_magic=2

while test $# -gt 0 ; do
  case "$1" in
    --require-magic)
        require_magic=1
        shift
        ;;

    --require-no-magic)
        require_magic=0
        shift
        ;;

    --genesis-hash)
        shift
        genesis_hash="$1"
        shift
        ;;
    --output)
        shift
        outfile="$1"
        shift
        ;;
    *)
        ;;
    esac
  done

case "$require_magic" in
  0)
    require_magic="RequiresNoMagic"
    ;;
  1)
    require_magic="RequiresMagic"
    ;;
  *)
    echo "Error: Missing --require-magic or --require-no-magic parameter"
    exit 1
    ;;
  esac

sed "s/^RequiresNetworkMagic.*/RequiresNetworkMagic: ${require_magic}/;s/^GenesisHash.*/GenesisHash: ${genesis_hash}/" \
	config/tx-submit-mainnet-config.yaml > "${outfile}"
