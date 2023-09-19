#!/usr/bin/env bash

# This script installs a specific version of secp256k1 for both Linux and OSX.
#
# To use this script run it from the top level and provide a git sha from secp256k1.

# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -eu
set -o pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 SECP256K1_REF" >&2
    exit 2
fi

SECP256K1_REF=$1

UNAME=$(uname -s) INSTAL_CMD=
case $UNAME in
    Darwin )      INSTAL_CMD="make install";;
    Linux )       INSTAL_CMD="sudo make install";;
esac

PREFIX=
case $UNAME in
    Darwin ) PREFIX="";;
    Linux ) PREFIX="--prefix=/usr";;
esac

if [ ! -d "secp256k1" ]; then
    git clone https://github.com/bitcoin-core/secp256k1.git secp256k1
fi 

pushd secp256k1 || exit 1

git reset --hard "$SECP256K1_REF"
./autogen.sh
./configure $PREFIX --enable-module-schnorrsig --enable-experimental
make
make check
$INSTAL_CMD

popd || exit 2

