#!/usr/bin/env bash

set -euo pipefail

git ls-files -- '*.hs' \
  | grep -v Setup.hs \
  | xargs fourmolu -c -m inplace

git diff --exit-code
