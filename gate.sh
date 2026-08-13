#!/usr/bin/env bash
set -uo pipefail
export CARGO_TARGET_DIR="$PWD/target-local"
direnv exec . "$@"
