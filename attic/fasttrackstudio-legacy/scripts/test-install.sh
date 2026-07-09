#!/bin/bash
# Run the installer in silent mode for quick testing.
# Usage: ./scripts/test-install.sh [install-dir]
set -euo pipefail

INSTALL_DIR="${1:-$HOME/Music/Dev/FastTrackStudio}"

echo "Testing silent install to: $INSTALL_DIR"
echo ""

# Clean previous test install
if [ -d "$INSTALL_DIR" ]; then
    echo "Removing previous test install..."
    rm -rf "$INSTALL_DIR"
fi

# Run the installer
cargo run -p fts-installer --release -- --silent --install-dir "$INSTALL_DIR"

echo ""
echo "Verifying install..."
ERRORS=0

check() {
    if [ -e "$1" ]; then
        echo "  OK  $1"
    else
        echo "  MISSING  $1"
        ERRORS=$((ERRORS + 1))
    fi
}

check "$INSTALL_DIR/Reaper/REAPER.app"
check "$INSTALL_DIR/Reaper/reaper.ini"
check "$INSTALL_DIR/Library"

if [ "$ERRORS" -eq 0 ]; then
    echo ""
    echo "All checks passed."
else
    echo ""
    echo "$ERRORS check(s) failed."
    exit 1
fi
