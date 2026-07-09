#!/usr/bin/env bash
# ci/run-reaper-tests.sh — Run REAPER integration tests on Linux (headless via Xvfb)
#
# Usage:
#   nix develop --command ci/run-reaper-tests.sh [test-filter]
#
# This script:
#   1. Downloads REAPER Linux x86_64 (or uses REAPER_EXECUTABLE if set)
#   2. Creates a portable REAPER install with headless config
#   3. Builds the reaper-extension and symlinks .so into UserPlugins/
#   4. Runs REAPER under xvfb-run and delegates to `cargo xtask reaper-test --no-build`
#
# Environment variables (all optional):
#   REAPER_VERSION   — REAPER version to download (default: 7.34)
#   REAPER_EXECUTABLE — Path to existing REAPER binary (skips download)
#   REAPER_PATH      — Override REAPER config/resource directory
#   REAPER_RESOURCES — Override REAPER resources directory (defaults to REAPER_PATH)
#   FTS_TEST_FILTER  — Test name filter (alternative to positional arg)

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
WORKSPACE_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
REAPER_VERSION="${REAPER_VERSION:-7.34}"
TEST_FILTER="${1:-${FTS_TEST_FILTER:-}}"

echo "=== REAPER Integration Tests (CI) ==="
echo "  Workspace: $WORKSPACE_ROOT"
echo "  REAPER version: $REAPER_VERSION"

# ── Step 1: Provision REAPER ──────────────────────────────────────────────────

REAPER_TMPDIR="$(mktemp -d)"
trap 'rm -rf "$REAPER_TMPDIR"' EXIT

if [[ -n "${REAPER_EXECUTABLE:-}" ]] && command -v "$REAPER_EXECUTABLE" &>/dev/null; then
    echo "  Using existing REAPER: $REAPER_EXECUTABLE"
    REAPER_BIN="$REAPER_EXECUTABLE"
else
    echo "  Downloading REAPER $REAPER_VERSION for Linux x86_64..."
    REAPER_TARBALL="reaper${REAPER_VERSION//.}_linux_x86_64.tar.xz"
    REAPER_URL="https://www.reaper.fm/files/${REAPER_VERSION%%.*}.x/${REAPER_TARBALL}"

    curl -fsSL "$REAPER_URL" -o "$REAPER_TMPDIR/$REAPER_TARBALL"
    tar -xf "$REAPER_TMPDIR/$REAPER_TARBALL" -C "$REAPER_TMPDIR"

    # The tarball extracts to reaper_linux_x86_64/
    REAPER_EXTRACT_DIR="$REAPER_TMPDIR/reaper_linux_x86_64"
    if [[ ! -d "$REAPER_EXTRACT_DIR" ]]; then
        # Try alternate directory name
        REAPER_EXTRACT_DIR="$(find "$REAPER_TMPDIR" -maxdepth 1 -type d -name 'reaper*' | head -1)"
    fi

    REAPER_BIN="$REAPER_EXTRACT_DIR/REAPER/reaper"
    if [[ ! -x "$REAPER_BIN" ]]; then
        # Some tarballs have the binary at the top level
        REAPER_BIN="$(find "$REAPER_EXTRACT_DIR" -name 'reaper' -type f | head -1)"
    fi
    chmod +x "$REAPER_BIN"
    echo "  REAPER binary: $REAPER_BIN"
fi

# ── Step 2: Create portable REAPER install ────────────────────────────────────

REAPER_PORTABLE="${REAPER_PATH:-$REAPER_TMPDIR/reaper-portable}"
mkdir -p "$REAPER_PORTABLE/UserPlugins"

# Minimal config: no audio, no updates, no splash, no autosave
cat > "$REAPER_PORTABLE/reaper.ini" <<'INI'
[REAPER]
audiodev=
noupdate=1
verchk=0
autosave=0
showlastfn=0
INI

echo "  Portable install: $REAPER_PORTABLE"

# ── Step 3: Build extension and symlink ───────────────────────────────────────

echo ""
echo ">>> Building REAPER extension..."
cd "$WORKSPACE_ROOT"
cargo build -p reaper-extension

# Symlink the .so into UserPlugins
EXT_SO="$WORKSPACE_ROOT/target/debug/libreaper_fts.so"
if [[ -f "$EXT_SO" ]]; then
    ln -sf "$EXT_SO" "$REAPER_PORTABLE/UserPlugins/libreaper_fts.so"
    echo "  Linked $EXT_SO → UserPlugins/"
else
    echo "ERROR: Extension not found at $EXT_SO"
    exit 1
fi

# ── Step 4: Run tests under Xvfb ─────────────────────────────────────────────

echo ""
echo ">>> Running tests under xvfb-run..."

export REAPER_EXECUTABLE="$REAPER_BIN"
export REAPER_PATH="$REAPER_PORTABLE"
export REAPER_RESOURCES="$REAPER_PORTABLE"

XTASK_ARGS="reaper-test --no-build"
if [[ -n "$TEST_FILTER" ]]; then
    XTASK_ARGS="$XTASK_ARGS $TEST_FILTER"
fi

xvfb-run -a cargo xtask $XTASK_ARGS

echo ""
echo "=== REAPER integration tests passed ==="
