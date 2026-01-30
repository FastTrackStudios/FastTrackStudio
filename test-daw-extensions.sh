#!/bin/bash
# Test DAW Extensions in REAPER
#
# This script:
# 1. Builds the REAPER extension and DAW extensions
# 2. Installs the extension to REAPER
# 3. Launches REAPER with logging enabled
#
# Expected behavior:
# - daw-reaper extension should start and show "DAW Extension Ready"
# - daw-test extension should start and call play()
# - Every 5 seconds, you should see "Test log #N - Extension is alive!" in REAPER console
# - REAPER should start playing when daw-test calls play()

set -e

echo "🔨 Building extensions..."
cargo build -p daw-proto -p daw-reaper -p daw-test -p reaper_extension

echo ""
echo "📦 Installing REAPER extension..."
just install-extension

echo ""
echo "🎬 Launching REAPER..."
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "Expected output in REAPER console:"
echo "  1. Extension host initializes"
echo "  2. daw-reaper-extension loads"
echo "  3. daw-test-extension loads"
echo "  4. Transport: play (from daw-test calling play())"
echo "  5. [DAW Extension] Test log #1 - Extension is alive!"
echo "  6. [DAW Extension] Test log #2 - Extension is alive!"
echo "  7. ... (every 5 seconds)"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "Open REAPER console (Actions > Show REAPER console...) to see logs"
echo ""

# Launch REAPER with Rust backtrace enabled
RUST_BACKTRACE=1 RUST_LOG=debug just launch-reaper
