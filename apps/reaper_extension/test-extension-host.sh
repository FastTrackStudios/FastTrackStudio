#!/bin/bash
# Test script for ROAM SHM Extension Host in REAPER Extension
set -e

echo "🧪 Testing ROAM SHM Extension Host"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo

# Get project root
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/../.." && pwd)"
EXTENSION_DIR="$PROJECT_ROOT/Extensions/FTS"

# Step 1: Build extensions
echo "📦 Building extensions..."
cargo build --bin hello-world-extension
cargo build --bin daw-reaper-extension
echo "   ✓ Built: target/debug/hello-world-extension"
echo "   ✓ Built: target/debug/daw-reaper-extension"
echo

# Step 1.5: Setup extension directory
echo "📁 Setting up extension directory..."
mkdir -p "$EXTENSION_DIR"
cp -v "$PROJECT_ROOT/target/debug/hello-world-extension" "$EXTENSION_DIR/"
cp -v "$PROJECT_ROOT/target/debug/daw-reaper-extension" "$EXTENSION_DIR/"
chmod +x "$EXTENSION_DIR/hello-world-extension"
chmod +x "$EXTENSION_DIR/daw-reaper-extension"
echo "   ✓ Extension directory: $EXTENSION_DIR"
echo

# Step 2: Build REAPER extension
echo "📦 Building REAPER extension..."
cargo build -p reaper_extension
echo "   ✓ Built: target/debug/libreaper_extension.dylib"
echo

# Step 3: Check paths
EXTENSION_PATH="target/debug/libreaper_extension.dylib"
PLUGIN_PATH="target/debug/hello-world-extension"

if [ ! -f "$EXTENSION_PATH" ]; then
    echo "❌ Extension not found: $EXTENSION_PATH"
    exit 1
fi

if [ ! -f "$PLUGIN_PATH" ]; then
    echo "❌ Plugin not found: $PLUGIN_PATH"
    exit 1
fi

echo "✅ Both binaries exist"
echo

# Step 4: Install extension
REAPER_PLUGINS_DIR="$HOME/Library/Application Support/REAPER/UserPlugins"
EXTENSION_SYMLINK="$REAPER_PLUGINS_DIR/reaper_extension.dylib"

echo "📋 Installing extension..."
mkdir -p "$REAPER_PLUGINS_DIR"

# Remove old symlink if exists
if [ -L "$EXTENSION_SYMLINK" ]; then
    rm "$EXTENSION_SYMLINK"
    echo "   Removed old symlink"
fi

# Create symlink (without 'lib' prefix)
ln -sf "$(pwd)/$EXTENSION_PATH" "$EXTENSION_SYMLINK"
echo "   ✓ Symlinked to: $EXTENSION_SYMLINK"
echo

# Step 5: Clean up old SHM segment
SHM_PATH="/tmp/fts-reaper-shm"
if [ -f "$SHM_PATH" ]; then
    rm "$SHM_PATH"
    echo "🧹 Cleaned up old SHM segment"
    echo
fi

# Step 6: Run REAPER with logging
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "🚀 Starting REAPER with SHM Plugin Host"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo
echo "Watch for:"
echo "  ✓ 'Initializing SHM Extension Host'"
echo "  ✓ 'Extension directory: .../Extensions/FTS'"
echo "  ✓ 'Discovering extensions in: .../Extensions/FTS'"
echo "  ✓ 'Found potential extension: hello-world-extension'"
echo "  ✓ 'Found potential extension: daw-reaper-extension'"
echo "  ✓ 'Extension spawned with peer ID: X'"
echo "  ✓ 'Extension registered successfully'"
echo "  ✓ 'Extension loaded successfully: hello-world-extension'"
echo "  ✓ 'Extension loaded successfully: daw-reaper-extension'"
echo
echo "Press Ctrl+C to stop REAPER"
echo

# Set RUST_LOG for detailed logging
export RUST_LOG="reaper_extension=debug,info"

# Find REAPER
if [ -f "/Applications/REAPER.app/Contents/MacOS/REAPER" ]; then
    exec /Applications/REAPER.app/Contents/MacOS/REAPER
elif command -v reaper &> /dev/null; then
    exec reaper
else
    echo "❌ REAPER not found"
    echo "   Please install REAPER or update the path in this script"
    exit 1
fi
