# Justfile for FastTrackStudio project
# Usage: just <command> [args...]

# Load .env file if it exists (for REAPER_PATH, REAPER_EXECUTABLE, BUILD_MODE)
# Environment variables can still be overridden: REAPER_PATH=/path just <command>

# Helper function to load .env file
# This will be used in bash scripts to source the .env file
load-env := "if [ -f .env ]; then set -a; source .env; set +a; fi"

# Default values (used if .env doesn't exist or variable not set)
DEFAULT_REAPER_PATH := "/Users/codywright/Music/FTS-REAPER"
DEFAULT_REAPER_EXECUTABLE := "/Users/codywright/Music/FTS-REAPER/FTS-LIVE.app/Contents/MacOS/REAPER"
DEFAULT_BUILD_MODE := "debug"

# ============================================================================
# REAPER Extension
# ============================================================================

# Build the REAPER extension
build-extension:
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Save command-line EXTENSION_FEATURES if set (before loading .env)
    CMD_LINE_FEATURES="${EXTENSION_FEATURES:-}"
    
    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi
    
    # Command-line override takes precedence over .env
    if [[ -n "$CMD_LINE_FEATURES" ]]; then
        EXTENSION_FEATURES="$CMD_LINE_FEATURES"
    fi
    
    BUILD_MODE="${BUILD_MODE:-debug}"
    
    # Build with features from .env (or default)
    # EXTENSION_FEATURES can be:
    #   - "all" or empty: use default features from Cargo.toml
    #   - "none": use --no-default-features
    #   - comma-separated list: use --features "feature1,feature2"
    # Default to "all" if not set
    EXTENSION_FEATURES="${EXTENSION_FEATURES:-all}"
    
    # Debug: show what we're using
    echo "🔍 EXTENSION_FEATURES: [$EXTENSION_FEATURES]"
    
    if [[ "$EXTENSION_FEATURES" == "none" ]]; then
        echo "🔧 Building extension with NO features (completely transparent)"
        if [[ "$BUILD_MODE" == "release" ]]; then
            cargo build --package reaper_extension --release --no-default-features
        else
            cargo build --package reaper_extension --no-default-features
        fi
    elif [[ "$EXTENSION_FEATURES" == "all" ]]; then
        # "all" = default features
        echo "🔧 Building extension with default features"
        if [[ "$BUILD_MODE" == "release" ]]; then
            cargo build --package reaper_extension --release
        else
            cargo build --package reaper_extension
        fi
    elif [[ -z "$EXTENSION_FEATURES" ]]; then
        # Empty string = default features (shouldn't happen due to default above, but handle it)
        echo "🔧 Building extension with default features (empty EXTENSION_FEATURES)"
        if [[ "$BUILD_MODE" == "release" ]]; then
            cargo build --package reaper_extension --release
        else
            cargo build --package reaper_extension
        fi
    else
        # Handle comma-separated feature list
        # Use --no-default-features to exclude defaults, then add only the specified features
        echo "🔧 Building extension with features: $EXTENSION_FEATURES (excluding defaults)"
        if [[ "$BUILD_MODE" == "release" ]]; then
            cargo build --package reaper_extension --release --no-default-features --features "$EXTENSION_FEATURES"
        else
            cargo build --package reaper_extension --no-default-features --features "$EXTENSION_FEATURES"
        fi
    fi

# Install the REAPER extension to the specified REAPER path
install-extension: build-extension
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi
    
    REAPER_PATH="${REAPER_PATH:-/Users/codywright/Music/FTS-REAPER}"
    EXTENSION_DIR="$REAPER_PATH/UserPlugins"
    EXTENSION_NAME="reaper_extension"
    BUILD_MODE="${BUILD_MODE:-debug}"
    
    # Create UserPlugins directory if it doesn't exist
    mkdir -p "$EXTENSION_DIR"
    
    BUILD_DIR="target/$BUILD_MODE"
    
    # Find the built extension file (must start with reaper_ on Unix/macOS)
    if [[ -f "$BUILD_DIR/libreaper_extension.so" ]]; then
        EXTENSION_FILE="$BUILD_DIR/libreaper_extension.so"
        TARGET_NAME="${EXTENSION_NAME}.so"
    elif [[ -f "$BUILD_DIR/reaper_extension.dll" ]]; then
        EXTENSION_FILE="$BUILD_DIR/reaper_extension.dll"
        TARGET_NAME="${EXTENSION_NAME}.dll"
    elif [[ -f "$BUILD_DIR/libreaper_extension.dylib" ]]; then
        EXTENSION_FILE="$BUILD_DIR/libreaper_extension.dylib"
        TARGET_NAME="${EXTENSION_NAME}.dylib"
    else
        echo "❌ Error: Extension file not found in $BUILD_DIR"
        echo "💡 Run 'just build-extension' first"
        exit 1
    fi
    
    # Remove existing file/symlink if it exists
    if [[ -L "$EXTENSION_DIR/$TARGET_NAME" ]] || [[ -f "$EXTENSION_DIR/$TARGET_NAME" ]]; then
        rm -f "$EXTENSION_DIR/$TARGET_NAME"
    fi
    
    # Get absolute path to the extension file
    ABS_EXTENSION_FILE="$(cd "$(dirname "$EXTENSION_FILE")" && pwd)/$(basename "$EXTENSION_FILE")"
    
    # Create symlink to the extension (for development - automatically updates on rebuild)
    ln -s "$ABS_EXTENSION_FILE" "$EXTENSION_DIR/$TARGET_NAME"
    
    echo "✅ Extension symlinked to: $EXTENSION_DIR/$TARGET_NAME"
    echo "📁 Source: $EXTENSION_FILE"
    echo ""
    echo "🚀 Now start REAPER and check the console for our message!"

# Create a symlink to the extension (for development)
link-extension: build-extension
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi
    
    REAPER_PATH="${REAPER_PATH:-/Users/codywright/Music/FTS-REAPER}"
    EXTENSION_DIR="$REAPER_PATH/UserPlugins"
    EXTENSION_NAME="reaper_extension"
    BUILD_MODE="${BUILD_MODE:-debug}"
    BUILD_DIR="target/$BUILD_MODE"
    
    # Create UserPlugins directory if it doesn't exist
    mkdir -p "$EXTENSION_DIR"
    
    # Find the built extension file
    if [[ -f "$BUILD_DIR/libreaper_extension.so" ]]; then
        EXTENSION_FILE="$BUILD_DIR/libreaper_extension.so"
        TARGET_NAME="${EXTENSION_NAME}.so"
    elif [[ -f "$BUILD_DIR/reaper_extension.dll" ]]; then
        EXTENSION_FILE="$BUILD_DIR/reaper_extension.dll"
        TARGET_NAME="${EXTENSION_NAME}.dll"
    elif [[ -f "$BUILD_DIR/libreaper_extension.dylib" ]]; then
        EXTENSION_FILE="$BUILD_DIR/libreaper_extension.dylib"
        TARGET_NAME="${EXTENSION_NAME}.dylib"
    else
        echo "❌ Error: Extension file not found in $BUILD_DIR"
        echo "💡 Run 'just build-extension' first"
        exit 1
    fi
    
    # Remove existing symlink if it exists
    rm -f "$EXTENSION_DIR/$TARGET_NAME"
    
    # Create symlink with absolute path
    ln -s "$(realpath "$EXTENSION_FILE")" "$EXTENSION_DIR/$TARGET_NAME"
    
    echo "🔗 Extension symlinked to: $EXTENSION_DIR/$TARGET_NAME"
    echo "📁 Source: $(realpath "$EXTENSION_FILE")"
    echo ""
    echo "🚀 Now start REAPER and check the console for our message!"
    echo "💡 The symlink will automatically update when you rebuild!"

# Remove the extension from REAPER
uninstall-extension:
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi
    
    REAPER_PATH="${REAPER_PATH:-/Users/codywright/Music/FTS-REAPER}"
    EXTENSION_DIR="$REAPER_PATH/UserPlugins"
    EXTENSION_NAME="reaper_extension"
    
    # Remove all possible extension files
    rm -f "$EXTENSION_DIR/${EXTENSION_NAME}.so"
    rm -f "$EXTENSION_DIR/${EXTENSION_NAME}.dll"
    rm -f "$EXTENSION_DIR/${EXTENSION_NAME}.dylib"
    rm -f "$EXTENSION_DIR/libreaper_extension.so"
    rm -f "$EXTENSION_DIR/libreaper_extension.dylib"
    
    echo "🗑️  Extension removed from: $EXTENSION_DIR"

# ============================================================================
# CLI Application
# ============================================================================

# Build the CLI application
build-cli:
    #!/usr/bin/env bash
    set -euo pipefail
    BUILD_MODE="${BUILD_MODE:-debug}"
    if [[ "$BUILD_MODE" == "release" ]]; then
        cargo build --package cli --release
    else
        cargo build --package cli
    fi

# Run the CLI application
run-cli: build-cli
    #!/usr/bin/env bash
    set -euo pipefail
    BUILD_MODE="${BUILD_MODE:-debug}"
    BUILD_DIR="target/$BUILD_MODE"
    exec "$BUILD_DIR/cli"

# ============================================================================
# General Build Commands
# ============================================================================

# Build everything
build:
    #!/usr/bin/env bash
    set -euo pipefail
    BUILD_MODE="${BUILD_MODE:-debug}"
    if [[ "$BUILD_MODE" == "release" ]]; then
        cargo build --release
    else
        cargo build
    fi

# Build in release mode
build-release:
    #!/usr/bin/env bash
    set -euo pipefail
    BUILD_MODE=release cargo build --release

# Clean build artifacts
clean:
    cargo clean

# Run tests
test:
    cargo test

# Run tests with output
test-verbose:
    cargo test -- --nocapture

# Check code without building
check:
    cargo check

# Format code
fmt:
    cargo fmt

# Lint code
lint:
    cargo clippy

# ============================================================================
# REAPER Utilities
# ============================================================================

# Show the configured REAPER path
show-reaper-path:
    #!/usr/bin/env bash
    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi
    
    REAPER_PATH="${REAPER_PATH:-/Users/codywright/Music/FTS-REAPER}"
    echo "📁 REAPER Path: $REAPER_PATH"
    echo "📁 Extension Directory: $REAPER_PATH/UserPlugins"
    if [[ -d "$REAPER_PATH/UserPlugins" ]]; then
        echo "✅ UserPlugins directory exists"
        echo ""
        echo "Installed extensions:"
        ls -la "$REAPER_PATH/UserPlugins" | grep -E "\.(so|dll|dylib)$" || echo "  (none)"
    else
        echo "⚠️  UserPlugins directory does not exist"
    fi

# Launch REAPER (macOS) - runs in foreground to show logs
launch-reaper:
    #!/usr/bin/env bash
    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi
    
    REAPER_EXECUTABLE="${REAPER_EXECUTABLE:-/Users/codywright/Music/FTS-REAPER/FTS-LIVE.app/Contents/MacOS/REAPER}"
    if [[ -f "$REAPER_EXECUTABLE" ]]; then
        echo "🚀 Launching REAPER: $REAPER_EXECUTABLE"
        echo "📋 Logs will appear below. Press Ctrl+C to stop REAPER."
        echo ""
        # Run the executable directly to see stdout/stderr in terminal
        # Change to the app's Resources directory so REAPER can find its resources
        APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
        cd "$APP_DIR/Contents/Resources" || exit 1
        exec "$REAPER_EXECUTABLE"
    else
        echo "❌ REAPER executable not found: $REAPER_EXECUTABLE"
        echo "💡 Set REAPER_EXECUTABLE environment variable to the correct path"
        exit 1
    fi

# Build extension, link it, and launch REAPER in foreground for testing
test-reaper: link-extension
    #!/usr/bin/env bash
    set -euo pipefail
    
    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi
    
    echo "✅ Extension built and linked"
    echo "🚀 Launching REAPER..."
    echo ""
    
    # Launch REAPER in foreground (this will show logs)
    REAPER_EXECUTABLE="${REAPER_EXECUTABLE:-/Users/codywright/Music/FTS-REAPER/FTS-LIVE.app/Contents/MacOS/REAPER}"
    if [[ -f "$REAPER_EXECUTABLE" ]]; then
        echo "📋 Logs will appear below. Press Ctrl+C to stop REAPER."
        echo ""
        # Run the executable directly to see stdout/stderr in terminal
        # Change to the app's Resources directory so REAPER can find its resources
        APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
        cd "$APP_DIR/Contents/Resources" || exit 1
        exec "$REAPER_EXECUTABLE"
    else
        echo "❌ REAPER executable not found: $REAPER_EXECUTABLE"
        echo "💡 Set REAPER_EXECUTABLE environment variable to the correct path"
        exit 1
    fi

# Show running REAPER processes
show-reaper-procs:
    #!/usr/bin/env bash
    echo "🔍 Checking for running REAPER processes..."
    if pgrep -f "REAPER" > /dev/null; then
        ps aux | grep -i reaper | grep -v grep
    else
        echo "No REAPER processes found"
    fi

# ============================================================================
# Development Helpers
# ============================================================================

# Install all (extension + CLI)
install-all: install-extension
    echo "✅ All components installed"

# Link all (extension + CLI)
link-all: link-extension
    echo "✅ All components linked"

# Uninstall all
uninstall-all: uninstall-extension
    echo "✅ All components uninstalled"

# Build all
build-all: build-extension build-cli
    echo "✅ All components built"

# Quick development cycle: build extension, link it, and show status
dev-cycle: link-extension show-reaper-path
    echo ""
    echo "💡 Next steps:"
    echo "   1. Launch REAPER: just launch-reaper"
    echo "   2. Check console for extension messages"
    echo "   3. Make changes and run 'just dev-cycle' again"

# Show help
help:
    #!/usr/bin/env bash
    echo "FastTrackStudio Justfile Commands"
    echo ""
    echo "REAPER Extension:"
    echo "  build-extension      Build the REAPER extension"
    echo "  install-extension    Install extension to REAPER (copies file)"
    echo "  link-extension      Link extension to REAPER (symlink for dev)"
    echo "  uninstall-extension Remove extension from REAPER"
    echo ""
    echo "CLI Application:"
    echo "  build-cli           Build the CLI application"
    echo "  run-cli             Build and run the CLI application"
    echo ""
    echo "General:"
    echo "  build               Build all packages"
    echo "  build-release       Build all packages in release mode"
    echo "  clean               Clean build artifacts"
    echo "  test                Run tests"
    echo "  check               Check code without building"
    echo "  fmt                 Format code"
    echo "  lint                Lint code with clippy"
    echo ""
    echo "REAPER Utilities:"
    echo "  show-reaper-path    Show configured REAPER paths"
    echo "  launch-reaper       Launch REAPER (macOS)"
    echo "  show-reaper-procs   Show running REAPER processes"
    echo ""
    echo "Development:"
    echo "  install-all         Install all components"
    echo "  link-all            Link all components"
    echo "  build-all           Build all components"
    echo "  dev-cycle           Build, link extension, and show status"
    echo ""
    echo "Configuration:"
    echo "  .env file           Create .env from .env.example and customize paths"
    echo "  Environment vars    Can override .env: REAPER_PATH=/path just <command>"
    echo ""
    echo "Environment Variables (from .env or command line):"
    echo "  REAPER_PATH         REAPER resource directory"
    echo "  REAPER_EXECUTABLE   REAPER executable path"
    echo "  BUILD_MODE          Set to 'release' for release builds"
    echo "  EXTENSION_FEATURES  Extension features: 'all' (default), 'none', or comma-separated list"
    echo ""
    echo "Examples:"
    echo "  just build-extension"
    echo "  just link-extension"
    echo "  BUILD_MODE=release just build-all"
    echo "  REAPER_PATH=/custom/path just install-extension"
    echo "  EXTENSION_FEATURES=none just build-extension  # Build without input feature"
    echo "  EXTENSION_FEATURES=input just build-extension  # Build with only input feature"
    echo ""
    echo "Setup:"
    echo "  cp .env.example .env  # Create .env file from template"
    echo "  # Then edit .env with your REAPER paths"

