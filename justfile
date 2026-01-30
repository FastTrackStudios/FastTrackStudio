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

# Rebuild extensions (triggers hot-reload if REAPER is running)
rebuild-extensions:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📦 Rebuilding extensions..."
    cargo build -p daw-proto -p daw-test -p hello-world -p http-gateway -p socket-gateway
    echo "✅ Extensions rebuilt - hot-reload should pick up changes"

# Build extension, link it, build extensions, symlink them, and launch REAPER in foreground for testing
test-reaper: link-extension
    #!/usr/bin/env bash
    set -euo pipefail

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    echo "✅ Extension built and linked"
    echo ""

    # Build extensions (daw-reaper is NOT an extension - DAW services are in the host)
    echo "📦 Building extensions..."
    cargo build -p daw-proto -p daw-test -p hello-world -p http-gateway -p socket-gateway
    echo "✅ Extensions built:"
    echo "   - target/debug/daw-test-extension"
    echo "   - target/debug/hello-world-extension"
    echo "   - target/debug/http-gateway-extension"
    echo "   - target/debug/socket-gateway-extension"
    echo ""

    # Determine Extensions/FTS directory relative to REAPER
    REAPER_EXECUTABLE="${REAPER_EXECUTABLE:-/Users/codywright/Music/FTS-REAPER/FTS-LIVE.app/Contents/MacOS/REAPER}"
    if [[ ! -f "$REAPER_EXECUTABLE" ]]; then
        echo "❌ REAPER executable not found: $REAPER_EXECUTABLE"
        echo "💡 Set REAPER_EXECUTABLE environment variable to the correct path"
        exit 1
    fi

    # Calculate path to Extensions/FTS
    # REAPER_EXECUTABLE: /path/to/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app/Contents/MacOS/REAPER
    # APP_DIR:           /path/to/FastTrackStudio/Reaper/FTS-TRACKS/FTS-LIVE.app
    # RESOURCE_DIR:      /path/to/FastTrackStudio/Reaper/FTS-TRACKS (REAPER resource dir)
    # PARENT:            /path/to/FastTrackStudio/Reaper
    # GRANDPARENT:       /path/to/FastTrackStudio
    # EXTENSIONS_DIR:    /path/to/FastTrackStudio/Extensions/FTS
    APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
    RESOURCE_DIR="$(dirname "$APP_DIR")"
    PARENT="$(dirname "$RESOURCE_DIR")"
    GRANDPARENT="$(dirname "$PARENT")"
    EXTENSIONS_DIR="$GRANDPARENT/Extensions/FTS"

    echo "📁 Extensions directory: $EXTENSIONS_DIR"

    # Create Extensions/FTS directory
    mkdir -p "$EXTENSIONS_DIR"

    # Clean up old symlinks from previous versions
    for old_name in "hello-world-plugin" "hello-world-extension" "daw-reaper-extension"; do
        OLD_TARGET="$EXTENSIONS_DIR/$old_name"
        if [[ -L "$OLD_TARGET" ]] || [[ -f "$OLD_TARGET" ]]; then
            rm -f "$OLD_TARGET"
            echo "🗑️  Removed old symlink: $OLD_TARGET"
        fi
    done

    # Symlink new extensions
    # Note: daw-reaper is NOT an extension - DAW services are implemented in the host
    echo ""
    echo "🔗 Creating symlinks for extensions..."

    for extension in "daw-test-extension" "hello-world-extension" "http-gateway-extension" "socket-gateway-extension"; do
        SOURCE="$(pwd)/target/debug/$extension"
        TARGET="$EXTENSIONS_DIR/$extension"

        # Remove old symlink if exists
        if [[ -L "$TARGET" ]] || [[ -f "$TARGET" ]]; then
            rm -f "$TARGET"
        fi

        # Create new symlink
        ln -s "$SOURCE" "$TARGET"
        echo "  ✅ $extension -> $TARGET"
    done

    echo ""
    echo "💡 For remote control, run 'just control-desktop' in a separate terminal"
    echo ""

    echo "🚀 Launching REAPER..."
    echo "📋 Logs will appear below. Press Ctrl+C to stop REAPER."
    echo ""

    # Run the executable directly to see stdout/stderr in terminal
    # Change to the app's Resources directory so REAPER can find its resources
    cd "$APP_DIR/Contents/Resources" || exit 1
    exec "$REAPER_EXECUTABLE"

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

# ============================================================================
# Audio Plugin Development
# ============================================================================

# Plugin configuration - loaded from .env or uses OS-appropriate defaults
# These are set as justfile variables but can be overridden by environment variables

# Force rebuild of nih-plug submodule (use when local changes aren't being picked up)
rebuild-nih-plug:
    cargo clean -p nih_plug -p nih_plug_dioxus
    @echo "Cleaned nih_plug packages - next build will recompile"

# Bundle an audio plugin (CLAP/VST3)
bundle PLUGIN:
    #!/usr/bin/env bash
    set -euo pipefail
    # Touch submodule Cargo.toml files to ensure cargo detects changes
    touch libs/nih-plug/Cargo.toml
    touch libs/nih-plug/nih_plug_dioxus/Cargo.toml 2>/dev/null || true
    cargo xtask bundle {{PLUGIN}} --release

# Bundle plugin in debug mode
bundle-debug PLUGIN:
    cargo xtask bundle {{PLUGIN}}

# Install a bundled plugin to user plugin directories (no sudo required)
# Only installs CLAP version by default
install-plugin PLUGIN: (bundle PLUGIN)
    #!/usr/bin/env bash
    set -euo pipefail

    # Load .env file if it exists
    JUSTFILE_DIR="{{justfile_directory()}}"
    if [ -f "$JUSTFILE_DIR/.env" ]; then set -a; source "$JUSTFILE_DIR/.env"; set +a; fi

    PLUGIN_NAME="{{PLUGIN}}"
    BUNDLE_DIR="$JUSTFILE_DIR/target/bundled"

    # Determine CLAP directory with OS-appropriate default
    if [[ -z "${CLAP_DIR:-}" ]]; then
        case "$(uname -s)" in
            Darwin) CLAP_DIR="$HOME/Library/Audio/Plug-Ins/CLAP" ;;
            Linux)  CLAP_DIR="$HOME/.clap" ;;
            *)      CLAP_DIR="$HOME/.clap" ;;
        esac
    fi

    # Ensure directory exists
    mkdir -p "$CLAP_DIR"

    # Install CLAP only (handle both file on Linux and directory on macOS)
    if [ -e "$BUNDLE_DIR/${PLUGIN_NAME}.clap" ]; then
        echo "Installing CLAP plugin..."
        rm -rf "$CLAP_DIR/${PLUGIN_NAME}.clap"
        cp -r "$BUNDLE_DIR/${PLUGIN_NAME}.clap" "$CLAP_DIR/"
        echo "CLAP installed to: $CLAP_DIR/${PLUGIN_NAME}.clap"
    else
        echo "Error: CLAP bundle not found at $BUNDLE_DIR/${PLUGIN_NAME}.clap"
        exit 1
    fi

# Install both CLAP and VST3 versions of a plugin
install-plugin-all PLUGIN: (bundle PLUGIN)
    #!/usr/bin/env bash
    set -euo pipefail

    # Load .env file if it exists
    JUSTFILE_DIR="{{justfile_directory()}}"
    if [ -f "$JUSTFILE_DIR/.env" ]; then set -a; source "$JUSTFILE_DIR/.env"; set +a; fi

    PLUGIN_NAME="{{PLUGIN}}"
    BUNDLE_DIR="$JUSTFILE_DIR/target/bundled"

    # Determine plugin directories with OS-appropriate defaults
    if [[ -z "${CLAP_DIR:-}" ]]; then
        case "$(uname -s)" in
            Darwin) CLAP_DIR="$HOME/Library/Audio/Plug-Ins/CLAP" ;;
            Linux)  CLAP_DIR="$HOME/.clap" ;;
            *)      CLAP_DIR="$HOME/.clap" ;;
        esac
    fi
    if [[ -z "${VST3_DIR:-}" ]]; then
        case "$(uname -s)" in
            Darwin) VST3_DIR="$HOME/Library/Audio/Plug-Ins/VST3" ;;
            Linux)  VST3_DIR="$HOME/.vst3" ;;
            *)      VST3_DIR="$HOME/.vst3" ;;
        esac
    fi

    # Ensure directories exist
    mkdir -p "$CLAP_DIR"
    mkdir -p "$VST3_DIR"

    # Install CLAP (handle both file on Linux and directory on macOS)
    if [ -e "$BUNDLE_DIR/${PLUGIN_NAME}.clap" ]; then
        echo "Installing CLAP plugin..."
        rm -rf "$CLAP_DIR/${PLUGIN_NAME}.clap"
        cp -r "$BUNDLE_DIR/${PLUGIN_NAME}.clap" "$CLAP_DIR/"
        echo "CLAP installed to: $CLAP_DIR/${PLUGIN_NAME}.clap"
    fi

    # Install VST3 (handle both file on Linux and directory on macOS)
    if [ -e "$BUNDLE_DIR/${PLUGIN_NAME}.vst3" ]; then
        echo "Installing VST3 plugin..."
        rm -rf "$VST3_DIR/${PLUGIN_NAME}.vst3"
        cp -r "$BUNDLE_DIR/${PLUGIN_NAME}.vst3" "$VST3_DIR/"
        echo "VST3 installed to: $VST3_DIR/${PLUGIN_NAME}.vst3"
    fi

# Uninstall a plugin from user plugin directories
uninstall-plugin PLUGIN:
    #!/usr/bin/env bash
    set -euo pipefail

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    PLUGIN_NAME="{{PLUGIN}}"

    # Determine plugin directories with OS-appropriate defaults
    if [[ -z "${CLAP_DIR:-}" ]]; then
        case "$(uname -s)" in
            Darwin) CLAP_DIR="$HOME/Library/Audio/Plug-Ins/CLAP" ;;
            Linux)  CLAP_DIR="$HOME/.clap" ;;
            *)      CLAP_DIR="$HOME/.clap" ;;
        esac
    fi
    if [[ -z "${VST3_DIR:-}" ]]; then
        case "$(uname -s)" in
            Darwin) VST3_DIR="$HOME/Library/Audio/Plug-Ins/VST3" ;;
            Linux)  VST3_DIR="$HOME/.vst3" ;;
            *)      VST3_DIR="$HOME/.vst3" ;;
        esac
    fi

    if [ -d "$CLAP_DIR/${PLUGIN_NAME}.clap" ]; then
        rm -rf "$CLAP_DIR/${PLUGIN_NAME}.clap"
        echo "Removed CLAP: $CLAP_DIR/${PLUGIN_NAME}.clap"
    fi

    if [ -d "$VST3_DIR/${PLUGIN_NAME}.vst3" ]; then
        rm -rf "$VST3_DIR/${PLUGIN_NAME}.vst3"
        echo "Removed VST3: $VST3_DIR/${PLUGIN_NAME}.vst3"
    fi

# Launch REAPER with NIH plugin logging enabled
reaper-plugin:
    #!/usr/bin/env bash
    set -euo pipefail

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    # Determine NIH log file with OS-appropriate default
    if [[ -z "${NIH_LOG_FILE:-}" ]]; then
        case "$(uname -s)" in
            Darwin) NIH_LOG_FILE="$HOME/Library/Logs/REAPER/nih.log" ;;
            Linux)  NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
            *)      NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
        esac
    fi

    if [[ -z "${REAPER_EXECUTABLE:-}" ]]; then
        echo "Error: REAPER_EXECUTABLE not set in .env"
        exit 1
    fi

    mkdir -p "$(dirname "$NIH_LOG_FILE")"
    echo "Launching REAPER with NIH logging..."
    echo "Logs: $NIH_LOG_FILE"

    # On macOS, change to app Resources directory
    if [[ "$(uname -s)" == "Darwin" ]]; then
        APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
        cd "$APP_DIR/Contents/Resources" || exit 1
    fi
    NIH_LOG="$NIH_LOG_FILE" exec "$REAPER_EXECUTABLE"

# Launch REAPER with WGPU debug logging
reaper-plugin-debug:
    #!/usr/bin/env bash
    set -euo pipefail

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    # Determine NIH log file with OS-appropriate default
    if [[ -z "${NIH_LOG_FILE:-}" ]]; then
        case "$(uname -s)" in
            Darwin) NIH_LOG_FILE="$HOME/Library/Logs/REAPER/nih.log" ;;
            Linux)  NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
            *)      NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
        esac
    fi

    if [[ -z "${REAPER_EXECUTABLE:-}" ]]; then
        echo "Error: REAPER_EXECUTABLE not set in .env"
        exit 1
    fi

    mkdir -p "$(dirname "$NIH_LOG_FILE")"
    echo "Launching REAPER with WGPU debug logging..."
    echo "Logs: $NIH_LOG_FILE"

    # On macOS, change to app Resources directory
    if [[ "$(uname -s)" == "Darwin" ]]; then
        APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
        cd "$APP_DIR/Contents/Resources" || exit 1
    fi
    MTL_HUD_ENABLED=1 RUST_LOG=wgpu_core=debug,wgpu_hal=debug NIH_LOG="$NIH_LOG_FILE" exec "$REAPER_EXECUTABLE"

# Plugin development workflow: build, install, launch REAPER with logs
dev PLUGIN: (install-plugin PLUGIN)
    #!/usr/bin/env bash
    set -euo pipefail

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    # Determine NIH log file with OS-appropriate default
    if [[ -z "${NIH_LOG_FILE:-}" ]]; then
        case "$(uname -s)" in
            Darwin) NIH_LOG_FILE="$HOME/Library/Logs/REAPER/nih.log" ;;
            Linux)  NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
            *)      NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
        esac
    fi

    if [[ -z "${REAPER_EXECUTABLE:-}" ]]; then
        echo "Error: REAPER_EXECUTABLE not set in .env"
        exit 1
    fi

    mkdir -p "$(dirname "$NIH_LOG_FILE")"

    echo ""
    echo "Starting plugin development session for: {{PLUGIN}}"
    echo "================================================"
    echo ""

    # Check if we're in tmux
    if [ -n "${TMUX:-}" ]; then
        echo "Detected tmux - splitting panes..."
        echo ""

        # Build the REAPER launch command based on OS
        if [[ "$(uname -s)" == "Darwin" ]]; then
            APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
            REAPER_CMD="cd \"$APP_DIR/Contents/Resources\" && NIH_LOG=\"$NIH_LOG_FILE\" \"$REAPER_EXECUTABLE\""
        else
            REAPER_CMD="NIH_LOG=\"$NIH_LOG_FILE\" \"$REAPER_EXECUTABLE\""
        fi

        # Split horizontally, run REAPER in new pane on right
        tmux split-window -h "$REAPER_CMD"

        echo "REAPER launched in right pane"
        echo "Use Ctrl+B then arrow keys to switch panes"
        echo ""
        echo "Tailing logs..."
        echo ""

        # Wait for log file and tail it
        while [ ! -f "$NIH_LOG_FILE" ]; do sleep 0.5; done
        tail -f "$NIH_LOG_FILE"
    else
        echo "Launching REAPER..."
        echo "Logs: $NIH_LOG_FILE"
        echo ""
        echo "Tip: Run in tmux for split-pane log viewing"
        echo ""

        # On macOS, change to app Resources directory
        if [[ "$(uname -s)" == "Darwin" ]]; then
            APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
            cd "$APP_DIR/Contents/Resources" || exit 1
        fi
        NIH_LOG="$NIH_LOG_FILE" exec "$REAPER_EXECUTABLE"
    fi

# Launch REAPER with logs (no rebuild/install) - same as dev but skip build
# Plugin name is optional (all plugins share the same log file)
run PLUGIN="":
    #!/usr/bin/env bash
    set -euo pipefail

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    # Determine NIH log file with OS-appropriate default
    if [[ -z "${NIH_LOG_FILE:-}" ]]; then
        case "$(uname -s)" in
            Darwin) NIH_LOG_FILE="$HOME/Library/Logs/REAPER/nih.log" ;;
            Linux)  NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
            *)      NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
        esac
    fi

    if [[ -z "${REAPER_EXECUTABLE:-}" ]]; then
        echo "Error: REAPER_EXECUTABLE not set in .env"
        exit 1
    fi

    mkdir -p "$(dirname "$NIH_LOG_FILE")"

    # Clear old logs
    rm -f "$NIH_LOG_FILE"

    # Check if we're in tmux
    if [ -n "${TMUX:-}" ]; then
        echo "Launching REAPER in tmux split..."

        # Build the REAPER launch command based on OS
        if [[ "$(uname -s)" == "Darwin" ]]; then
            APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
            REAPER_CMD="cd \"$APP_DIR/Contents/Resources\" && NIH_LOG=\"$NIH_LOG_FILE\" \"$REAPER_EXECUTABLE\""
        else
            REAPER_CMD="NIH_LOG=\"$NIH_LOG_FILE\" \"$REAPER_EXECUTABLE\""
        fi

        # Split horizontally, run REAPER in new pane on right
        tmux split-window -h "$REAPER_CMD"

        echo "REAPER launched in right pane"
        echo "Tailing logs..."
        echo ""

        # Wait for log file and tail it
        while [ ! -f "$NIH_LOG_FILE" ]; do sleep 0.5; done
        tail -f "$NIH_LOG_FILE"
    else
        echo "Launching REAPER..."
        echo "Logs: $NIH_LOG_FILE"
        echo "Tip: Run in tmux for split-pane log viewing"
        echo ""

        # On macOS, change to app Resources directory
        if [[ "$(uname -s)" == "Darwin" ]]; then
            APP_DIR="$(dirname "$(dirname "$(dirname "$REAPER_EXECUTABLE")")")"
            cd "$APP_DIR/Contents/Resources" || exit 1
        fi
        NIH_LOG="$NIH_LOG_FILE" exec "$REAPER_EXECUTABLE"
    fi

# Tail plugin logs
plugin-logs:
    #!/usr/bin/env bash

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    # Determine NIH log file with OS-appropriate default
    if [[ -z "${NIH_LOG_FILE:-}" ]]; then
        case "$(uname -s)" in
            Darwin) NIH_LOG_FILE="$HOME/Library/Logs/REAPER/nih.log" ;;
            Linux)  NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
            *)      NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
        esac
    fi

    if [ -f "$NIH_LOG_FILE" ]; then
        tail -f "$NIH_LOG_FILE"
    else
        echo "Waiting for log file: $NIH_LOG_FILE"
        while [ ! -f "$NIH_LOG_FILE" ]; do sleep 0.5; done
        tail -f "$NIH_LOG_FILE"
    fi

# Clear plugin logs
clear-plugin-logs:
    #!/usr/bin/env bash

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    # Determine NIH log file with OS-appropriate default
    if [[ -z "${NIH_LOG_FILE:-}" ]]; then
        case "$(uname -s)" in
            Darwin) NIH_LOG_FILE="$HOME/Library/Logs/REAPER/nih.log" ;;
            Linux)  NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
            *)      NIH_LOG_FILE="$HOME/.local/share/REAPER/Logs/nih.log" ;;
        esac
    fi

    if [ -f "$NIH_LOG_FILE" ]; then
        rm "$NIH_LOG_FILE"
        echo "Cleared: $NIH_LOG_FILE"
    else
        echo "No log file to clear"
    fi

# List available plugins
list-plugins:
    #!/usr/bin/env bash
    echo "Available plugins:"
    echo ""
    for dir in apps/plugins/*/; do
        if [ -f "${dir}Cargo.toml" ]; then
            plugin_name=$(basename "$dir")
            echo "  - $plugin_name"
        fi
    done

# Show plugin info
plugin-info PLUGIN:
    #!/usr/bin/env bash

    # Load .env file if it exists
    if [ -f .env ]; then set -a; source .env; set +a; fi

    # Determine plugin directories with OS-appropriate defaults
    if [[ -z "${CLAP_DIR:-}" ]]; then
        case "$(uname -s)" in
            Darwin) CLAP_DIR="$HOME/Library/Audio/Plug-Ins/CLAP" ;;
            Linux)  CLAP_DIR="$HOME/.clap" ;;
            *)      CLAP_DIR="$HOME/.clap" ;;
        esac
    fi
    if [[ -z "${VST3_DIR:-}" ]]; then
        case "$(uname -s)" in
            Darwin) VST3_DIR="$HOME/Library/Audio/Plug-Ins/VST3" ;;
            Linux)  VST3_DIR="$HOME/.vst3" ;;
            *)      VST3_DIR="$HOME/.vst3" ;;
        esac
    fi

    echo "Plugin: {{PLUGIN}}"
    echo ""
    echo "Source: apps/plugins/{{PLUGIN}}/"
    echo ""
    echo "Installed CLAP:"
    if [ -d "$CLAP_DIR/{{PLUGIN}}.clap" ]; then
        ls -la "$CLAP_DIR/{{PLUGIN}}.clap"
    else
        echo "  (not installed)"
    fi
    echo ""
    echo "Installed VST3:"
    if [ -d "$VST3_DIR/{{PLUGIN}}.vst3" ]; then
        ls -la "$VST3_DIR/{{PLUGIN}}.vst3"
    else
        echo "  (not installed)"
    fi

# Show help
help:
    #!/usr/bin/env bash
    echo "FastTrackStudio Justfile Commands"
    echo ""
    echo "Audio Plugin Development:"
    echo "  just dev <plugin>        Build, install, and launch REAPER with log tailing"
    echo "  just bundle <plugin>     Bundle plugin (CLAP/VST3) in release mode"
    echo "  just bundle-debug <plugin>  Bundle plugin in debug mode"
    echo "  just install-plugin <plugin>  Install bundled plugin to system"
    echo "  just uninstall-plugin <plugin>  Remove plugin from system"
    echo "  just list-plugins        List available plugins"
    echo "  just plugin-info <plugin>  Show plugin installation status"
    echo "  just plugin-logs         Tail NIH plugin logs"
    echo "  just clear-plugin-logs   Clear log file"
    echo "  just reaper-plugin       Launch REAPER with NIH logging"
    echo "  just reaper-plugin-debug Launch REAPER with WGPU debug logging"
    echo ""
    echo "REAPER Extension:"
    echo "  build-extension      Build the REAPER extension"
    echo "  install-extension    Install extension to REAPER (copies file)"
    echo "  link-extension       Link extension to REAPER (symlink for dev)"
    echo "  uninstall-extension  Remove extension from REAPER"
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
    echo "Examples:"
    echo "  just dev wgpu_reaper_demo     # Full dev cycle for plugin"
    echo "  just bundle wgpu_reaper_demo  # Just build the plugin"
    echo "  just list-plugins             # See available plugins"
    echo ""
    echo "Ralphy + Task Master:"
    echo "  just ralphy <task>  Run single task with Ralphy"
    echo "  just ralphy-prd     Run Ralphy on Task Master PRD"
    echo "  just ralphy-tasks   Export Task Master tasks and run with Ralphy"
    echo "  just ralphy-export  Export Task Master tasks to Ralphy format"
    echo "  just ralphy-pr      Run with branch-per-task and PR creation"
    echo "  just ralphy-dry     Dry-run to see what would happen"
    echo "  just ralphy-config  Show Ralphy configuration"
    echo ""
    echo "Git Worktrees:"
    echo "  worktree-setup      Setup submodule symlinks for a git worktree"
    echo "  worktree-check      Verify worktree submodule symlinks"
    echo ""
    echo "Configuration:"
    echo "  .env file           Create .env from .env.example and customize paths"
    echo "  Environment vars    Can override .env: REAPER_PATH=/path just <command>"

# ============================================================================
# Git Worktree Setup
# ============================================================================

# Setup submodule symlinks for a git worktree
# Git worktrees don't automatically get submodule contents, so we symlink from main repo
worktree-setup:
    #!/usr/bin/env bash
    set -euo pipefail

    # Find the main repo (first worktree in the list, usually the original clone)
    MAIN_REPO=$(git worktree list | head -1 | awk '{print $1}')
    CURRENT_DIR=$(pwd)

    if [[ "$MAIN_REPO" == "$CURRENT_DIR" ]]; then
        echo "⚠️  You're in the main repo, not a worktree. No symlinks needed."
        exit 0
    fi

    echo "🔗 Setting up worktree symlinks..."
    echo "   Main repo: $MAIN_REPO"
    echo "   Worktree:  $CURRENT_DIR"
    echo ""

    # List of submodules/directories to symlink
    # These are paths relative to the repo root
    SUBMODULES=(
        "libs/nih-plug"
        "libs/lumen-blocks"
        "libs/vendor/anyrender"
        "libs/vendor/baseview"
        "libs/vendor/blitz"
        "libs/vendor/reaper-rs"
        "libs/vendor/stylo"
        "libs/reference/sheet-music/musescore"
    )

    for submodule in "${SUBMODULES[@]}"; do
        SOURCE="$MAIN_REPO/$submodule"
        TARGET="$CURRENT_DIR/$submodule"

        # Check if source exists in main repo
        if [[ ! -d "$SOURCE" ]]; then
            echo "⚠️  Skipping $submodule (not found in main repo)"
            continue
        fi

        # Create parent directory if needed
        mkdir -p "$(dirname "$TARGET")"

        # Remove existing directory/symlink
        if [[ -L "$TARGET" ]]; then
            echo "   Updating symlink: $submodule"
            rm "$TARGET"
        elif [[ -d "$TARGET" ]]; then
            # Check if it's empty (git submodule placeholder)
            if [[ -z "$(ls -A "$TARGET" 2>/dev/null)" ]]; then
                echo "   Replacing empty dir: $submodule"
                rmdir "$TARGET"
            else
                echo "⚠️  Skipping $submodule (directory not empty)"
                continue
            fi
        else
            echo "   Creating symlink: $submodule"
        fi

        # Create symlink
        ln -s "$SOURCE" "$TARGET"
    done

    echo ""
    echo "✅ Worktree setup complete!"
    echo ""
    echo "You can now run:"
    echo "   cargo check -p web"
    echo "   dx serve --platform web"

# Verify worktree submodule symlinks are working
worktree-check:
    #!/usr/bin/env bash
    set -euo pipefail

    echo "🔍 Checking worktree submodule symlinks..."
    echo ""

    SUBMODULES=(
        "libs/nih-plug"
        "libs/lumen-blocks"
        "libs/vendor/anyrender"
        "libs/vendor/baseview"
        "libs/vendor/blitz"
        "libs/vendor/reaper-rs"
        "libs/vendor/stylo"
        "libs/reference/sheet-music/musescore"
    )

    ALL_OK=true
    for submodule in "${SUBMODULES[@]}"; do
        if [[ -L "$submodule" ]]; then
            if [[ -d "$submodule" ]]; then
                echo "✅ $submodule (symlink, valid)"
            else
                echo "❌ $submodule (symlink, broken)"
                ALL_OK=false
            fi
        elif [[ -d "$submodule" ]]; then
            if [[ -f "$submodule/Cargo.toml" ]] || [[ -d "$submodule/fonts" ]]; then
                echo "✅ $submodule (directory, has content)"
            else
                echo "⚠️  $submodule (directory, possibly empty)"
            fi
        else
            echo "❌ $submodule (missing)"
            ALL_OK=false
        fi
    done

    echo ""
    if [[ "$ALL_OK" == "true" ]]; then
        echo "✅ All submodules look good!"
    else
        echo "⚠️  Some submodules need attention. Run 'just worktree-setup' to fix."
    fi

# ============================================================================
# Web App Development
# ============================================================================

# Build Tailwind CSS for the web app
web-css:
    cd apps/web && bunx @tailwindcss/cli --input tailwind.css --output assets/tailwind.css

# Watch and rebuild Tailwind CSS on changes
web-css-watch:
    cd apps/web && bunx @tailwindcss/cli --input tailwind.css --output assets/tailwind.css --watch

# Run the web app dev server (builds CSS first)
web: web-css
    cd apps/web && dx serve

# Run web dev server with CSS watcher in parallel
web-dev:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Starting Tailwind CSS watcher and Dioxus dev server..."
    echo "Press Ctrl+C to stop both"
    # Run CSS watcher in background
    (cd apps/web && bunx @tailwindcss/cli --input tailwind.css --output assets/tailwind.css --watch) &
    CSS_PID=$!
    # Run dx serve in foreground
    trap "kill $CSS_PID 2>/dev/null" EXIT
    cd apps/web && dx serve

# ============================================================================
# Control App Development (ROAM HTTP Bridge)
# ============================================================================

# Build Tailwind CSS for the control-web app
control-web-css:
    cd apps/control-web && bunx @tailwindcss/cli --input tailwind.css --output assets/tailwind.css

# Run the control-web app dev server (builds CSS first, network accessible)
control-web: control-web-css
    cd apps/control-web && dx serve --addr 0.0.0.0 --port 9250

# Run control-web dev server with CSS watcher in parallel (network accessible)
control-web-dev:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "Starting Tailwind CSS watcher and Dioxus dev server..."
    echo "Server will be available on the network at port 9250"
    echo "Press Ctrl+C to stop both"
    # Run CSS watcher in background
    (cd apps/control-web && bunx @tailwindcss/cli --input tailwind.css --output assets/tailwind.css --watch) &
    CSS_PID=$!
    # Run dx serve in foreground, bound to all interfaces
    trap "kill $CSS_PID 2>/dev/null" EXIT
    cd apps/control-web && dx serve --addr 0.0.0.0 --port 9250

# Build control-web app (release mode)
build-control-web:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📦 Building control-web app (release)..."
    cd apps/control-web
    bunx @tailwindcss/cli --input tailwind.css --output assets/tailwind.css
    dx build --release
    echo "✅ Control-web built at: target/dx/control-web/release/web/public"

# Build control-web app in debug mode (faster, larger WASM)
build-control-web-debug:
    #!/usr/bin/env bash
    set -euo pipefail
    echo "📦 Building control-web app (debug)..."
    cd apps/control-web
    bunx @tailwindcss/cli --input tailwind.css --output assets/tailwind.css
    dx build
    echo "✅ Control-web built at: target/dx/control-web/debug/web/public"

# Run control-desktop dev server (hot reload, no prebuild)
# For prototyping the desktop UI independently
control-desktop-dev:
    cd apps/control-desktop && dx serve

# Run control-desktop with control-web embedded
# Builds control-web first, then runs desktop app
control-desktop: build-control-web-debug
    cd apps/control-desktop && dx serve

# Build control-desktop for release
build-control-desktop:
    cargo build -p control-desktop --release

# Build everything for control (web UI + desktop app)
build-control: build-control-web-debug
    echo "✅ Control-web built at: target/dx/control-web/debug/web/public"
    echo ""
    echo "Run 'just control-desktop' to start the desktop app with embedded server"

# Development: Quick reference for control app development
control-dev:
    #!/usr/bin/env bash
    echo "╔════════════════════════════════════════════════════════════╗"
    echo "║  Control App Development                                   ║"
    echo "╠════════════════════════════════════════════════════════════╣"
    echo "║                                                            ║"
    echo "║  Web UI only (hot reload, network accessible):             ║"
    echo "║    just control-web-dev                                    ║"
    echo "║    → http://0.0.0.0:9250 (access from phone/tablet)        ║"
    echo "║                                                            ║"
    echo "║  Desktop UI only (hot reload):                             ║"
    echo "║    just control-desktop-dev                                ║"
    echo "║    → Desktop window opens, serves web on :9251             ║"
    echo "║                                                            ║"
    echo "║  Both in parallel:                                         ║"
    echo "║    Terminal 1: just control-web-dev     (port 9250)        ║"
    echo "║    Terminal 2: just control-desktop-dev (port 9251)        ║"
    echo "║                                                            ║"
    echo "╚════════════════════════════════════════════════════════════╝"

# Full control app test: build web UI and run desktop app
test-control: build-control-web-debug
    #!/usr/bin/env bash
    set -euo pipefail
    echo "✅ Control-web built"
    echo ""
    echo "Starting control-desktop..."
    echo "  - Desktop UI will open"
    echo "  - Web UI available at http://localhost:3000"
    echo ""
    cd apps/control-desktop && dx serve

# ============================================================================
# Ralphy + Task Master Integration
# ============================================================================

# Export Task Master tasks to Ralphy-compatible markdown
ralphy-export:
    .ralphy/taskmaster-to-ralphy.sh

# Run Ralphy on Task Master PRD (create .taskmaster/docs/prd.md first)
ralphy-prd:
    ralphy --prd .taskmaster/docs/prd.md

# Run Ralphy on exported tasks from Task Master
ralphy-tasks: ralphy-export
    ralphy --prd .ralphy/tasks.md

# Run a single task with Ralphy
ralphy TASK:
    ralphy "{{TASK}}"

# Run Ralphy in fast mode (skip tests/lint)
ralphy-fast TASK:
    ralphy --fast "{{TASK}}"

# Run Ralphy with branch-per-task and PR creation
ralphy-pr:
    ralphy --prd .taskmaster/docs/prd.md --branch-per-task --create-pr

# Dry-run Ralphy to see what would happen
ralphy-dry:
    ralphy --dry-run --prd .taskmaster/docs/prd.md

# Show Ralphy configuration
ralphy-config:
    ralphy --config
