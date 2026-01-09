# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

FastTrackStudio is a music production toolset built in Rust, integrating with REAPER DAW. It consists of:
- A REAPER extension for DAW integration and real-time state synchronization
- A cross-platform desktop/mobile/web app using Dioxus
- Audio plugins (CLAP/VST3) using nih-plug
- A chart parser for music notation
- P2P networking via iroh for real-time collaboration

## Development Environment (NixOS/Nix)

```bash
# Enter the devshell (provides all dependencies)
nix develop

# Or with direnv (auto-loads when entering directory)
direnv allow
```

The `flake.nix` provides: Rust toolchain, fontconfig, ALSA, JACK, X11/Wayland, OpenGL, GTK (for file dialogs), and other build dependencies.

## Build Commands

```bash
# General
cargo build                          # Build workspace
cargo test                           # Run all tests
cargo clippy                         # Lint (clippy::all = deny, clippy::pedantic = warn)
cargo fmt                            # Format code

# Using justfile (recommended)
just build                           # Build all packages
just test                            # Run tests
just lint                            # Run clippy
just help                            # Show all available commands

# REAPER Extension
just build-extension                 # Build extension
just install-extension               # Build and symlink to REAPER UserPlugins
just test-reaper                     # Build, link, and launch REAPER with logs

# Audio Plugins
just bundle <plugin>                 # Bundle plugin (CLAP/VST3) in release mode
just install-plugin <plugin>         # Install bundled plugin
just dev <plugin>                    # Full dev cycle: build, install, launch REAPER with logs
just list-plugins                    # List available plugins

# Desktop App (Dioxus)
dx serve --platform desktop          # Run desktop app in dev mode

# xtask commands
cargo xtask ci                       # Run full CI pipeline
cargo xtask generate-types           # Generate TypeScript types from Rust
cargo xtask test --package <name>    # Run tests for specific package
```

## Architecture

### Directory Structure
- `apps/` - Applications (desktop, mobile, web, reaper_extension, plugins/)
- `modules/` - Core business logic (fts, daw, ui, api, peer-2-peer)
- `libs/` - Reusable libraries (monarchy, nih-plug, swell-ui, pdf)
- `packages/` - Specialized packages (charts, keyflow)
- `xtask/` - Build automation tasks

### Key Modules
- **fts** - Core FastTrackStudio logic: setlists, lyrics, chords, naming conventions. Features: `reaper`, `dioxus`, `rpp`
- **daw** - DAW abstraction layer: transport state, MIDI/OSC routing, instrument management
- **ui** - Shared Dioxus UI components using lumen-blocks
- **peer-2-peer** - iroh-based P2P networking for real-time sync
- **charts** - Music chart parser with smart chord memory and section numbering
- **monarchy** - Metadata-based hierarchical organization system with derive macros
- **keyflow** - Music understanding: chord detection, chart building

### REAPER Extension Features
Conditional compilation via feature flags (see `.env.example`):
- `input` - Key interception and redirection
- `live` - Live tracks functionality
- `lyrics` - Lyrics display
- `iroh_server` - IPC with desktop app
- `keyflow` - Chord detection

Set `EXTENSION_FEATURES=none` in `.env` for transparent extension (no features).

### Tech Stack
- **Rust 2024 edition** with resolver = "3"
- **Dioxus 0.7** for UI (desktop/mobile/web)
- **reaper-rs** for REAPER integration
- **nih-plug** (local submodule at libs/nih-plug) for audio plugins
- **iroh/irpc** for P2P networking
- **Tailwind CSS** (auto-compiled by Dioxus 0.7+)

## Configuration

Copy `.env.example` to `.env` and configure:
- `REAPER_PATH` - REAPER resource directory
- `REAPER_EXECUTABLE` - Path to REAPER binary
- `BUILD_MODE` - debug or release
- `EXTENSION_FEATURES` - Extension feature flags

## Workspace Lints

- `unsafe_code = "forbid"` at workspace level (allowed only in reaper_extension and fts with reaper feature)
- `clippy::all = "deny"`, `clippy::pedantic = "warn"`
- Unused code warnings are allowed (`unused = "allow"`)

## Important Notes

- The nih-plug submodule at `libs/nih-plug` is patched for REAPER embed UI support
- Blitz crates are patched to git versions for latest CSS support
- TypeScript types are generated to `apps/desktop/src/bindings/`
- On macOS, REAPER extensions must NOT have `lib` prefix in filename
