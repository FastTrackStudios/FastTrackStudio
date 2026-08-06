# REAPER extension (apps/extensions/reaper-fts-extensions) — `just reaper …`
#
# The production REAPER extension stack (cdylib reaper_fts_extensions).
# Dev installs symlink the build + live-editable config into REAPER's
# resource dir; release packaging (root `just dist`) copies the .so into
# the tarball.

reaper_home    := env("REAPER_HOME", home_directory() / "fts-dev")
reaper_plugins := env("REAPER_PLUGINS", reaper_home / "UserPlugins")
fts_config     := reaper_home / "fasttrackstudio"
lib_name       := "reaper_fts_extensions"
# Extra cargo features on top of defaults (mod-launcher, mod-session,
# mod-sync, mod-input, mod-mirror, ui-dock, poll-broadcast, host-hooks).
# Override with the env var, e.g.  FTS_EXT_FEATURES=mod-mirror just reaper build
features := env("FTS_EXT_FEATURES", "")

# List the reaper recipes
[private]
default:
    @just --list reaper

# Build the REAPER extension (release)
build:
    cargo build --release -p fts-extensions --features "{{features}}"

# Build with NO default features (bisection mode)
build-minimal:
    cargo build --release -p fts-extensions --no-default-features --features "{{features}}"

# Build + symlink the extension and its config into $REAPER_HOME
install: build install-config
    mkdir -p {{reaper_plugins}}
    ln -sf "{{justfile_directory()}}/target/release/lib{{lib_name}}.so" "{{reaper_plugins}}/{{lib_name}}.so"
    @echo "Symlinked -> {{reaper_plugins}}/{{lib_name}}.so"

# Remove the extension symlink
uninstall:
    rm -f "{{reaper_plugins}}/{{lib_name}}.so"
    @echo "Removed {{reaper_plugins}}/{{lib_name}}.so"

# In-tree sources: reaper-input keybinds/workflows + fts-launcher packs.
# Live-editable: save a .styx and the extension hot-reloads it.
# Symlink module config into $REAPER_HOME/fasttrackstudio
install-config:
    #!/usr/bin/env bash
    set -euo pipefail
    input_cfg="{{justfile_directory()}}/features/reaper/reaper-input/config/config"
    packs="{{justfile_directory()}}/features/launcher/fts-launcher/packs"
    echo "Installing config symlinks -> {{fts_config}}/"
    mkdir -p "{{fts_config}}/input/keybinds"
    for p in fasttrackstudio logic reaper pro-tools ableton overlays; do
        ln -sfn "$input_cfg/$p" "{{fts_config}}/input/keybinds/$p"
    done
    rm -rf "{{fts_config}}/input/workflows"
    ln -sfn "$input_cfg/workflows" "{{fts_config}}/input/workflows"
    mkdir -p "{{fts_config}}/launcher/packs"
    ln -sfn "$packs/reaper-core"       "{{fts_config}}/launcher/packs/reaper-core"
    ln -sfn "$packs/reaper-visibility" "{{fts_config}}/launcher/packs/reaper-visibility"
    echo "Done."

# Remove the config symlinks
uninstall-config:
    #!/usr/bin/env bash
    for p in fasttrackstudio logic reaper pro-tools ableton overlays; do
        rm -f "{{fts_config}}/input/keybinds/$p"
    done
    rm -f "{{fts_config}}/input/workflows"
    rm -f "{{fts_config}}/launcher/packs/reaper-core"
    rm -f "{{fts_config}}/launcher/packs/reaper-visibility"
    @echo "Removed config symlinks from {{fts_config}}/"

# Tail the extension log (live)
log:
    tail -f "$(ls -t {{home_directory()}}/.local/state/fasttrackstudio/reaper-fts-extensions.log.* | head -n 1)"

# ── Integration tests ──────────────────────────────────────────────────
# Needs a REAPER install; FTS_REAPER_EXECUTABLE/RESOURCES point the
# harness at it (wrapper script keeps the test profile isolated).

_wrapper := justfile_directory() / "apps/extensions/reaper-fts-extensions/scripts/reaper-test-wrapper.sh"

# Run integration tests headless (no GUI)
integration-test *args:
    FTS_REAPER_EXECUTABLE="{{_wrapper}}" \
    FTS_REAPER_RESOURCES="{{reaper_home}}" \
    cargo run -p fts-extensions-xtask -- {{args}}

# Integration tests with a visible REAPER window
integration-test-gui *args:
    FTS_REAPER_EXECUTABLE="{{_wrapper}}" \
    FTS_REAPER_RESOURCES="{{reaper_home}}" \
    cargo run -p fts-extensions-xtask -- --gui {{args}}

# daw-domain REAPER integration tests (daw-bridge host, isolated rig
# under target/fts-reaper-test): transport / tempo map / markers /
# items / takes / routing / ext-state / action registry against a
# live headless REAPER.
daw-test *args:
    FTS_REAPER_EXECUTABLE="{{_wrapper}}" \
    cargo run -p daw-reaper-xtask -- {{args}}

# Same, with a VISIBLE REAPER window, left open afterwards so you can
# look at what the test did. Watch one test rather than the suite:
#
#   just reaper daw-test-watch chord_notes_survive_the_round_trip
daw-test-watch *args:
    FTS_REAPER_EXECUTABLE="{{_wrapper}}" \
    cargo run -p daw-reaper-xtask -- --gui --keep-open {{args}}
