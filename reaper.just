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

# Install the midi-tools toolbar icons and put their buttons on a toolbar.
#
# Two steps that look like one but aren't: `fts-icons --install` writes the
# PNG strips into the resource dir, and `daw toolbar-add` puts the buttons
# on a toolbar. Installing an icon does not create a button, and adding a
# button does not create an icon.
#
# The button half talks to a RUNNING REAPER over the extension's RPC, so
# start REAPER (with the extension loaded) first. The icon half works
# either way.
#
#   just reaper midi-tools-toolbar
#   just reaper midi-tools-toolbar "midi toolbar 1"
midi-tools-toolbar target="floating toolbar 1":
    #!/usr/bin/env bash
    set -euo pipefail
    cd "{{justfile_directory()}}"
    echo "-- icons --"
    cargo run -q -p fts-icons --bin fts-icons -- \
        build features/reaper/fts-icons/examples/midi-tools.toml --install
    echo "-- buttons -> {{target}} --"
    # --workflow midi-tools tags both, so `remove_workflow_buttons` undoes
    # the pair in one go. add_button is idempotent, so re-running this is a
    # refresh rather than a duplicate.
    cargo run -q -p daw --features cli --bin daw -- \
        toolbar-add _FTS_MIDI_VELOCITY "Velocity" \
        --target "{{target}}" --workflow midi-tools --icon fts_midi_velocity.png
    cargo run -q -p daw --features cli --bin daw -- \
        toolbar-add _FTS_MIDI_ARP "Arp" \
        --target "{{target}}" --workflow midi-tools --icon fts_midi_arp.png
    cargo run -q -p daw --features cli --bin daw -- \
        toolbar-live --target "{{target}}"

# ── Theme ──────────────────────────────────────────────────────────────
# The FastTrackStudio REAPER theme lives UNPACKED in-tree at
# features/reaper/fts-theme. REAPER loads unpacked themes straight out of
# ColorThemes/ (a <name>/ folder next to a <name>.ReaperTheme), so the dev
# loop is: symlink once, edit rtconfig.txt / PNGs in the repo, hit
# "Theme development: Reload theme" in REAPER. No zip round-trip.

theme_dir  := justfile_directory() / "features/reaper/fts-theme"
theme_name := "FastTrackStudio"

# Symlink the in-tree theme into $REAPER_HOME/ColorThemes (live-editable)
theme-install:
    #!/usr/bin/env bash
    set -euo pipefail
    dest="{{reaper_home}}/ColorThemes"
    mkdir -p "$dest"
    ln -sfn "{{theme_dir}}/{{theme_name}}"                "$dest/{{theme_name}}"
    ln -sfn "{{theme_dir}}/{{theme_name}}.ReaperTheme"    "$dest/{{theme_name}}.ReaperTheme"
    echo "Symlinked -> $dest/{{theme_name}}{,.ReaperTheme}"
    echo "Pick it in REAPER: Options > Themes > {{theme_name}}"

# Remove the theme symlinks
theme-uninstall:
    rm -f "{{reaper_home}}/ColorThemes/{{theme_name}}" \
          "{{reaper_home}}/ColorThemes/{{theme_name}}.ReaperTheme"
    @echo "Removed {{theme_name}} from {{reaper_home}}/ColorThemes"

# Screenshot a real REAPER wearing the theme (private Xvfb, no plugin scan)
#
# Needs Xvfb/imagemagick/xdotool, and openbox for a faithful layout:
#   nix develop .#reaper-test -c just reaper theme-shot
theme-shot out="target/theme-shots/theme.png" *args:
    cargo run -q -p fts-themer --bin fts-themer -- \
        --theme "{{theme_dir}}" shot --out "{{out}}" {{args}}

# Same, but against your real ~/fts-dev profile (ini restored afterwards)
theme-shot-profile out="target/theme-shots/profile.png":
    cargo run -q -p fts-themer --bin fts-themer -- \
        --theme "{{theme_dir}}" shot --out "{{out}}" \
        --profile "{{reaper_home}}"

# Serve the theme editor GUI (colours, live preview, accent generation)
theme-edit:
    cd "{{justfile_directory()}}/features/reaper/fts-themer-ui" && \
        dx serve --platform web --hot-patch false

# Pack the theme into dist/<name>.ReaperThemeZip for distribution
theme-pack:
    #!/usr/bin/env bash
    set -euo pipefail
    out="{{justfile_directory()}}/dist/{{theme_name}}.ReaperThemeZip"
    mkdir -p "$(dirname "$out")"
    rm -f "$out"
    # Paths inside the zip must be relative to the theme dir: REAPER wants
    # <name>.ReaperTheme and <name>/ at the archive root.
    (cd "{{theme_dir}}" && zip -qr "$out" \
        "{{theme_name}}.ReaperTheme" "{{theme_name}}" \
        -x '*/.DS_Store' '*/Thumbs.db')
    echo "Packed -> $out ($(du -h "$out" | cut -f1))"
