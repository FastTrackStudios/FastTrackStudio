# fts-ui task runner. `just` lists recipes.

default:
    @just --list

# Serve the native (Blitz) Lookbook with hot-reload, in release mode.
#
# `dx serve` watches the source and auto-updates: RSX/asset edits hot-reload
# live, Rust changes trigger a rebuild. Release matters — under debug_assertions
# Stylo/Parley mis-render (and the VRT path panics), so previews are only
# trustworthy in release. The native renderer comes from apps/native/Dioxus.toml
# (`default_platform = "native"`); audio stories via the `audio` feature.
# Override logging with RUST_LOG, e.g. `RUST_LOG=fts_story_shell=debug just showcase`.
showcase:
    dx serve -p fts-ui-showcase-native --release

# Same, but the webview (wry) showcase shell.
showcase-desktop:
    dx serve -p fts-ui-showcase-desktop --release
