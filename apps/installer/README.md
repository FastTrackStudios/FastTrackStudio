# FastTrackStudio Installer

The standalone installer app (Dioxus UI over `libs/installer-core`).

## Future role

The installer's job is narrowing to ONE thing: **download and install the
`fasttrackstudio` app** (plus the engine binaries that ship next to it,
e.g. `signal-engine`, and the `fts` CLI). Everything else it historically
handled — REAPER setup, extensions, presets — moves behind the app itself:
once installed, `apps/fasttrackstudio` manages its engines (see its
`engines.rs` supervisor) and keeps itself current via its auto-update
module (`updates.rs`, feed: codeberg releases), reusing
`libs/installer-core`'s download/retry/progress primitives.

Not rebuilt yet — this is a design note so the next pass on the installer
starts from the right scope.
