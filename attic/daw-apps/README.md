# attic/daw-apps — parked crates from the dissolved daw/ workspace

Parked here during the wave-2 restructure (LAYOUT.md park/delete policy).
These directories are OUTSIDE every Cargo workspace — they do not build.
Revive one by moving it into apps/ (or features/) and adding it to the
root `[workspace] members`, converting its old relative `path = "…"` deps
to `x.workspace = true`.

| dir | was | why parked |
|---|---|---|
| `daw-native` | daw/apps/daw/native | standalone native daw app — audit-per-app, keep only what runs |
| `example-extension` | daw/apps/example-extension | REAPER example extension (docs-grade sample) |
| `example-plugin` | daw/apps/example-plugin | REAPER example plugin (docs-grade sample) |
| `daw-xtask` | daw/xtask | daw workspace task runner (`cargo xtask`); depended on the fts-repo git dep and the dissolved workspace's alias |
| `daw-beads` | daw/.beads | beads issue-tracking data of the old daw repo |
| `daw-tasks` | daw/tasks | task notes of the old daw repo |

The old workspace's docs moved to `crates/daw/docs/`.
