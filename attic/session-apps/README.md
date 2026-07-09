# attic/session-apps — parked crates from the dissolved session/ workspace

Parked here during the wave-3 restructure (LAYOUT.md park/delete policy).
These directories are OUTSIDE every Cargo workspace — they do not build.
Revive one by moving it into apps/ and adding it to the root
`[workspace] members`, converting its old relative `path = "…"` deps to
`x.workspace = true`.

| dir | was | why parked |
|---|---|---|
| `desktop` | session/apps/desktop | desktop gateway app — superseded: the unified app (apps/fasttrackstudio) embeds the session engine in-process |
| `web` | session/apps/web | session web remote — superseded: signal's web remote carries the session client |
| `cli` | session/apps/cli | session CLI — no consumer left (FastTrackStudio's `session-cli` workspace-dep entry was unused and removed) |
| `xtask` | session/xtask | session workspace task runner (`cargo xtask`); depended on the fts-repo git dep and the dissolved workspace's alias |

The old workspace's crates moved to `crates/session/{session,proto,ui}`,
`features/guide` (session-guide) and `features/reaper/session-extension`;
its docs moved to `crates/session/docs/`.
