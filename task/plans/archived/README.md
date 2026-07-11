# Archived plans

These are designs that targeted architecture we've since ripped
or abandoned. Kept as historical record + future mining
material; **not** representative of current state.

If you're a fresh reader trying to figure out what Task is
today, do NOT start here. Start at the top-level
[`plans/`](../) (forward-looking) or [`plans/done/`](../done/)
(shipped slices with their design rationale).

## What's in here

| File | What it targeted | Why archived |
|---|---|---|
| `decentralized-foundation.md` / `.goal.md` | Phase 1–13 CRDT entity vision | Loro entity layer fully ripped (commits `64a8740` + `63ce4a1`); see `plans/done/{knowledge,project-crdt}-rip.md` |
| `derive-entity-crdt.md` | Architect `#[derive(Entity)] #[architect(repo)]` design | Macro lives in architect repo; Task no longer uses the Repo dispatchers |
| `logseq-data-model-alignment.md` | Knowledge-CRDT page/block schema | knowledge-crdt deleted |
| `logseq-outliner.md` | Logseq-style outliner UI | Superseded by `editor::EditorApp` |
| `loro-text-editor-upgrade.md` / `.goal.md` | Per-page Loro CRDT in the outliner | Pre-rip; future per-file CRDT lives in the editor crate, not as planned here |
| `obsidian-vault-mount.md` | Mount Obsidian dir as a CRDT vault | Replaced by `vault-obsidian::open_as_backend` (file-native) |
| `sync-architecture.md` | Phase 1 CRDT sync + Phase 2 file replication | Phase 1 gone; Phase 2 shipped as `vault-sync-proto` / `vault-live` (see `plans/done/vault-sync-vox-migration.md`) |
| `threads-feature-deepening.*` | Multi-phase chat/email threads feature | Feature not built; depended on entity-CRDT layer |
| `vault-publisher.md` | Static site from `.loro` snapshots | `apps/publish` deleted in `64a8740`; publish-core parked under `crates/legacy/` |
| `vox-phase-{2,3}-*.md` | DB services + repo services over vox | All `*Repo` dispatchers ripped |
| `agent-mvp.goal.md` / `agent-p4-dashboard.goal.md` | Agent feature with CRDT-backed `AgentRun` | Entity-coupled; feature not built |
| `cursor-awareness.md` | Multi-peer cursor sync via Loro awareness | Loro awareness ripped along with `WorkspaceSyncImpl` |
| `notifications-mvp.goal.md` | Notifications on `AgentRun` transitions | Entity-coupled; feature not built |
| `vertical-slice-surprises.md` | Retrospective on an earlier reset | Premise (the older slice shape) no longer current |

## When to revive an archived plan

- A design idea here might still be sound; the **implementation
  path** is what's stale (CRDT-coupled). If you want to bring
  one back, rewrite it against the file-backed `vault` + the
  TaskNotes-style task feature, and put the new version in
  `plans/` (not here).
- The archived doc itself stays put as the trail.
