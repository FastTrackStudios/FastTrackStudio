# Plans

Forward-looking design docs for Task.

## Active

| File | What |
|---|---|
| [`vault-feature.md`](vault-feature.md) | Current vault architecture (proto / live / facade / obsidian). Not "future work" — captures the shipped shape so a fresh reader can orient. |
| [`task-feature.md`](task-feature.md) | First-party TaskNotes-style task feature. Foundation shipped; slices 2–7 (time tracking, .base view runner, kanban UI, recurrence, field mapping, TaskService) queued. |
| [`knowledge-graph.md`](knowledge-graph.md) | Design notes from reading `nashsu/llm_wiki`. Targets a `vault-graph` crate (4-signal relevance, Louvain clusters, force-directed layout). Not started. |
| [`vault-sync-desktop-multiserver.md`](vault-sync-desktop-multiserver.md) | Wire a `Local | Remote { VaultSyncClient }` `VaultBackend` enum into the desktop app's vault selector. Pending. |

## [`done/`](done/) — shipped slices

Plans + records of work that landed. Useful as "what changed
and why" reference.

- `vault-sync-vox-migration.md` — `vault-sync` moved to
  `#[architect::rpc]` over vox.
- `gantt-port.md` — Dioxus port of svar-widgets/gantt into
  `features/view/view-gantt/`.
- `knowledge-rip.md` — deleted the knowledge feature; parsers
  migrated into vault.
- `project-crdt-rip.md` — deleted the Loro entity layer
  (project-{proto, crdt, db}, task-db, server's
  `WorkspaceSyncImpl` + `*RepoLoro` dispatchers).
- `editor-integration.md` — moved the editor subtree into
  the workspace; wired `editor::EditorApp` into `task-ui`;
  fixed the wasm dev shell.

## [`archived/`](archived/) — abandoned designs

Plans whose target architecture got ripped. Kept as
historical record + future mining material. See
[`archived/README.md`](archived/README.md) for the
file-by-file map.

## When to add a plan here

- **Multi-slice work**: write it down before the second
  commit. The commit messages capture *what changed*; the
  plan captures *why this and not the alternatives*.
- **Research**: when you've spent a day reading some other
  project's code, capture the model + what to crib vs not
  (see `knowledge-graph.md` for the shape).
- **Don't write plans for**: single-commit refactors, bug
  fixes, mechanical renames. Commit message is enough.
