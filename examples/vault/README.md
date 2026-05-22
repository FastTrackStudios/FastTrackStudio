# Example vault

Small Obsidian-shaped vault tracked in-repo so tests, the playground,
and the CLI have a consistent fixture to point at.

Layout — PARA + Zettelkasten-ish, plain-name folders:

| Folder      | What |
|-------------|---|
| `Inbox/`    | Capture target, unsorted. |
| `Fleeting/` | Rough notes, may evaporate. |
| `Wisdom/`   | Atomic notes (zettels) — one claim per note. |
| `Wiki/`     | Reference knowledge — concepts + tools. |
| `Projects/` | Active work, one note per project. |
| `People/`   | Collaborators, references. |
| `Daily/`    | `YYYY-MM-DD.md` synthesis notes. |
| `Meetings/` | `YYYY-MM-DD <topic>.md`. |
| `Bases/`    | `.base` view files (YAML query DSL). |
| `Stubs/`    | Placeholders for not-yet-fleshed notes. |
| `tasks/`    | Task notes (TaskNotes-shape frontmatter). |

Each folder contains a folder note (e.g. `Wiki/Wiki.md`) acting
as the virt-folder parent. Notes carry `folder: "[[Parent]]"` in
their frontmatter pointing at that note — single value, since
multi-folder navigation is handled by tags instead.

The notes exercise wikilinks, section refs, Logseq-style block
refs (`((uuid))`), embeds, frontmatter properties, Obsidian tags,
mermaid + math blocks, and daily notes.

Used by:

- `features/editor/examples/playground` — `init_vault()` prefers
  this directory over `~/Documents/Task` so the playground always
  has cross-doc resolution wired even on a fresh clone.
- `apps/cli` (the `vault-*` subcommands) — point the CLI at
  `examples/vault/` for quick `cargo run -p task-cli -- vault tasks
  --root examples/vault` smoke checks.
- `features/vault/vault-obsidian` integration tests — the
  Observatory-scale `tests/smoke_observatory.rs` is gated behind
  a present-only check; this small vault is the always-on
  in-tree fixture for write-up examples.

Editing these files commits to git like any other tracked content.
For day-to-day notes, point your editor at a different directory.
