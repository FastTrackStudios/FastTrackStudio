# Example vault

Small Obsidian-shaped vault tracked in-repo so tests, the playground,
and the CLI have a consistent fixture to point at.

Layout: six markdown pages exercising wikilinks, section refs,
Logseq-style block refs (`((uuid))`), embeds, frontmatter
properties, Obsidian tags, mermaid + math blocks, and a daily
note.

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
