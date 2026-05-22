# Wiki log

Append-only operation timeline. Each entry: `## [YYYY-MM-DD] <op> | <title>`.

Grep with `grep '^## \[' Wiki/log.md` for a clean timeline.

## [2026-05-21] admin | Bootstrap

Initial wiki bootstrap. `schema.md` + `purpose.md` seeded; 33 existing notes (concepts + tools, imported from the pre-wiki `concepts/` and `tools/` folders) catalogued in `index.md`. `raw/sources/` empty — no documents imported yet.

## [2026-05-21] admin | Layout finalized

Adopted llm_wiki-shaped layout: `Wiki/raw/sources/` for immutable input, `Wiki/<Type>/` (or flat `Wiki/`) for pages, `Wiki/_state/` for opaque agent state. `schema.md` + `purpose.md` at the wiki root. See `plans/wiki-feature.md`.
