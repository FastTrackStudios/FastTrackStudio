# LLM-Wiki parity tracker

Tracks Task's wiki feature against [`nashsu/llm_wiki`][upstream].
Updated as slices land. The "Working" column states whether
the feature works end-to-end **from the CLI today**, not just
whether types or prompts exist.

[upstream]: https://github.com/nashsu/llm_wiki

## Status legend

| Glyph | Meaning |
|---|---|
| ✅ | Working end-to-end. CLI / API surfaces it. |
| 🟡 | Spec'd at the type / prompt level. Bridge or impl missing. |
| ❌ | Out of scope or not started. |

## Summary

| Bucket | Count |
|---|---|
| ✅ Working | 20 |
| 🟡 Spec'd, unwired | 4 |
| ❌ Out of scope | 1 |

Total surface: 25 features. Roughly **80% working, 16% spec'd, 4% out of scope.**

## Storage + bootstrap

| Feature | Status | Where |
|---|---|---|
| `Wiki/raw/sources/` + `Wiki/_state/` layout | ✅ | `wiki-proto::paths`, `wiki-live::raw::bootstrap_dirs` |
| Schema + purpose docs | ✅ | `wiki-proto::schema`, `wiki-live::context::ensure_schema/ensure_purpose` |
| Atomic temp-rename writes | ✅ | `wiki-live::state::atomic_write` |
| sha256 dedup on raw import | ✅ | `wiki-live::raw::import_raw_source` |
| `snapshot.json` (skip re-ingest of unchanged bytes) | ✅ | `wiki-live::snapshot::rescan_sources` + CLI `task wiki rescan`. |

## Ingest pipeline

| Feature | Status | Where |
|---|---|---|
| Two-step CoT (analyze → generate) | ✅ | `agent-wiki::bridge::run_ingest` |
| FILE block parser | ✅ | `agent-wiki::parsers::parse_ingest_blocks` |
| REVIEW block parser | ✅ | (same module) |
| Persistent JSON queue with state machine | ✅ | `wiki-live::queue` |
| `index.md` (catalog by `type:`) | ✅ | `wiki-live::index::rebuild_index` |
| `log.md` (grep-friendly headers) | ✅ | `wiki-live::log_md::append_log` |
| Language directive injection | ✅ | `agent-wiki::prompts::language_directive`, CLI `--language` |
| Recursive folder import | ✅ | `wiki-live::folder_import`. CLI: `task wiki import --dir`. |
| Source folder watcher (auto-enqueue) | 🟡 | `task wiki rescan --enqueue` covers manual polling. FS-event-driven auto-enqueue (via `vault-live`'s watcher) deferred. |
| Auto-retry on crash recovery | 🟡 | `retries` field exists; no auto-bump on backend death. |

## Knowledge graph

| Feature | Status | Where |
|---|---|---|
| 4-signal relevance | ✅ | `wiki-graph::build_graph`. CLI: `task wiki graph`. |
| Louvain community detection + cohesion | 🟡 | No impl. ~150 LOC slice. |
| Knowledge gaps — orphan + missing-page | ✅ | `wiki-graph::find_gaps`. CLI: `task wiki gaps`. |
| Knowledge gaps — sparse cluster + bridge | 🟡 | Needs Louvain first. |

## Lint, dedup, research

| Feature | Status | Where |
|---|---|---|
| Semantic lint blocks | ✅ | `parse_lint_blocks` + `bridge::run_lint`. Findings persist under `Wiki/_state/lint_findings.json`. CLI: `task wiki lint`, `task wiki findings`. |
| Dedup detect | ✅ | `parse_dedup_groups` + `bridge::run_dedup_detect`. CLI: `task wiki dedup`. |
| Dedup merge | ✅ | `bridge::run_dedup_merge` returns the merged `(path, markdown)` ready for `record_pages`. |
| Deep Research (multi-query) | ✅ | `parse_research_plan` + `bridge::run_propose_research`. CLI: `task wiki research --gap-title …`. Web-search execution is still external. |
| Sweep stale review items | ✅ | `parse_sweep_resolved` + `bridge::run_sweep_reviews`. |

## Multimodal + search

| Feature | Status | Where |
|---|---|---|
| Image extraction (pdfium PDFs, zip+XML for PPTX/DOCX) | 🟡 | `wiki-proto::multimodal::ExtractOpts/ExtractedImage`. No impl. |
| Vision caption | 🟡 | Both prompts ported. Needs vision-capable LLM backend. |
| Token search | 🟡 | `wiki-proto::search::SearchMode::Token`. No impl. |
| Vector search (LanceDB) | 🟡 | `SearchMode::Hybrid` reserved. No embedding store. |

## API surface

| Feature | Status | Where |
|---|---|---|
| Local HTTP API (`:19828`) | 🟡 | `wiki-proto::WikiService` trait + `architect::rpc` wire format. Not yet mounted on the server. |
| Health snapshot | ✅ | `wiki-live::WikiHealth`. CLI: `task wiki health`. |
| `rescan_sources` | ✅ | `wiki-live::snapshot::rescan_sources`. CLI: `task wiki rescan`. |

## Out of scope

| Feature | Status | Notes |
|---|---|---|
| Chrome web clipper | ❌ | Separate UI work. |

## Tactical roadmap (cheapest → biggest)

1. ✅ `wiki-graph` — 4-signal + orphan/missing-page gaps.
2. ✅ Recursive folder import.
3. ✅ Snapshot.json sha256 dedup + rescan.
4. ✅ Lint parser + bridge + findings store.
5. ✅ Dedup detect + merge parser + bridge.
6. ✅ Deep Research parser + bridge.
7. ✅ Sweep stale reviews parser + bridge.
8. ✅ Health endpoint.
9. **Source folder watcher** — wire `vault-live`'s watcher to `wiki-live::enqueue_ingest`. Auto-retry tied in.
10. **Louvain clusters + cohesion** — ~150 LOC; pure computation.
11. **HTTP API mount** — bind `WikiService` onto the architect-rpc server.
12. **Multimodal extraction** (pdfium + zip).
13. **Token search** — grep + TF-IDF.
14. **Vector search (LanceDB)** — embedding-backed retrieval.

## Demo state today

```bash
# End-to-end ingest:
task wiki ingest -v examples/vault \
  -s examples/vault/Wiki/raw/sources/karpathy-llm-wiki.md

# 4-signal graph + gaps:
task wiki graph -v examples/vault --limit 20
task wiki gaps -v examples/vault

# Health snapshot:
task wiki health -v examples/vault
# → pages: 39 / sources: 2 / queue_depth: 0 / queue_failed: 0 …

# Source layer + rescan diff:
task wiki import -v examples/vault --dir /path/to/notes
task wiki rescan -v examples/vault --enqueue

# LLM-driven maintenance:
task wiki lint -v examples/vault                              # raises findings
task wiki findings -v examples/vault                          # lists open findings
task wiki dedup -v examples/vault                             # detects duplicate slugs
task wiki research -v examples/vault \
  --gap-title "PageRank" --gap-description "Heavily referenced concept"
```

Every ✅ above is wired through the CLI. The remaining 🟡 are
focused follow-ups: live watcher, Louvain, HTTP mount,
multimodal, search.
