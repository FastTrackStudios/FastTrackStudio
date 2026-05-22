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
| ✅ Working | 13 |
| 🟡 Spec'd, unwired | 11 |
| ❌ Out of scope | 1 |

Total surface: 25 features. Roughly **52% working, 44% spec'd, 4% out of scope.**

## Storage + bootstrap

| Feature | Status | Where |
|---|---|---|
| `Wiki/raw/sources/` + `Wiki/_state/` layout | ✅ | `wiki-proto::paths`, `wiki-live::raw::bootstrap_dirs` |
| Schema + purpose docs | ✅ | `wiki-proto::schema`, `wiki-live::context::ensure_schema/ensure_purpose` |
| Atomic temp-rename writes | ✅ | `wiki-live::state::atomic_write` (used throughout) |
| sha256 dedup on raw import | ✅ | `wiki-live::raw::import_raw_source` (rename collisions to `-<sha8>`) |
| `snapshot.json` (skip re-ingest of unchanged bytes) | 🟡 | Path constant exists. Not yet enforced on enqueue. |

## Ingest pipeline

| Feature | Status | Where |
|---|---|---|
| Two-step CoT (analyze → generate) | ✅ | `agent-wiki::bridge::run_ingest` |
| FILE block parser (`---FILE:` / `---END FILE---`) | ✅ | `agent-wiki::parsers::parse_ingest_blocks` |
| REVIEW block parser | ✅ | (same — both ingest blocks types) |
| Persistent JSON queue with state machine | ✅ | `wiki-live::queue` |
| `index.md` (catalog by `type:`) | ✅ | `wiki-live::index::rebuild_index` |
| `log.md` (grep-friendly `## [YYYY-MM-DD] <op> | <title>`) | ✅ | `wiki-live::log_md::append_log` |
| Language directive injection | ✅ | `agent-wiki::prompts::language_directive`, CLI `--language` |
| Recursive folder import (preserve dir structure) | 🟡 | Single-file works. Need a small walker around `import_raw_source`. |
| Source folder watcher (auto-enqueue on FS events) | 🟡 | `vault-live` has a watcher; not yet bridged. `wiki-proto::set_watch` declared. |
| Auto-retry on crash recovery | 🟡 | `retries` field on `IngestTask` exists; no auto-bump on backend death. |

## Knowledge graph

| Feature | Status | Where |
|---|---|---|
| 4-signal relevance (direct ×3, source-overlap ×4, Adamic-Adar ×1.5, type-affinity ×1) | ✅ | `wiki-graph::build_graph`. CLI: `task wiki graph`. |
| Louvain community detection + cohesion | 🟡 | `wiki-proto::graph::Cluster`. No impl. |
| Knowledge gaps — orphan + missing-page | ✅ | `wiki-graph::find_gaps`. CLI: `task wiki gaps`. |
| Knowledge gaps — sparse cluster + bridge | 🟡 | Needs Louvain first. |

## Lint, dedup, research

| Feature | Status | Where |
|---|---|---|
| Semantic lint blocks (`---LINT: type | severity | title---`) | 🟡 | Prompt ported. Parser + bridge `todo!()`. |
| Dedup detect (slug-grouping JSON) | 🟡 | Prompt ported. Parser + bridge `todo!()`. |
| Dedup merge (full-page rewrite) | 🟡 | Prompt ported. Bridge `todo!()`. |
| Deep Research (multi-query topic generation) | 🟡 | Prompt ported. Parser + bridge `todo!()`. No web-search backend (Tavily/SerpApi/SearXNG). |
| Sweep stale review items | 🟡 | Prompt ported. JSON parser + bridge `todo!()`. |

## Multimodal + search

| Feature | Status | Where |
|---|---|---|
| Image extraction (pdfium for PDFs, zip+XML for PPTX/DOCX) | 🟡 | `wiki-proto::multimodal::ExtractOpts/ExtractedImage`. No impl. |
| Vision caption (pinned + contextual prompts) | 🟡 | Both prompts ported. Needs vision-capable LLM backend (Codex `gpt-5.4-vision` etc.). |
| Token search | 🟡 | `wiki-proto::search::SearchOpts/SearchMode::Token`. No impl. |
| Vector search (LanceDB) | 🟡 | `SearchMode::Hybrid` reserved. No embedding store. |

## API surface

| Feature | Status | Where |
|---|---|---|
| Local HTTP API (`:19828` in llm_wiki) | 🟡 | `wiki-proto::WikiService` is the trait, `architect::rpc` emits the wire format. Not yet mounted on the server. |
| Health snapshot | 🟡 | `wiki-proto::health::WikiHealth` shipped. No `wiki-live` impl yet. |
| `rescan_sources` | 🟡 | Declared on `WikiService`. No impl. |

## Out of scope

| Feature | Status | Notes |
|---|---|---|
| Chrome web clipper | ❌ | Separate UI work. Reach for it only when the rest of the pipeline is solid. |

## Tactical roadmap (cheapest → biggest)

1. ✅ **`wiki-graph`** shipped — 4-signal graph + orphan/missing-page gaps. CLI: `task wiki graph|gaps`. Louvain + sparse-cluster/bridge gaps queued.
2. **Recursive folder import** — ~30 LOC around `import_raw_source`.
3. **Source folder watcher** — wire `vault-live`'s watcher to `wiki-live::enqueue_ingest`.
4. **Snapshot.json dedup** — skip re-ingest of unchanged bytes by tracking last-seen sha256.
5. **Lint parser + bridge** — prompt is already ported.
6. **Dedup detect + merge parser + bridge**.
7. **Deep Research parser + bridge** — bridge stops at submission; web search is an external integration.
8. **Health endpoint on `wiki-live`**.
9. **Multimodal extraction** (pdfium PDFs + zip PPTX/DOCX).
10. **HTTP API mount** — bind `WikiService` onto the existing architect-rpc server.
11. **Vector search** — only if the curator wants embedding-backed retrieval. LanceDB integration is its own slice.

## Demo state today

```bash
# End-to-end ingest, parity with llm_wiki's flagship workflow:
task wiki ingest -v examples/vault \
  -s examples/vault/Wiki/raw/sources/karpathy-llm-wiki.md \
  --language English --timeout-secs 240

# Output: 8 typed pages + 2 review items written under
# examples/vault/Wiki/, index + log refreshed, ingest task
# logged in _state/ingest_queue.json.

# 4-signal graph snapshot:
task wiki graph -v examples/vault --limit 20
# → nodes=20  edges=…
#     10  [concept]  Knowledge graph
#      9  [entity]   Obsidian
#      …

# Knowledge gaps:
task wiki gaps -v examples/vault
# → gaps=47 (orphans=12 missing-pages=35)
#     [Orphan] Page "Block Library" has degree 1 — …
#     [MissingPage] [[Editor]] is referenced 3 time(s)…
```

The above is the working surface as of this commit. Every
🟡 row above is one slice away.
