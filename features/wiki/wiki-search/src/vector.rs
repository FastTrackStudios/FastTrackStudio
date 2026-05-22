//! Vector + hybrid search via LanceDB. Behind the
//! `vector` feature flag.
//!
//! **Scaffold only** — wires the call shape but doesn't
//! actually run a query yet. A real impl needs:
//!
//! 1. An indexing pipeline (embedding generation per page,
//!    typically Codex-or-equivalent for vector_512). Lives
//!    in `wiki-agent` once the embedding endpoint is
//!    wired.
//! 2. A LanceDB schema (`(path TEXT, title TEXT,
//!    embedding VECTOR(512), updated_at TIMESTAMP)`).
//! 3. The hybrid merge — re-rank token hits against
//!    cosine distance using reciprocal-rank fusion.
//!
//! Today: returns the token-only path with `mode` flipped
//! to `Hybrid` so callers see they asked for it; vector
//! count stays 0. This keeps the surface stable while we
//! wire the indexer.

use std::path::Path;

use wiki_proto::search::{SearchHits, SearchMode, SearchOpts};

use crate::SearchError;

pub(crate) fn search_hybrid(
    vault_root: &Path,
    opts: &SearchOpts,
) -> Result<SearchHits, SearchError> {
    // TODO: lancedb::connect + table.query + vector search.
    // For now, fall through to token + tag the response.
    let mut token_hits = crate::token::search_token(vault_root, opts)?;
    token_hits.mode = SearchMode::Hybrid;
    Ok(token_hits)
}
