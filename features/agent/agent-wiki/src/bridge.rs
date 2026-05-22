//! Orchestration helpers — sequences `agent_proto::AgentService`
//! dispatches with `wiki_proto::WikiService` writes to run
//! whole pipelines (ingest, lint pass, deep research, dedup
//! merge).
//!
//! ## State of implementation
//!
//! Function signatures + doc comments are committed.
//! Bodies are `todo!()` placeholders; concrete
//! orchestration lands when the first agent backend lands.
//! The point of this slice is the type surface — bridge
//! callers (CLI, UI, server) can take dependencies on these
//! signatures without waiting for the impls.
//!
//! ## Typical loop (ingest)
//!
//! ```text
//! IngestRequest { wiki_id, source_path, ... }
//!       │
//!       ▼
//! wiki.read_schema + read_purpose + read_index  (context bundle)
//!       │
//!       ▼
//! agent.dispatch_turn  (system: INGEST_ANALYZE_SYSTEM,
//!                       user: source bytes)
//!       │ ── stream events ──▶ caller (progress)
//!       ▼
//! (analysis text)
//!       │
//!       ▼
//! wiki.record_analysis(task_id, AnalysisDraft { ... })
//!       │
//!       ▼
//! agent.dispatch_turn  (system: INGEST_GENERATE_SYSTEM,
//!                       user: analysis)
//!       │
//!       ▼
//! parsers::parse_ingest_blocks(response)
//!       │
//!       ▼
//! wiki.record_pages(task_id, PageDrafts)
//! wiki.append_log(LogEntry::ingest)
//! ```

use crate::error::AgentWikiError;

/// Run one ingest task end-to-end (steps 1 + 2). Caller
/// supplies handles to the two services + the ids; this
/// helper sequences the calls and parses LLM output.
pub struct IngestRun<'a> {
    pub wiki_id: &'a str,
    pub session_id: &'a str,
    pub task_id: &'a str,
    pub source_filename: &'a str,
    pub language: &'a str,
}

/// Result of one [`IngestRun`].
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IngestRunResult {
    /// Pages the agent wrote (matches `wiki-proto`
    /// `PageDraft.path`).
    pub pages_written: Vec<String>,
    /// Review items spawned by the run.
    pub review_count: u32,
}

/// Drive one full ingest pipeline. Returns
/// [`AgentWikiError::Bridge`] if either service errors or
/// the LLM output fails parsing.
pub fn run_ingest(_run: IngestRun<'_>) -> Result<IngestRunResult, AgentWikiError> {
    todo!("wires agent_proto::AgentService + wiki_proto::WikiService")
}

/// Run a lint pass over the wiki. Bridges
/// `wiki_proto::WikiService::lint` (for the cheap rule-based
/// scopes) and an LLM dispatch (for `Contradiction` /
/// `MissingCrossRef` scopes).
pub fn run_lint(_wiki_id: &str, _session_id: &str) -> Result<u32, AgentWikiError> {
    todo!("returns count of newly-raised findings")
}

/// Propose a research plan for a knowledge gap. Single LLM
/// call; output parsed via
/// [`crate::parsers::parse_research_plan`].
pub fn run_propose_research(
    _wiki_id: &str,
    _session_id: &str,
    _gap_id: &str,
) -> Result<crate::parsers::ResearchTopicPlan, AgentWikiError> {
    todo!()
}

/// Sweep the wiki's review queue and resolve items the LLM
/// thinks are now stale (covered by recent ingests).
/// Returns ids resolved.
pub fn run_sweep_reviews(_wiki_id: &str, _session_id: &str) -> Result<Vec<String>, AgentWikiError> {
    todo!()
}

/// Detect duplicate pages. Returns groups the LLM
/// flagged — the curator (or a follow-up
/// [`run_dedup_merge`]) decides which to merge.
pub fn run_dedup_detect(
    _wiki_id: &str,
    _session_id: &str,
) -> Result<Vec<crate::parsers::DuplicateGroup>, AgentWikiError> {
    todo!()
}

/// Merge a duplicate group into a single page. Writes the
/// merged page via `record_pages` and updates the index.
pub fn run_dedup_merge(
    _wiki_id: &str,
    _session_id: &str,
    _group: &crate::parsers::DuplicateGroup,
) -> Result<String, AgentWikiError> {
    todo!("returns the path of the merged page")
}
