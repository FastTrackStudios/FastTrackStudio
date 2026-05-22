//! Deep-Research flow. The LLM proposes search queries
//! grounded in a [`crate::graph::KnowledgeGap`] or
//! [`crate::review::ReviewItem`], the agent executes them
//! externally (Tavily / SerpApi / SearXNG / manual), and
//! submits results back as raw sources to be re-ingested.

use chrono::{DateTime, Utc};
use facet::Facet;

#[derive(Debug, Clone, PartialEq, Facet)]
#[repr(C)]
pub struct ResearchPlan {
    pub id: String,
    /// What this plan targets. Either a knowledge-graph gap
    /// id, a lint finding id, or a review item id — backend
    /// disambiguates via `source_kind`.
    pub source_kind: ResearchSourceKind,
    pub source_id: String,
    /// Curator-facing topic statement. Editable before the
    /// plan executes.
    pub topic: String,
    /// Search queries to run. Multi-query so the LLM can
    /// triangulate.
    pub queries: Vec<ResearchQuery>,
    pub created_at: DateTime<Utc>,
    pub status: ResearchStatus,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(C)]
pub enum ResearchSourceKind {
    KnowledgeGap,
    LintFinding,
    ReviewItem,
    /// Curator opened a free-form research session.
    Manual,
}

#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct ResearchQuery {
    /// Search engine to use (`"tavily"`, `"serpapi"`,
    /// `"searxng"`, `"manual"`).
    pub engine: String,
    /// Verbatim query string.
    pub q: String,
    /// Result cap.
    pub limit: u32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(C)]
pub enum ResearchStatus {
    /// Plan generated but not yet started.
    Proposed,
    /// Agent is running queries.
    Running,
    /// All queries returned; awaiting [`crate::service::WikiService::submit_research_result`].
    Awaiting,
    /// Results submitted; spawned ingest tasks are tracked
    /// independently.
    Submitted,
    /// Curator cancelled before completion.
    Cancelled,
}

/// One result returned from a research query. Submitting a
/// batch of these spawns an [`crate::ingest::IngestTask`] per
/// row.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct RawSource {
    /// Where the source came from (URL, file path, etc.).
    pub origin: String,
    /// Filename to use when saving to `Wiki/sources/`. The
    /// backend ensures uniqueness.
    pub filename: String,
    /// Source bytes (markdown, HTML, PDF, etc.). The
    /// backend persists this to `Wiki/sources/<filename>`.
    pub bytes: Vec<u8>,
    /// MIME type.
    pub mime: String,
    /// Optional title from the search result. Used in
    /// `log.md` and the agent's analysis prompt.
    pub title: String,
}
