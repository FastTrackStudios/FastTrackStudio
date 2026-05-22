//! Index + log — the catalog and timeline.
//!
//! Both are markdown files under `Wiki/`, but this crate
//! exposes them as structured types so backends can produce
//! consistent output without re-parsing markdown every time.
//! Convert with the backend's `serialize_index` /
//! `parse_index` helpers (those live in `wiki-live`, not here).

use chrono::{DateTime, NaiveDate, Utc};
use facet::Facet;

/// Parsed view of `Wiki/index.md`. The on-disk markdown is the
/// source of truth; this is the LLM-readable mirror.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct WikiIndex {
    /// One section per page `type:` (`entity`, `concept`, etc.).
    /// Sections are emitted in stable order so diffs are
    /// reviewable.
    pub sections: Vec<IndexSection>,
    /// Total pages indexed. Convenience; computable from
    /// `sections`.
    pub total: u32,
}

#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct IndexSection {
    /// Page `type:` this section covers (e.g. `"concept"`).
    pub page_type: String,
    /// Entries sorted by title.
    pub entries: Vec<IndexEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct IndexEntry {
    /// Page title, used as the wikilink target.
    pub title: String,
    /// Vault-relative path (e.g. `Wiki/Concepts/Foo.md`).
    pub path: String,
    /// One-line summary lifted from the page's first paragraph
    /// or the LLM's analysis. May be empty.
    pub summary: String,
    /// Count of `sources:` entries this page cites (for
    /// `source`-typed pages, always 1).
    pub source_count: u32,
}

/// One entry appended to `Wiki/log.md`. Backends serialize
/// these as a fenced markdown section starting `## [YYYY-MM-DD]
/// <op> | <title>` so the timeline stays grep-able.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct LogEntry {
    /// When this happened. Wall-clock UTC; the serializer
    /// renders the date in the header and the full timestamp
    /// in the body.
    pub at: DateTime<Utc>,
    /// The operation kind.
    pub op: LogOp,
    /// Short human-facing title (page title for ingest, query
    /// text for query, scope name for lint, etc.).
    pub title: String,
    /// Optional free-form body. Markdown; rendered under the
    /// section header.
    pub body: String,
    /// Pages touched by this operation, if any. Wikilink-able
    /// titles.
    pub pages_touched: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(C)]
pub enum LogOp {
    Ingest,
    Query,
    Lint,
    Review,
    Research,
    /// Bootstrap (wiki created, schema written, etc.) and
    /// other one-off admin events.
    Admin,
}

/// A clock-free date used in the log header (`[2026-05-21]`).
/// Wraps `chrono::NaiveDate` to keep the wire shape obvious.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Facet)]
#[repr(C)]
pub struct LogDate {
    pub date: NaiveDate,
}
