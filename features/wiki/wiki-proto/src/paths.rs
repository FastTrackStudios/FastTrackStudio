//! Canonical paths inside a wiki.
//!
//! Backends use these to locate state files; agents use them
//! over RPC to know which files they're reading or writing.
//! Everything is `<vault>/Wiki/` relative — federation peers
//! see the same shape.
//!
//! ## On-disk layout
//!
//! ```text
//! <vault>/Wiki/
//! ├── schema.md       ← contract / conventions
//! ├── purpose.md      ← goals / scope / key questions
//! ├── index.md        ← content catalog (LLM-maintained)
//! ├── log.md          ← append-only operation timeline
//! ├── overview.md     ← global synthesis (optional)
//! ├── sources/        ← imported raw documents (mirror)
//! ├── media/          ← extracted images, attachments
//! └── _state/         ← opaque agent state (JSON)
//!     ├── ingest_queue.json
//!     ├── review.json
//!     ├── lint_findings.json
//!     ├── research_plans.json
//!     ├── peers.json
//!     └── snapshot.json     (sha256 of every tracked file)
//! ```
//!
//! Wiki pages themselves live as plain markdown anywhere under
//! `Wiki/` (typically `Wiki/<Type>/<Slug>.md`, e.g.
//! `Wiki/Concepts/Spaced repetition.md`). The `type:`
//! frontmatter field is what the graph + lint use, not the
//! folder.

/// Folder name at the vault root that hosts the wiki.
pub const WIKI_ROOT: &str = "Wiki";

/// Schema doc filename (relative to `Wiki/`).
pub const SCHEMA_MD: &str = "schema.md";

/// Purpose doc filename (relative to `Wiki/`).
pub const PURPOSE_MD: &str = "purpose.md";

/// Content catalog filename (relative to `Wiki/`).
pub const INDEX_MD: &str = "index.md";

/// Append-only operation log filename (relative to `Wiki/`).
pub const LOG_MD: &str = "log.md";

/// Global synthesis page (optional; relative to `Wiki/`).
pub const OVERVIEW_MD: &str = "overview.md";

/// Sub-folder for imported raw sources.
pub const SOURCES_DIR: &str = "sources";

/// Sub-folder for extracted images + attachments.
pub const MEDIA_DIR: &str = "media";

/// Sub-folder for opaque agent state. JSON-encoded.
pub const STATE_DIR: &str = "_state";

/// Ingest queue state file (relative to `Wiki/_state/`).
pub const INGEST_QUEUE_JSON: &str = "ingest_queue.json";

/// Review queue state file (relative to `Wiki/_state/`).
pub const REVIEW_JSON: &str = "review.json";

/// Open lint findings state file (relative to `Wiki/_state/`).
pub const LINT_FINDINGS_JSON: &str = "lint_findings.json";

/// Research plans state file (relative to `Wiki/_state/`).
pub const RESEARCH_PLANS_JSON: &str = "research_plans.json";

/// Federation peer registry (relative to `Wiki/_state/`).
pub const PEERS_JSON: &str = "peers.json";

/// File-content snapshot (sha256-keyed) for change detection
/// (relative to `Wiki/_state/`).
pub const SNAPSHOT_JSON: &str = "snapshot.json";
