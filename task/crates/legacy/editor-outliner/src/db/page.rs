//! Page model — one `.md` file in the vault. Mirrors Logseq's
//! `frontend.db.model/page` shape: stable id, on-disk basename,
//! optional aliases + frontmatter, journal-day flag for
//! `journals/YYYY-MM-DD.md`.

use chrono::{DateTime, Utc};
use uuid::Uuid;

#[derive(Clone, Debug, PartialEq)]
pub struct Page {
    pub id: Uuid,
    /// Filename without the `.md` extension (`Home`, `Projects/Alpha`).
    pub basename: String,
    /// Path-relative-to-vault including the extension
    /// (`pages/Home.md`, `journals/2026-05-20.md`).
    pub relpath: String,
    /// Page-level aliases (from frontmatter `aliases:`).
    pub aliases: Vec<String>,
    /// Raw frontmatter as a JSON blob (Logseq stores it
    /// similarly so round-trip stays cheap).
    pub frontmatter_json: String,
    /// `true` when the page lives under `journals/` — drives the
    /// "journal day" sidebar group.
    pub is_journal: bool,
    /// ISO-8601 day (`2026-05-20`) when `is_journal` is true.
    pub journal_day: Option<String>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Construct a page from a basename. Used by tests + the
/// new-page handler.
pub fn new_page(basename: impl Into<String>) -> Page {
    let basename = basename.into();
    let now = Utc::now();
    Page {
        id: Uuid::new_v4(),
        relpath: format!("pages/{basename}.md"),
        basename,
        aliases: Vec::new(),
        frontmatter_json: "{}".into(),
        is_journal: false,
        journal_day: None,
        created_at: now,
        updated_at: now,
    }
}
