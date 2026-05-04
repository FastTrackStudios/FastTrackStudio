//! SQLite index — fast queries over .md frontmatter.
//!
//! The index is a disposable cache. It can be rebuilt from .md files
//! at any time. It provides:
//!
//! - Sub-millisecond queries by status, assignee, due date, project, tag
//! - Full-text search across task titles and bodies
//! - Change tracking (who changed what, when)
//!
//! The index is NOT the source of truth. The .md files are.

mod search;
mod sqlite;

pub use search::SearchIndex;
pub use sqlite::{ChangeRow, ConflictRow, RebuildStats, TaskIndex, TaskRow};
