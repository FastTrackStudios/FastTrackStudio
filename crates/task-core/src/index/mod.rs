//! SQLite index — fast queries layered on top of the SeaORM persistence
//! boundary. Provides:
//!
//! - Sub-millisecond filters by status, assignee, due date, project, tag
//! - Full-text search across task titles and bodies
//! - Change tracking (who changed what, when)

mod search;
mod sqlite;

pub use search::SearchIndex;
pub use sqlite::{ChangeRow, ConflictRow, TaskIndex, TaskRow};
