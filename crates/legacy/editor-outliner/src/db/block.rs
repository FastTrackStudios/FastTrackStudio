//! Block model — one bullet in the outliner. Mirrors Logseq's
//! `frontend.db.model/block`.

use chrono::{DateTime, Utc};
use uuid::Uuid;

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub id: Uuid,
    pub page_id: Uuid,
    /// `None` for top-level blocks; `Some(parent_id)` for nested.
    pub parent_id: Option<Uuid>,
    /// Fractional-index string. Compare lexicographically for
    /// document order (Logseq uses the same approach via
    /// `:block/order`).
    pub sort_key: String,
    /// Raw markdown the user typed. May contain inline tokens
    /// like `[[Page]]`, `((uuid))`, `**bold**`.
    pub content: String,
    /// Block-level properties parsed out of `key:: value` lines.
    /// Stored as JSON so round-tripping preserves order.
    pub properties_json: String,
    /// Collapsed children visibility flag.
    pub collapsed: bool,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// Construct a fresh block on a given page with the given
/// content. `parent_id` is None for a top-level block.
pub fn new_block(page_id: Uuid, parent_id: Option<Uuid>, content: impl Into<String>) -> Block {
    let now = Utc::now();
    Block {
        id: Uuid::new_v4(),
        page_id,
        parent_id,
        sort_key: "m".into(),
        content: content.into(),
        properties_json: "{}".into(),
        collapsed: false,
        created_at: now,
        updated_at: now,
    }
}
