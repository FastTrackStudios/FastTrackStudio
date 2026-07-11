//! Inbox CRUD — persistence for captured items.
//!
//! Deliberately a flat CRUD surface: capture, daily-read, snooze,
//! mark-processed, and archive are all just an `upsert_inbox_item`
//! with a mutated item (same way `scheduling-proto`'s
//! `upsert_event` covers both create and update). The caller owns
//! the state transitions; the backend just round-trips markdown.

use crate::error::InboxError;
use crate::inbox_item::InboxItem;

#[architect::rpc]
pub trait Inbox {
    /// Every item in the inbox, processed or not.
    fn list_inbox(&self) -> Result<Vec<InboxItem>, InboxError>;

    /// The daily-review queue for `today` (ISO `YYYY-MM-DD`): open
    /// items not snoozed past today, oldest capture first. The
    /// "what surfaces today" rule (see [`InboxItem::in_review_queue`])
    /// lives here so every surface — CLI brief, web inbox — gets the
    /// same queue without re-implementing the snooze filter.
    fn review_queue(&self, today: String) -> Result<Vec<InboxItem>, InboxError>;
    /// One item by id.
    fn get_inbox_item(&self, id: &str) -> Result<InboxItem, InboxError>;
    /// Create or replace an item (keyed by `id`). Capture, snooze,
    /// process, and archive all flow through here.
    fn upsert_inbox_item(&self, item: &InboxItem) -> Result<(), InboxError>;
    /// Permanently remove an item from the vault.
    fn delete_inbox_item(&self, id: &str) -> Result<(), InboxError>;
}
