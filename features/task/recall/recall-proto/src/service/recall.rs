//! Recall CRUD — persistence for the learning deck.
//!
//! A flat CRUD surface like `inbox_proto`: create/author, edit,
//! review-reschedule, and archive all flow through `upsert_card` with
//! a mutated card. The caller owns the FSRS state transitions (via
//! [`crate::RecallCard::review`]); the backend just round-trips
//! markdown.

use crate::error::RecallError;
use crate::recall_card::RecallCard;

#[architect::rpc]
pub trait Recall {
    /// Every card in the deck, archived or not.
    fn list_cards(&self) -> Result<Vec<RecallCard>, RecallError>;

    /// The review queue for `today` (ISO `YYYY-MM-DD`): due,
    /// non-archived cards. The "what surfaces today" rule (see
    /// [`RecallCard::in_review_queue`]) lives here so every surface
    /// gets the same queue without re-implementing the due filter.
    fn review_queue(&self, today: String) -> Result<Vec<RecallCard>, RecallError>;

    /// Create or replace a card (keyed by `id`). Author, edit,
    /// review-reschedule, and archive all flow through here.
    fn upsert_card(&self, card: &RecallCard) -> Result<(), RecallError>;

    /// Permanently remove a card from the vault.
    fn delete_card(&self, id: &str) -> Result<(), RecallError>;
}
