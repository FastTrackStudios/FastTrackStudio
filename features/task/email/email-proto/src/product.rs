//! `EmailProduct` — the product layer over the synced mailbox:
//! the staged-send outbox (human-in-the-loop approval). Served
//! next to [`crate::EmailSync`] by a store-backed backend
//! (`email-product`); events ride the same `EmailChange` stream
//! (`EmailEvent::OutboxChanged`), so there is no second stream to
//! subscribe to.

use crate::{Derivation, Draft, EmailSyncError, OutboxEntry};

#[architect::rpc]
pub trait EmailProduct {
    /// Cached derivations (urgency / tags / …) for the given
    /// message-ids on `account`. Only rows that exist are
    /// returned — a message the triage pass hasn't reached yet
    /// simply has none (the UI renders no chips). Computation
    /// happens in the bounded background pass, never here.
    fn derivations(
        &self,
        account: &str,
        ids: Vec<String>,
    ) -> Result<Vec<Derivation>, EmailSyncError>;

    /// Every outbox entry for `account`, newest first. Includes
    /// terminal entries (`Sent` / `Failed` / `Cancelled`) so the
    /// panel shows outcomes, not just the queue.
    fn list_outbox(&self, account: &str) -> Result<Vec<OutboxEntry>, EmailSyncError>;

    /// Stage `draft` for approval. Creates a `PendingApproval`
    /// entry and publishes `OutboxChanged`. `origin` records who
    /// staged it (`"user"`, `"agent:<name>"`, …).
    fn submit_draft(
        &self,
        account: &str,
        draft: Draft,
        origin: &str,
    ) -> Result<OutboxEntry, EmailSyncError>;

    /// Approve a `PendingApproval` (or re-approve a `Failed`)
    /// entry — flips it to `Approved`; the server-side delivery
    /// poller picks it up from there. Publishes `OutboxChanged`.
    fn approve(&self, account: &str, id: u64) -> Result<OutboxEntry, EmailSyncError>;

    /// Withdraw a not-yet-delivered entry (`PendingApproval` /
    /// `Approved` / `Failed` → `Cancelled`). Publishes
    /// `OutboxChanged`. Cancelling a terminal entry is an error.
    fn cancel(&self, account: &str, id: u64) -> Result<OutboxEntry, EmailSyncError>;
}
