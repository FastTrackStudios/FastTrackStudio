//! Org-wide presence — the fixed doc id + timeout for the
//! Discord-style "who's online" channel.
//!
//! Each org has its own `LayerRouter` + [`crate::OrgAppState`], so a
//! single fixed doc id works: every client of an org joins the same
//! `DocPresence` channel, and orgs never share a host. The
//! [`crdt::sync::PresenceHost`] is not coupled to any `CrdtDoc` — it is
//! just a mirror `EphemeralStore` + fan-out, so no document backs this
//! id.
//!
//! NOTE: the constant is **duplicated** in
//! `crates/ui/src/presence.rs` (`ui::presence::PRESENCE_DOC_ID`) — the
//! UI crate can't depend on the server binary and no shared proto crate
//! fits a transport-level constant today. Keep the two in sync.

use uuid::Uuid;

/// The org-wide presence channel's doc id. ASCII `task-org-presenc`
/// as a u128 — stable, greppable, and never colliding with a real
/// (random v4) doc id.
pub const PRESENCE_DOC_ID: Uuid = Uuid::from_u128(0x7461736b_2d6f_7267_2d70_726573656e63);

/// How long a peer's state survives without an update before the host
/// considers it gone (Loro's ephemeral timeout). Mirrors the client's
/// `use_presence_channel(PRESENCE_DOC_ID, 30_000)`.
pub const PRESENCE_TIMEOUT_MS: i64 = 30_000;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn doc_id_spells_task_org_presenc() {
        assert_eq!(&PRESENCE_DOC_ID.as_bytes()[..], b"task-org-presenc");
    }
}
