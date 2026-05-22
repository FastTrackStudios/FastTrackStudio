//! Live change events streamed to subscribers of
//! [`crate::VaultSync::subscribe`].
//!
//! Every successful PUT or DELETE on the vault publishes one
//! [`VaultEvent`] to every connected subscriber. If a subscriber
//! falls behind (broadcast channel cap exceeded), the server
//! sends [`VaultEvent::Resync`] instead of replaying — clients
//! re-pull the manifest in response.

use facet::Facet;

/// Single change-event on a vault.
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
#[repr(u8)]
pub enum VaultEvent {
    /// File was created or modified. Clients can skip the pull
    /// when their local sha already matches (echo from their
    /// own push).
    Put {
        path: String,
        sha256: String,
        mtime_ms: i64,
        size: u64,
    },
    /// File was removed.
    Delete { path: String },
    /// Server hint after a broadcast-lag — re-pull the manifest
    /// to catch missed events. Sent in lieu of replay; the
    /// connection itself stays open.
    Resync,
}

#[cfg(feature = "vox")]
#[allow(unsafe_code)]
mod reborrow_impls {
    use super::VaultEvent;
    unsafe impl vox_types::Reborrow for VaultEvent {
        type Ref<'a> = VaultEvent;
    }
}
