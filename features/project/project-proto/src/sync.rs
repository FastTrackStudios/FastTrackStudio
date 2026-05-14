//! `WorkspaceSync` — Loro-CRDT-over-vox transport.
//!
//! The server side bridges its workspace `LoroDoc`'s
//! `subscribe_local_update` into a `Tx<UpdateBytes>`; clients hold
//! their own `LoroDoc` and merge updates as they arrive.
//! `apply_update` is the reverse direction — clients push their
//! committed-local bytes back up.

use facet::Facet;
use thiserror::Error;
#[cfg(feature = "vox")]
use vox::Tx;

/// One opaque chunk of Loro update bytes. Newtype because the orphan
/// rule blocks `impl vox_types::Reborrow for Vec<u8>` from outside
/// the vox-types crate, and `Tx<T>` requires `T: Reborrow` on the
/// recv side. Cheap: just wraps a `Vec<u8>`.
#[derive(Debug, Clone, Facet)]
pub struct UpdateBytes(pub Vec<u8>);

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for UpdateBytes {
    type Ref<'a> = UpdateBytes;
}

#[derive(Debug, Clone, PartialEq, Eq, Facet, Error)]
#[repr(u8)]
pub enum SyncError {
    #[error("invalid update: {0}")]
    InvalidUpdate(String),
    #[error("internal: {0}")]
    Internal(String),
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait WorkspaceSync {
    /// Push the bytes of a locally-committed update up to the server.
    /// The server imports + merges, then fans the same bytes out to
    /// every other subscriber via its broadcast channel.
    async fn apply_update(&self, update: UpdateBytes) -> Result<(), SyncError>;

    /// Subscribe to the workspace doc's update stream. The server's
    /// first send is a `Snapshot` export so the client catches up;
    /// every subsequent send is the bytes of one committed change.
    /// The call returns when the client drops the receiving end of
    /// `output` or the server shuts down.
    async fn subscribe(&self, output: Tx<UpdateBytes>);
}
