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

/// Identifies one collaboration boundary (one `LoroDoc`) on a
/// server. Servers hold many; clients subscribe one at a time.
///
/// String shape (server-local namespace; the server URL already
/// implies the org):
///
/// - `vault/org` — org-wide reference vault
/// - `vault/comms` — comms index
/// - `comms/thread/<uuid>` — one chat/email thread
/// - `project/<uuid>` — one project
/// - `user/<uuid>` — per-user private state
///
/// Free-form to keep the wire format flexible while conventions
/// settle. Callers normalize before lookup.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Facet)]
pub struct DocId(pub String);

impl DocId {
    pub fn new(s: impl Into<String>) -> Self {
        Self(s.into())
    }

    /// `project/<uuid>` — the canonical per-project doc id.
    pub fn project(id: ::uuid::Uuid) -> Self {
        Self(format!("project/{id}"))
    }

    /// `vault/org` — the org reference vault.
    pub fn org_vault() -> Self {
        Self("vault/org".into())
    }

    /// `vault/comms` — the comms vault index.
    pub fn comms_vault() -> Self {
        Self("vault/comms".into())
    }

    /// `comms/thread/<uuid>` — one chat / email thread.
    pub fn comms_thread(id: ::uuid::Uuid) -> Self {
        Self(format!("comms/thread/{id}"))
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for DocId {
    type Ref<'a> = DocId;
}

#[derive(Debug, Clone, PartialEq, Eq, Facet, Error)]
#[repr(u8)]
pub enum SyncError {
    #[error("invalid update: {0}")]
    InvalidUpdate(String),
    #[error("internal: {0}")]
    Internal(String),
    #[error("unknown doc: {0}")]
    UnknownDoc(String),
    #[error("forbidden")]
    Forbidden,
}

#[cfg_attr(feature = "vox", vox::service)]
pub trait WorkspaceSync {
    /// Push the bytes of a locally-committed update to a specific
    /// doc. The server imports + merges, then fans the same bytes
    /// out to every other subscriber of THAT DOC via the per-doc
    /// broadcast channel.
    async fn apply_update(&self, doc_id: DocId, update: UpdateBytes) -> Result<(), SyncError>;

    /// Subscribe to one doc's update stream. First send is a
    /// `Snapshot` export so the client catches up; every subsequent
    /// send is the bytes of one committed change. Returns when the
    /// client drops the receiver or the server shuts down. Subscribing
    /// to a doc the caller has no capability for returns
    /// `SyncError::Forbidden` immediately (without sending anything).
    async fn subscribe(&self, doc_id: DocId, output: Tx<UpdateBytes>);
}
