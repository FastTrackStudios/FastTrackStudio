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

/// Listing response — wrapper around `Vec<DocId>` so vox's wire
/// codec has a named type to bind to.
#[derive(Debug, Clone, Facet)]
pub struct DocList {
    pub doc_ids: Vec<DocId>,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for DocList {
    type Ref<'a> = DocList;
}

/// Trivial unit-arg wrapper. `list_docs` takes no inputs but vox
/// methods need at least one typed argument.
#[derive(Debug, Clone, Facet)]
pub struct ListDocsRequest;

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for ListDocsRequest {
    type Ref<'a> = ListDocsRequest;
}

/// Phase 10 — `subscribe_kinds` arg. `kinds` is the set of root
/// container names the subscriber cares about. Empty list = all
/// kinds (back-compat path, same as the legacy `subscribe`).
#[derive(Debug, Clone, Facet)]
pub struct KindFilter {
    pub doc_id: DocId,
    pub kinds: Vec<String>,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for KindFilter {
    type Ref<'a> = KindFilter;
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

/// Awareness payload — encoded `EphemeralStore` bytes from one
/// peer announcing its current cursor / selection / presence
/// state. Opaque on the wire; clients decode with the matching
/// `EphemeralStore::apply()`.
///
/// Shipped via [`WorkspaceSync::publish_awareness`] (out) +
/// [`WorkspaceSync::subscribe_awareness`] (in). Server holds
/// one `EphemeralStore` per `doc_id` and fans out to every
/// subscriber for that doc.
#[derive(Debug, Clone, Facet)]
pub struct AwarenessFrame {
    /// Source peer — clients use this to colorize remote
    /// cursors and to suppress their own echo when they receive
    /// their own published state back.
    pub from_peer: ::uuid::Uuid,
    /// `EphemeralStore` payload (one or more keys' encoded
    /// bytes). Apply to a local store to merge.
    pub bytes: Vec<u8>,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for AwarenessFrame {
    type Ref<'a> = AwarenessFrame;
}

/// Subscription request — `(doc_id, peer_id)`. Server skips
/// echoing the subscriber's own published frames back to them.
#[derive(Debug, Clone, Facet)]
pub struct AwarenessSubscribe {
    pub doc_id: DocId,
    pub peer_id: ::uuid::Uuid,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for AwarenessSubscribe {
    type Ref<'a> = AwarenessSubscribe;
}

/// Publish envelope — pairs the awareness frame with the doc
/// it belongs to.
#[derive(Debug, Clone, Facet)]
pub struct AwarenessPublish {
    pub doc_id: DocId,
    pub frame: AwarenessFrame,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for AwarenessPublish {
    type Ref<'a> = AwarenessPublish;
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

    /// Enumerate every doc id this server is currently hosting.
    /// Phase 8 — used by the federation UI to discover what's on
    /// each connected server. Returns docs the caller's
    /// capability scope can read; when no scope is enforced, all
    /// currently-open docs are returned. Newly-created docs that
    /// haven't been opened yet on the server may not appear.
    async fn list_docs(&self, _req: ListDocsRequest) -> Result<DocList, SyncError>;

    /// Phase 10 — same shape as `subscribe`, but the server only
    /// forwards updates whose touched root containers intersect
    /// the subscriber's `kinds` filter. Empty `kinds` = forward
    /// every update (back-compat path).
    ///
    /// Useful when a client only renders one entity kind — e.g.
    /// a kanban viewing only `tasks` can avoid receiving the
    /// `knowledge_blocks` byte stream from a busy editor.
    async fn subscribe_kinds(&self, filter: KindFilter, output: Tx<UpdateBytes>);

    /// Subscribe to awareness updates for a doc. The server
    /// forwards every other peer's `AwarenessFrame` for this
    /// doc as it's published. First send is the current snapshot
    /// (every active peer's state) so late joiners see the
    /// existing cursors immediately.
    ///
    /// The subscriber's own `peer_id` is filtered server-side
    /// so clients don't have to suppress their own echo.
    async fn subscribe_awareness(&self, sub: AwarenessSubscribe, output: Tx<AwarenessFrame>);

    /// Publish a local awareness update to peers subscribed to
    /// the same doc. Server merges into its per-doc
    /// `EphemeralStore` (so future joiners see the fresh state)
    /// and forwards the bytes to every other subscriber.
    async fn publish_awareness(&self, msg: AwarenessPublish) -> Result<(), SyncError>;
}
