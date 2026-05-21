//! `vault-sync-proto` — wire contract for the vault-sync feature.
//!
//! One canonical sync trait — [`VaultSync`] — decorated with
//! `#[architect::rpc]`. The macro derives:
//! - `VaultSyncRpc` — the hidden async mirror trait used by vox.
//! - `VaultSyncClient` — async caller proxy for remote consumers
//!   (native + wasm).
//! - `VaultSyncRpcDispatcher` (re-exported as
//!   [`Dispatcher`]) — server-side wrapper that mounts on a vox
//!   router.
//! - `serve(backend)` / `layer(backend)` / [`Service`] — mount
//!   verbs for `architect::Services` + `architect::layers!`
//!   bundles.
//!
//! Backends impl `VaultSync` directly (sync methods, zero-cost
//! in-process call sites). For server use the backend also
//! implements [`architect::HasDispatcher`] returning a real
//! marshaling dispatcher — typically
//! [`architect::dispatch::TokioBlockingDispatcher`] for blocking
//! `std::fs` IO on a tokio server.
//!
//! Conflict policy: **last-writer-wins with `IfMatch`**.
//! - `CreateOnly` — fail if the path already exists.
//! - `Sha(hex)`   — fail unless the server's current sha matches.
//! - `Force`      — unconditional. Only safe for the first push
//!                  of a brand-new vault.

use facet::Facet;
use thiserror::Error;

#[cfg(feature = "vox")]
use vox::Tx;

/// One entry in [`Manifest::files`].
#[derive(Debug, Clone, PartialEq, Eq, Facet)]
pub struct ManifestEntry {
    pub path: String,
    pub sha256: String,
    pub mtime_ms: i64,
    pub size: u64,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for ManifestEntry {
    type Ref<'a> = ManifestEntry;
}

/// Full file listing for one vault. `vault_id` echoes the
/// request so a client juggling several vaults can double-check
/// what came back.
#[derive(Debug, Clone, Facet)]
pub struct Manifest {
    pub vault_id: String,
    pub files: Vec<ManifestEntry>,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for Manifest {
    type Ref<'a> = Manifest;
}

/// Bytes payload returned by [`VaultSync::get_file`]. Newtype so
/// vox has a named wire type to bind to.
#[derive(Debug, Clone, Facet)]
pub struct FileBytes(pub Vec<u8>);

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for FileBytes {
    type Ref<'a> = FileBytes;
}

/// The freshly-committed file's sha + mtime after a successful
/// PUT. The sha lets the caller record the new "last-known-server"
/// value without a follow-up GET.
#[derive(Debug, Clone, Facet)]
pub struct PutAck {
    pub sha256: String,
    pub mtime_ms: i64,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for PutAck {
    type Ref<'a> = PutAck;
}

/// Conditional-write mode for [`VaultSync::put_file`] /
/// [`VaultSync::delete_file`].
#[derive(Debug, Clone, Facet)]
#[repr(u8)]
pub enum IfMatch {
    CreateOnly,
    Sha(String),
    Force,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for IfMatch {
    type Ref<'a> = IfMatch;
}

/// Live change event. Forwarded to every subscriber whenever a
/// PUT or DELETE handler completes successfully.
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
    /// to catch missed events.
    Resync,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for VaultEvent {
    type Ref<'a> = VaultEvent;
}

/// Errors at the trait boundary. `Conflict` carries the server's
/// current sha + bytes inline so callers can resolve without a
/// second round-trip.
#[derive(Debug, Clone, PartialEq, Eq, Facet, Error)]
#[repr(u8)]
pub enum VaultSyncError {
    #[error("not found")]
    NotFound,
    #[error("bad path")]
    BadPath,
    #[error("conflict (server sha {server_sha})")]
    Conflict {
        server_sha: String,
        server_bytes: Vec<u8>,
    },
    #[error("io: {0}")]
    Io(String),
    #[error("internal: {0}")]
    Internal(String),
}

/// Vault file-replication operations. Sync methods (cheap when
/// called in-process; marshaled through the backend's
/// `HasDispatcher` for remote callers). `subscribe` is async
/// because the broadcast stream can't be expressed in a sync
/// signature.
#[architect::rpc]
pub trait VaultSync {
    /// List every file in `vault_id`. Empty vault = empty list,
    /// not an error.
    fn manifest(&self, vault_id: &str) -> Result<Manifest, VaultSyncError>;

    /// Read one file's bytes. Returns
    /// `VaultSyncError::NotFound` for missing paths.
    fn get_file(&self, vault_id: &str, path: &str) -> Result<FileBytes, VaultSyncError>;

    /// Write one file. Honors `if_match`; on conflict the
    /// returned error carries the server's current sha + bytes.
    fn put_file(
        &self,
        vault_id: &str,
        path: &str,
        bytes: Vec<u8>,
        if_match: IfMatch,
    ) -> Result<PutAck, VaultSyncError>;

    /// Remove one file. Idempotent: deleting a missing path
    /// succeeds.
    fn delete_file(
        &self,
        vault_id: &str,
        path: &str,
        if_match: IfMatch,
    ) -> Result<(), VaultSyncError>;

    /// Subscribe to live change events for `vault_id`. The
    /// server keeps sending until the caller drops `tx`. On
    /// broadcast-lag the server sends [`VaultEvent::Resync`]
    /// and continues — clients should re-pull the manifest in
    /// response.
    async fn subscribe(&self, vault_id: String, tx: Tx<VaultEvent>);
}
