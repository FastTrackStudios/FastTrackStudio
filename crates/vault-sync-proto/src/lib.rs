//! `vault-sync-proto` — wire contract for the vault-sync feature.
//!
//! Replaces the standalone HTTP + WS surface that lived in
//! `apps/server/src/vault_sync.rs` with a `#[vox::service]` trait
//! mounted on the same `/vox` route every other architect /
//! vox-shaped feature uses. Native + wasm clients consume the
//! generated `VaultSyncClient` directly; no per-transport client
//! crates required.
//!
//! Surface:
//! - [`VaultSync::manifest`] — list every file in a vault (path,
//!   sha256, mtime, size).
//! - [`VaultSync::get_file`] — read one file's bytes by relative
//!   path.
//! - [`VaultSync::put_file`] — write one file with an
//!   [`IfMatch`] guard. On conflict the server returns the
//!   current sha + bytes inside [`VaultSyncError::Conflict`] so
//!   callers can run a 3-way merge without a follow-up GET.
//! - [`VaultSync::delete_file`] — remove a file, same `IfMatch`
//!   semantics.
//! - [`VaultSync::subscribe`] — stream every subsequent
//!   [`VaultEvent`] for a vault. First send replays nothing; the
//!   caller is expected to pull a fresh manifest before
//!   subscribing.
//!
//! Conflict policy: **last-writer-wins with `IfMatch`**.
//! - `IfMatch::CreateOnly` — fail if the path already exists.
//! - `IfMatch::Sha(_)`     — fail if the server's current sha
//!                            differs.
//! - `IfMatch::Force`      — unconditional. Only safe for the
//!                            very first push of a brand-new
//!                            vault.

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

/// Single-arg wrapper for [`VaultSync::manifest`] and
/// [`VaultSync::subscribe`]. Vox method args are one typed value;
/// the wrapper gives that value a stable wire name.
#[derive(Debug, Clone, Facet)]
pub struct VaultIdArg {
    pub vault_id: String,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for VaultIdArg {
    type Ref<'a> = VaultIdArg;
}

/// Server → client. Full file listing for one vault. `vault_id`
/// echoes the request so a client juggling several vaults can
/// double-check what came back.
#[derive(Debug, Clone, Facet)]
pub struct Manifest {
    pub vault_id: String,
    pub files: Vec<ManifestEntry>,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for Manifest {
    type Ref<'a> = Manifest;
}

/// Path-addressed read.
#[derive(Debug, Clone, Facet)]
pub struct GetFileArg {
    pub vault_id: String,
    pub path: String,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for GetFileArg {
    type Ref<'a> = GetFileArg;
}

/// Path-addressed write. `bytes` is the file body. `if_match`
/// guards against blind overwrites.
#[derive(Debug, Clone, Facet)]
pub struct PutFileArg {
    pub vault_id: String,
    pub path: String,
    pub bytes: Vec<u8>,
    pub if_match: IfMatch,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for PutFileArg {
    type Ref<'a> = PutFileArg;
}

/// Path-addressed delete. Same conflict-guard rules as
/// [`PutFileArg`].
#[derive(Debug, Clone, Facet)]
pub struct DeleteFileArg {
    pub vault_id: String,
    pub path: String,
    pub if_match: IfMatch,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for DeleteFileArg {
    type Ref<'a> = DeleteFileArg;
}

/// Server → client. Bytes payload for [`VaultSync::get_file`].
/// Newtype so vox has a named wire type to bind to.
#[derive(Debug, Clone, Facet)]
pub struct FileBytes(pub Vec<u8>);

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for FileBytes {
    type Ref<'a> = FileBytes;
}

/// Server → client. The freshly-committed file's sha + mtime
/// after a successful PUT. The sha lets the caller record the
/// new "last-known-server" value without a follow-up GET.
#[derive(Debug, Clone, Facet)]
pub struct PutAck {
    pub sha256: String,
    pub mtime_ms: i64,
}

#[cfg(feature = "vox")]
unsafe impl vox_types::Reborrow for PutAck {
    type Ref<'a> = PutAck;
}

/// Conditional-write modes. Wire-level mirror of the old HTTP
/// `If-Match` header semantics:
/// - `CreateOnly` ↔ `If-Match: *`
/// - `Sha(hex)`   ↔ `If-Match: <hex>`
/// - `Force`      ↔ no `If-Match` header (unconditional).
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

/// Errors at the trait boundary. `Conflict` carries the
/// server's current sha + bytes inline so callers can resolve
/// without a second round-trip.
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

#[cfg_attr(feature = "vox", vox::service)]
pub trait VaultSync {
    /// List every file in `vault_id`. Empty vault = empty list,
    /// not an error.
    async fn manifest(&self, req: VaultIdArg) -> Result<Manifest, VaultSyncError>;

    /// Read one file's bytes. Returns
    /// `VaultSyncError::NotFound` for missing paths.
    async fn get_file(&self, req: GetFileArg) -> Result<FileBytes, VaultSyncError>;

    /// Write one file. Honors `if_match`; on conflict the
    /// returned error carries the server's current sha + bytes.
    async fn put_file(&self, req: PutFileArg) -> Result<PutAck, VaultSyncError>;

    /// Remove one file. Idempotent: deleting a missing path
    /// succeeds.
    async fn delete_file(&self, req: DeleteFileArg) -> Result<(), VaultSyncError>;

    /// Subscribe to live change events for `vault_id`. The
    /// server keeps sending until the caller drops `output`.
    /// On broadcast-lag the server sends [`VaultEvent::Resync`]
    /// and continues — clients should re-pull the manifest in
    /// response.
    async fn subscribe(&self, req: VaultIdArg, output: Tx<VaultEvent>);
}
