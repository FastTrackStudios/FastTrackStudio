//! `VaultSync` — the canonical sync trait, decorated with
//! `#[architect::rpc]`.
//!
//! The macro derives the async vox face from this sync trait:
//! backends impl `VaultSync` directly (zero-cost in-process call
//! sites), and remote callers reach the same surface via the
//! auto-emitted [`VaultSyncClient`] over vox. See
//! `architect/DESIGN.md`.
//!
//! Backends carry whatever state they need (the server-side
//! `VaultSyncState` holds the filesystem root + per-vault
//! broadcast channels), and additionally implement
//! [`architect::HasDispatcher`] so the bridge knows how to
//! marshal sync method calls onto the right thread —
//! `TokioBlockingDispatcher` for the server, `CurrentThread`
//! for tests / in-process callers.

use crate::{FileBytes, FolderIndex, IfMatch, Manifest, PutAck, VaultEvent, VaultSyncError};
use vox::Tx;

/// File-replication operations on a single server. Sync methods
/// (cheap when called in-process; marshaled through the
/// backend's `HasDispatcher` for remote callers).
/// [`Self::subscribe`] is async because the broadcast stream
/// can't be expressed in a sync signature.
#[architect::rpc]
pub trait VaultSync {
    /// List every file in `vault_id`. Empty vault = empty list,
    /// not an error.
    fn manifest(&self, vault_id: &str) -> Result<Manifest, VaultSyncError>;

    /// Read one file's bytes. Returns
    /// [`VaultSyncError::NotFound`] for missing paths.
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

    /// Frontmatter-derived metadata for every `.md` page —
    /// path, basename, title, type, and the `folder` parent
    /// (Obsidian folder-note wikilink, resolved to a basename).
    /// Powers the virtual-folder sidebar without the client
    /// fetching + parsing each file.
    fn folder_index(&self, vault_id: &str) -> Result<FolderIndex, VaultSyncError>;

    /// Re-file a note: set or clear its `folder` frontmatter
    /// property. `parent` is the target folder note's basename,
    /// or `None` to move the note to the root. The edit is a
    /// surgical frontmatter splice (other properties + key order
    /// preserved). Honors `if_match` like [`Self::put_file`] and
    /// returns the freshly-committed sha.
    fn set_folder(
        &self,
        vault_id: &str,
        path: &str,
        parent: Option<String>,
        if_match: IfMatch,
    ) -> Result<PutAck, VaultSyncError>;

    /// Subscribe to live change events for `vault_id`. The
    /// server keeps sending until the caller drops `tx`. On
    /// broadcast-lag the server sends [`VaultEvent::Resync`]
    /// and continues — clients should re-pull the manifest in
    /// response.
    async fn subscribe(&self, vault_id: String, tx: Tx<VaultEvent>);
}
