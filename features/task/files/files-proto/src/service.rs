//! `FilesService` — the Files RPC surface v1 (issue #259): create a File
//! Root from an existing folder, browse (root-scoped and rootless
//! "Drive"), derive a file's version chain, and trigger a Session
//! checkpoint on demand. `#[subscribe] fn events` is how a checkpoint
//! (or a new root) appears without polling.
//!
//! Every method here is 4 params or fewer (Facet's `#[architect::rpc]`
//! constraint, per the monorepo's root CLAUDE.md).

use facet::Facet;
use serde::{Deserialize, Serialize};
use thiserror::Error;
use uuid::Uuid;

use crate::model::{BrowseEntry, ChainEntry, CheckpointInfo, FileRootInfo};

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet, Error)]
#[repr(u8)]
pub enum FilesError {
    #[error("not found: {0}")]
    NotFound(String),
    #[error("already exists: {0}")]
    AlreadyExists(String),
    #[error("bad request: {0}")]
    BadRequest(String),
    #[error("io: {0}")]
    Io(String),
}

/// Live-update payload for [`FilesService::events`]. Fetch current state
/// once via `list_roots`/`chain` (after subscribing, so nothing is
/// missed in between), then fold these in — same no-snapshot-variant
/// contract as `task_proto::TaskEvent` / `timer_proto::TimerEvent`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum FilesEvent {
    /// A new File Root was created.
    RootCreated(FileRootInfo),
    /// A root's session ended in a certified checkpoint.
    Checkpointed(CheckpointInfo),
}

#[architect::rpc]
pub trait FilesService {
    /// Turn an existing folder into a File Root: writes the marker
    /// file, mints a stable id, and initializes its version store.
    /// Fails with [`FilesError::AlreadyExists`] if `path` is already a
    /// root, and with [`FilesError::BadRequest`] if `path` doesn't
    /// exist or isn't a directory. `flavor` is accepted for wire
    /// stability but only `RootFlavor::Media` is implemented in v1 —
    /// `RootFlavor::Software` fails with [`FilesError::BadRequest`]
    /// (ADR 0001: software roots are colocated git, a distinct build).
    async fn create_root(
        &self,
        path: String,
        name: String,
        flavor: crate::model::RootFlavor,
    ) -> Result<FileRootInfo, FilesError>;

    /// Every File Root known to this org.
    async fn list_roots(&self) -> Result<Vec<FileRootInfo>, FilesError>;

    /// One root by id.
    async fn get_root(&self, id: Uuid) -> Result<FileRootInfo, FilesError>;

    /// List the direct children of `subpath` inside `root_id`'s live
    /// tree ("root browsing" — distinct from [`FilesService::drive_browse`]
    /// per the glossary). Empty `subpath` lists the root itself. Fails
    /// with [`FilesError::BadRequest`] if `subpath` escapes the root
    /// (`..` components) or names a file rather than a directory.
    async fn browse(&self, root_id: Uuid, subpath: String) -> Result<Vec<BrowseEntry>, FilesError>;

    /// List the direct children of an arbitrary filesystem path with no
    /// root context — "Drive" browsing (glossary: loose files outside
    /// any root). Never touches the version store.
    async fn drive_browse(&self, path: String) -> Result<Vec<BrowseEntry>, FilesError>;

    /// Derive `path`'s version chain (newest first) from `root_id`'s
    /// current checkpoint head, following recorded renames. Empty when
    /// `path` has never been checkpointed.
    async fn chain(&self, root_id: Uuid, path: String) -> Result<Vec<ChainEntry>, FilesError>;

    /// Scan-certify a Session checkpoint right now (glossary: the
    /// explicit-trigger half of "Session checkpoint" — the
    /// quiescence/debounce cadence engine is future work): full-scan
    /// the root's live tree, diff against the current head, and write
    /// one commit. `description` defaults to `"checkpoint now"` when
    /// `None`.
    async fn checkpoint_now(
        &self,
        root_id: Uuid,
        description: Option<String>,
    ) -> Result<CheckpointInfo, FilesError>;

    /// Every root-creation / checkpoint event, as it happens.
    #[subscribe]
    fn events(&self) -> FilesEvent;
}
