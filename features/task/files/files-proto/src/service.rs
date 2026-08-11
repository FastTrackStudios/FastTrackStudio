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

use crate::model::{BrowseEntry, ChainEntry, CheckpointInfo, FileRootInfo, SnapshotInfo};

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
    /// The cadence engine took an ephemeral auto-snapshot during
    /// activity (issue #260). Not a version — see [`SnapshotInfo`].
    Snapshotted(SnapshotInfo),
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
    /// explicit-trigger half of "Session checkpoint" — the other half
    /// is per-root quiescence, driven by the cadence engine of issue
    /// #260): full-scan the root's live tree, diff against the current
    /// head, and write one commit. `description` defaults to
    /// `"checkpoint now"` when `None`. Ends the root's open session, so
    /// a quiescence checkpoint never lands straight on top of an
    /// explicit one.
    async fn checkpoint_now(
        &self,
        root_id: Uuid,
        description: Option<String>,
    ) -> Result<CheckpointInfo, FilesError>;

    /// Feed the cadence engine activity hints for `root_id` — the
    /// root-relative paths a watcher saw written (issue #260). Hints
    /// are exactly that: they open/extend a session and mark save
    /// points, but nothing they claim is trusted as content — a full
    /// stat-scan certifies every capture. The server-side watcher calls
    /// the same engine path; this method exists so a sync daemon (or a
    /// DAW-side integration that knows it just saved) can report
    /// activity the server can't see. Paths matching the root's Ignore
    /// set are dropped; the return value is how many hints survived
    /// that filter.
    async fn hint_activity(&self, root_id: Uuid, paths: Vec<String>) -> Result<u32, FilesError>;

    /// The root's auto-snapshots (glossary), newest first — the
    /// ephemeral captures a mid-session mistake is recovered from.
    /// Never version-chain entries.
    async fn snapshots(&self, root_id: Uuid) -> Result<Vec<SnapshotInfo>, FilesError>;

    /// The root's Ignore set (glossary): the patterns that are neither
    /// versioned nor synced, seeded from the root's flavor at creation.
    /// Glob syntax, matched against the root-relative path and against
    /// the basename alone (so `*.rpp-bak` catches one at any depth).
    async fn ignore_set(&self, root_id: Uuid) -> Result<Vec<String>, FilesError>;

    /// Replace the root's Ignore set, returning the stored result
    /// (normalized: trimmed, deduplicated, sorted). Fails with
    /// [`FilesError::BadRequest`] if any pattern is not a valid glob.
    /// Already-versioned paths that a new pattern now covers are not
    /// retroactively removed from history — the set governs what enters
    /// the store from here on.
    async fn set_ignore_set(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, FilesError>;

    /// Every root-creation / checkpoint / snapshot event, as it happens.
    #[subscribe]
    fn events(&self) -> FilesEvent;
}
