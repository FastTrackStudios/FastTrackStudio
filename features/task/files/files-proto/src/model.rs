//! Wire types for the Files RPC surface (issue #259). Vocabulary is the
//! Task glossary (`apps/task/CONTEXT.md`): a **File Root** is a folder
//! tree with its own identity; **browsing** walks a live tree; a **File
//! version chain** is a file's per-saved-state history; a **Session
//! checkpoint** is the durable, chain-visible version minted on demand
//! here (v1: only the explicit "checkpoint now" trigger — the
//! quiescence/debounce cadence engine is future work).

use chrono::{DateTime, Utc};
use facet::Facet;
use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// A File Root's versioning mode, chosen at creation (ADR 0001). Only
/// `Media` is implemented end-to-end by this ticket — `Software` is
/// accepted so the wire shape is stable, but [`crate::FilesService`]'s
/// v1 [`crate::service::FilesService::create_root`] rejects it as
/// unimplemented rather than silently falling back to a media root.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
#[repr(u8)]
pub enum RootFlavor {
    Media,
    Software,
}

/// A File Root: a folder tree with a stable identity (ADR 0001 /
/// glossary "File Root"). `path` is the root's live tree on the
/// storage location hosting it — v1 is single-machine, so this is a
/// plain local filesystem path; the Storage Location registry (ADR's
/// out-of-scope-for-#259 placement axis) is future work.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct FileRootInfo {
    pub id: Uuid,
    pub name: String,
    pub path: String,
    pub flavor: RootFlavor,
    pub created_at: DateTime<Utc>,
}

/// One entry in a directory listing — either a root-scoped
/// [`crate::service::FilesService::browse`] or a rootless
/// [`crate::service::FilesService::drive_browse`] ("Drive" browsing
/// per the glossary: loose files outside any root).
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct BrowseEntry {
    pub name: String,
    pub is_dir: bool,
    /// `None` for directories.
    pub size: Option<u64>,
}

/// One entry in a file's version chain (glossary "File version
/// chain"), newest first — the wire projection of
/// `task_files_version_store::chain::VersionEntry`.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct ChainEntry {
    /// Hex-encoded jj `CommitId` that produced this saved state.
    pub commit_id: String,
    /// The path the file lived at in that commit (chains follow
    /// recorded renames).
    pub path: String,
    /// Hex-encoded jj `FileId` (content address) of this saved state.
    pub file_id: String,
    /// Set when this entry is the commit where the file arrived at
    /// `path` via a recorded rename.
    pub renamed_from: Option<String>,
}

/// Result of [`crate::service::FilesService::checkpoint_now`] (glossary
/// "Session checkpoint").
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct CheckpointInfo {
    pub root_id: Uuid,
    /// Hex-encoded jj `CommitId` of the new checkpoint commit.
    pub commit_id: String,
    pub description: String,
    pub at: DateTime<Utc>,
    /// Root-relative paths written or removed by this checkpoint,
    /// sorted. Empty when the live tree was already identical to the
    /// previous checkpoint (still a valid, if uneventful, checkpoint).
    pub changed_paths: Vec<String>,
}
