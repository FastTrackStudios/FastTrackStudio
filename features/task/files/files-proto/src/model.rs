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
    /// Names of every [`NamedVersion`] the Vault holds against this
    /// entry's commit — the curated metadata layered on top of the
    /// automatic chain (issue #261). Empty for an ordinary,
    /// un-curated Session checkpoint. The store itself knows nothing
    /// about names; this is resolved from the Vault on every read.
    pub names: Vec<String>,
}

/// A **Named Version** (glossary): a user-facing, deliberately labeled
/// version of a deliverable ("v3 for client"), curated on top of the
/// automatic chain. A Vault entity — a markdown page with frontmatter
/// under the org vault — referencing `(root id, change id)`; ADR 0001:
/// "the version store knows nothing about names".
///
/// Both ids are recorded: `change_id` is jj's stable, rewrite-surviving
/// identity (what the reference *is*), `commit_id` the exact content
/// pointer it resolved to when named (what GC protects and what a
/// share link streams). See
/// [`crate::service::FilesService::resolve_named_version`].
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct NamedVersion {
    pub id: Uuid,
    /// Vault-relative path of the entity's own page. Empty on input to
    /// [`crate::service::FilesService::name_version`]; the server fills
    /// it in.
    pub path: String,
    /// The curated label, as the producer typed it ("v3 for client").
    pub name: String,
    pub root_id: Uuid,
    /// Hex-encoded jj `ChangeId`.
    pub change_id: String,
    /// Hex-encoded jj `CommitId`.
    pub commit_id: String,
    /// The page body — free-form producer notes about this version.
    pub note: String,
    pub created_at: DateTime<Utc>,
}

/// A **Project Version** (glossary): a whole-project iteration of one
/// File Root — the same root with a new lineage, replacing the
/// "Project Title old" / "Project Title NEW final2" folder idiom. The
/// folder name never changes. Auto-numbered from 1 with an optional
/// label; a Vault entity like [`NamedVersion`], referencing the commit
/// the iteration starts from.
///
/// This ticket (#261) is the entity plus its numbering — the restart
/// flow that actually flips a root's live tree is #268.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct ProjectVersion {
    pub id: Uuid,
    /// Vault-relative path of the entity's own page (server-filled).
    pub path: String,
    pub root_id: Uuid,
    /// Auto-assigned, 1-based, per root — never reused.
    pub number: u32,
    /// Optional producer label ("Client remix").
    pub label: Option<String>,
    /// Hex-encoded jj `ChangeId` of the commit this iteration starts
    /// from.
    pub change_id: String,
    /// Hex-encoded jj `CommitId` of that same commit.
    pub commit_id: String,
    pub started_at: DateTime<Utc>,
}

/// What a Vault version reference resolves to in the store right now —
/// the answer a share link targeting a Named Version needs before it
/// can stream anything (glossary "Share link": target enum
/// `Note | Slice | Named Version | Review`).
///
/// `commit_id` is the exact change the reference names: resolved from
/// the entity's `change_id` through the root's index when possible, so
/// a rewritten change still lands on its current commit, and falling
/// back to the recorded `commit_id` otherwise.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct VersionRef {
    pub root_id: Uuid,
    /// Hex-encoded jj `ChangeId` — the stable half of the reference.
    pub change_id: String,
    /// Hex-encoded jj `CommitId` this reference resolves to now.
    pub commit_id: String,
}

/// Result of [`crate::service::FilesService::gc_root`] — one
/// mark-and-sweep pass over a root's version store, with the protect
/// set resolved from the Vault (ADR 0001: "protect set =
/// index-reachable ∪ Vault-referenced ... the Vault is the authority
/// on immortality").
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize, Facet)]
#[repr(C)]
pub struct GcReport {
    /// Tree/commit/copy-history objects removed.
    pub objects_swept: u64,
    /// Chunk-store manifests removed. Their now-unreferenced chunks are
    /// reclaimed on the chunk store's own background schedule.
    pub manifests_swept: u64,
    /// How many commits the Vault protected this pass — the Named
    /// Version and Project Version entities pointing at this root.
    pub protected_commits: u32,
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
