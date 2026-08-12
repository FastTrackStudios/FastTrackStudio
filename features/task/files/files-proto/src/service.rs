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

use crate::model::{
    BrowseEntry, ChainEntry, CheckpointInfo, FileRootInfo, GcReport, HydrationChange,
    HydrationReport, NamedVersion, ProjectVersion, SnapshotInfo, VersionRef,
};

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
    /// A version was curated with a name (issue #261).
    VersionNamed(NamedVersion),
    /// A Named Version's curation was dropped — the entity that was
    /// removed, so a subscriber can drop it by id without a refetch.
    VersionUnnamed(NamedVersion),
    /// A new Project Version of a root was started.
    ProjectVersionStarted(ProjectVersion),
    /// A file's live-tree content was replaced by a pointer stub, or a
    /// stub was restored to resident content (issue #263).
    HydrationChanged(HydrationChange),
}

#[architect::rpc]
pub trait FilesService {
    /// Turn an existing folder into a File Root: writes the marker
    /// file, mints a stable id, and initializes its version store.
    /// Fails with [`FilesError::AlreadyExists`] if `path` is already a
    /// root, and with [`FilesError::BadRequest`] if `path` doesn't
    /// exist or isn't a directory.
    ///
    /// `flavor` picks the versioning engine and is fixed for the root's
    /// life (see [`crate::model::RootFlavor`]).
    /// [`RootFlavor::Software`](crate::model::RootFlavor::Software)
    /// initializes a colocated git repository in the folder — or
    /// *adopts* the one already there, keeping its history and remotes
    /// (issue #273).
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

    /// The root's **editable** Ignore-set patterns (glossary: the
    /// patterns that are neither versioned nor synced). Gitignore
    /// syntax. These are layered *on top of* the root's flavor seed —
    /// REAPER backup/peak churn on a media root, build scaffolding and
    /// stray heavy media on a software root — which is why an untouched
    /// root reports an empty list while still ignoring plenty.
    async fn ignore_set(&self, root_id: Uuid) -> Result<Vec<String>, FilesError>;

    /// Replace the root's editable Ignore-set patterns, returning them
    /// as stored (trimmed and deduplicated; order is preserved, because
    /// in gitignore the last matching rule wins and a `!` re-include
    /// must be able to follow what it re-includes). Fails with
    /// [`FilesError::BadRequest`] on a pattern carrying a line break or
    /// written as a comment — both would mean something other than the
    /// one rule it claims to be.
    ///
    /// Already-versioned paths that a new pattern now covers are not
    /// retroactively removed from history — an Ignore set governs what
    /// *starts* being versioned; it never ends it.
    async fn set_ignore_set(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, FilesError>;

    /// Curate `commit_id` in `root_id` as a [`NamedVersion`] — writes
    /// the Vault entity that references `(root id, change id)`. The
    /// version store is not touched: naming is Vault-side curation
    /// (ADR 0001). Fails with [`FilesError::NotFound`] when `root_id`
    /// is unknown or `commit_id` names no commit in that root's store,
    /// and with [`FilesError::AlreadyExists`] when this root already
    /// has a Named Version by that name.
    async fn name_version(
        &self,
        root_id: Uuid,
        commit_id: String,
        name: String,
    ) -> Result<NamedVersion, FilesError>;

    /// Every Named Version the Vault holds, newest first. `root_id`
    /// filters to one root; `None` lists the whole org's.
    async fn list_named_versions(
        &self,
        root_id: Option<Uuid>,
    ) -> Result<Vec<NamedVersion>, FilesError>;

    /// What a Named Version points at in the store *right now* — the
    /// resolution step a share link targeting it performs before
    /// streaming (glossary "Share link"). Fails with
    /// [`FilesError::NotFound`] when the entity is gone or its change
    /// resolves to nothing in the root's store.
    async fn resolve_named_version(&self, id: Uuid) -> Result<VersionRef, FilesError>;

    /// Drop a Named Version's curation (the Vault entity), leaving the
    /// automatic chain untouched. Its content stops being immortal at
    /// the next [`FilesService::gc_root`].
    async fn unname_version(&self, id: Uuid) -> Result<(), FilesError>;

    /// Start a new [`ProjectVersion`] of `root_id` from its current
    /// checkpoint head: writes the Vault entity with the next
    /// auto-assigned number and an optional `label`. The live tree is
    /// untouched — the restart flow that carries files forward is
    /// #268.
    async fn start_project_version(
        &self,
        root_id: Uuid,
        label: Option<String>,
    ) -> Result<ProjectVersion, FilesError>;

    /// Every Project Version of `root_id`, oldest number first.
    async fn list_project_versions(&self, root_id: Uuid)
    -> Result<Vec<ProjectVersion>, FilesError>;

    /// Run one GC pass over `root_id`'s version store with the protect
    /// set resolved from the Vault: everything Named/Project Versions
    /// reference is immortal regardless of age, on top of jj's own
    /// index reachability.
    ///
    /// [`RootFlavor::Media`](crate::model::RootFlavor::Media) only. A
    /// software root's objects belong to its colocated git repository,
    /// which collects its own garbage (`git gc`); calling this on one
    /// fails with [`FilesError::BadRequest`] rather than reaching into
    /// a store Files does not own. Naming and Project Versions have no
    /// such split — curation is flavor-agnostic. `keep_newer_secs` guards concurrent writers
    /// by refusing to sweep anything written within that many seconds
    /// (default 60).
    ///
    /// The pass holds the root's write lock, so writers *in this
    /// process* are already excluded and the guard is really about a
    /// second process on the same store (a CLI's embedded backend, a
    /// storage agent). `Some(0)` therefore disables the only
    /// protection those writers have: pass it when you know nothing
    /// else holds this root — a test, or a maintenance window — and
    /// never on a live rig.
    async fn gc_root(
        &self,
        root_id: Uuid,
        keep_newer_secs: Option<u64>,
    ) -> Result<GcReport, FilesError>;

    /// Replace `path`'s live-tree content with a **pointer stub**
    /// (glossary: a small placeholder standing in for non-resident
    /// content — dehydration). The content itself stays in the version
    /// store; listings keep reporting the file with its logical size
    /// and identity. Media roots only (a software root's working tree
    /// belongs to its colocated git, same split as
    /// [`FilesService::gc_root`]).
    ///
    /// Refuses with [`FilesError::BadRequest`] when the file's on-disk
    /// content differs from the checkpoint head — dehydration never
    /// destroys unversioned work; checkpoint first. A path that is
    /// already a stub returns its current entry unchanged (idempotent).
    async fn dehydrate(&self, root_id: Uuid, path: String) -> Result<BrowseEntry, FilesError>;

    /// Restore `path`'s exact content over its stub, streamed from the
    /// version store and **verified by `FileId`** before it replaces
    /// the stub. A path that is already resident returns its current
    /// entry unchanged (idempotent). Fails with
    /// [`FilesError::NotFound`] when the stub's content is not in the
    /// store (a partial replica missing chunks hydrates through sync,
    /// issue #264).
    async fn hydrate(&self, root_id: Uuid, path: String) -> Result<BrowseEntry, FilesError>;

    /// The root's hydration-policy patterns (gitignore syntax, same
    /// dialect as the Ignore set). A path **matching** the policy is
    /// kept hydrated by [`FilesService::apply_hydration_policy`];
    /// everything else is kept dehydrated. Empty (the default) means no
    /// automatic dehydration ever — policy is opt-in per root.
    async fn hydration_policy(&self, root_id: Uuid) -> Result<Vec<String>, FilesError>;

    /// Replace the root's hydration-policy patterns, returning them as
    /// stored (same trim/dedup/order rules as
    /// [`FilesService::set_ignore_set`], for the same last-match-wins
    /// reason). Storing does not touch any file —
    /// [`FilesService::apply_hydration_policy`] is the pass that does.
    async fn set_hydration_policy(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, FilesError>;

    /// Run the root's hydration policy over its live tree now: hydrate
    /// every stub the patterns match, dehydrate every clean resident
    /// file they don't. An empty policy hydrates nothing and — being
    /// opt-in — dehydrates nothing. Dirty files (content differing from
    /// the checkpoint head) are never dehydrated; they come back in the
    /// report so the caller can checkpoint and re-apply.
    async fn apply_hydration_policy(&self, root_id: Uuid) -> Result<HydrationReport, FilesError>;

    /// Every root-creation / checkpoint / version-curation / hydration
    /// event, as it happens.
    #[subscribe]
    fn events(&self) -> FilesEvent;
}
