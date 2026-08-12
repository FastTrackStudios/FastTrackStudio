//! [`FilesBackend`]: server-side [`FilesService`] impl. Wraps
//! [`Registry`] (root identity) and one
//! `task_files_version_store::VersionStoreBackend`-backed jj repo per
//! root (opened lazily, cached for the process's lifetime — see
//! [`crate::repo_open`]).
//!
//! **All the real work below is synchronous**, driven through
//! `pollster::block_on` wherever it touches `task-files-version-store`
//! (which is itself async). This isn't a style choice: jj-lib's own
//! async fns aren't `Send` on every path (see `repo_open`'s module
//! doc), and `#[architect::rpc]` methods must return a `Send` future —
//! so none of this crate's logic can `.await` jj-lib directly from
//! inside an `async fn` without poisoning the RPC method's future.
//! Every `FilesService` method below runs its sync `*_inner` body on
//! `tokio::task::spawn_blocking` (same convention as `task-server`'s
//! `notifier.rs`/`mcp.rs`) rather than inline on the calling async
//! task — a full-tree scan or a multi-GB checkpoint must not stall the
//! shared runtime's other org RPCs (PR #280 review).
//!
//! **Filesystem confinement.** `create_root` and `drive_browse` accept
//! a caller-supplied path; both are confined to [`FilesBackend::confine_root`]
//! (this org's `<data_root>/orgs/<slug>/files/` — see
//! [`FilesBackend::new`]) rather than the whole server filesystem.
//! `permits.rs` mounts `create_root`/`drive_browse` at plain member
//! tier, same as every other CRUD verb on this router — the intended
//! authorization boundary is "any member of *this* org", not "root on
//! the box", so path arguments must never reach outside this org's own
//! subtree (they could otherwise read/ingest another org's data, since
//! every org's `OrgAppState` shares one `data_root`). A full Storage
//! Location grant model (ADR 0001, out of scope for #259) will
//! eventually make placement an explicit, operator-governed axis; this
//! confinement is the minimum viable stopgap until then. `browse`
//! (root-scoped) is confined the same way, against the *root's own*
//! canonicalized path rather than the whole org tree — see
//! `browse_inner`'s doc for how that also closes the absolute-subpath
//! and symlink-escape holes.

use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use chrono::Utc;
use files_proto::{
    BrowseEntry, ChainEntry, CheckpointInfo, FileRootInfo, FilesError, FilesEvent, FilesService,
    GcReport, HydrationChange, HydrationReport, NamedVersion, ProjectVersion, RootFlavor,
    SavePoint, SnapshotInfo, VersionRef,
};
use jj_lib::backend::{ChangeId, CommitId};
use jj_lib::object_id::{HexPrefix, ObjectId as _, PrefixResolution};
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::repo_path::{RepoPath, RepoPathBuf};
use task_files_version_store::VersionStoreBackend;
use uuid::Uuid;

use crate::badges;
use crate::cadence::journal::{CheckpointRecord, SnapshotRecord};
use crate::cadence::{
    ActivitySink, CadenceConfig, CadenceEngine, Clock, Due, DueKind, Journal, RootWatcher,
    SystemClock,
};
use crate::certify::MidHashHook;
use crate::checkpoint::Capture;
use crate::consts::{GIT_DIR, MARKER_FILE, STORE_DIR};
use crate::error::Error;
use crate::git_root;
use crate::hydration;
use crate::ignore;
use crate::registry::Registry;
use crate::repo_open;
use crate::scan;
use crate::stub;
use crate::versions::VaultVersions;

/// Default `keep_newer` window for [`FilesService::gc_root`]: nothing
/// written in the last minute is ever swept, so a sweep can't race a
/// checkpoint that is mid-write on another connection (the
/// concurrent-writer guard `Backend::gc`'s own contract describes).
const DEFAULT_GC_KEEP_NEWER_SECS: u64 = 60;

/// One root's live jj state: the repo handle (reassigned after every
/// `checkpoint_now`) and its current checkpoint head. `head` is tracked
/// explicitly rather than re-derived from `repo.view().heads()` on
/// every call — see `checkpoint::checkpoint`'s own doc example, which
/// establishes this as the pattern for reading back the commit a
/// checkpoint just produced.
struct RootRuntime {
    repo: Arc<ReadonlyRepo>,
    head: CommitId,
    /// Tip of the auto-snapshot branch hanging off `head`, or `None`
    /// when the session has taken none since the last checkpoint
    /// (issue #260 — snapshots branch off the checkpoint line rather
    /// than extending it, see [`crate::cadence`]).
    snapshot_head: Option<CommitId>,
}

/// Which kind of capture a write is — the one difference that decides
/// what it parents on and how it is recorded (issue #260).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CaptureKind {
    /// Ephemeral auto-snapshot: parented on the snapshot branch, never
    /// a chain entry.
    Snapshot,
    /// Certified Session checkpoint: parented on the checkpoint head.
    Checkpoint,
}

impl From<DueKind> for CaptureKind {
    fn from(kind: DueKind) -> Self {
        match kind {
            DueKind::Snapshot => Self::Snapshot,
            DueKind::Checkpoint => Self::Checkpoint,
        }
    }
}

/// What one performed capture produced — the wire payload plus which
/// kind it was, so [`FilesBackend::tick`] can report a cadence pass.
#[derive(Debug, Clone)]
pub enum Captured {
    Snapshot(SnapshotInfo),
    Checkpoint(CheckpointInfo),
}

/// Exactly the state a watcher hint needs: which roots exist, their
/// Ignore sets, and the cadence engine to report into.
///
/// This is a *slice* of [`FilesBackend`] rather than a clone of it, and
/// deliberately so. A watcher lives in the backend's `watchers` map and
/// its callback holds this sink; handing it a whole backend clone would
/// close a reference cycle (watchers map → watcher → callback → backend
/// clone → the same watchers map `Arc`) that no drop could ever break,
/// so a released org would leak its backend and keep watching (PR #283
/// review). `Hints` holds no watcher map and no driver handle, so the
/// cycle simply does not exist.
struct Hints {
    registry: Arc<Registry>,
    ignores: Arc<Mutex<HashMap<Uuid, Arc<jj_lib::gitignore::GitIgnoreFile>>>>,
    cadence: Arc<CadenceEngine>,
}

impl Hints {
    /// The root's whole Ignore set (flavor seed + its stored patterns),
    /// compiled on first touch and cached.
    fn ignore_of(
        ignores: &Mutex<HashMap<Uuid, Arc<jj_lib::gitignore::GitIgnoreFile>>>,
        root: &FileRootInfo,
    ) -> Result<Arc<jj_lib::gitignore::GitIgnoreFile>, Error> {
        if let Some(set) = ignores
            .lock()
            .expect("ignore cache lock poisoned")
            .get(&root.id)
        {
            return Ok(set.clone());
        }
        let set = ignore::for_root(&repo_open::store_dir(Path::new(&root.path)), root.flavor)?;
        ignores
            .lock()
            .expect("ignore cache lock poisoned")
            .insert(root.id, set.clone());
        Ok(set)
    }

    /// Note `paths` as activity on `root_id`, returning how many
    /// survived the root's Ignore set.
    fn note(&self, root_id: Uuid, paths: &[String]) -> Result<u32, Error> {
        let root = self
            .registry
            .get(root_id)
            .ok_or_else(|| Error::NotFound(root_id.to_string()))?;
        let ignores = Self::ignore_of(&self.ignores, &root)?;
        Ok(self
            .cadence
            .note_activity(root_id, paths, &ignores, root.flavor))
    }
}

/// Watcher hints land here (see [`crate::cadence::watcher`]): the
/// backend is what knows a root's flavor and Ignore set, so it is what
/// turns a raw path list into cadence activity.
impl ActivitySink for Hints {
    fn note_activity(&self, root_id: Uuid, paths: Vec<String>) {
        if let Err(err) = self.note(root_id, &paths) {
            tracing::debug!(%root_id, %err, "files watcher hint dropped");
        }
    }
}

#[derive(Clone, architect::HasDispatcher)]
pub struct FilesBackend {
    data_dir: PathBuf,
    /// Canonicalized once at construction — the boundary `create_root`
    /// / `drive_browse` path arguments must resolve inside (see the
    /// module doc's "Filesystem confinement" section).
    confine_root: PathBuf,
    registry: Arc<Registry>,
    /// The org vault holding the curated version entities (issue
    /// #261). Separate from `data_dir`: a File Root's *content* is
    /// never vault-replicated, but the Named / Project Version pages
    /// that reference it are ordinary vault files, and that is exactly
    /// what carries them offline-first to every device.
    versions: VaultVersions,
    repos: Arc<Mutex<HashMap<Uuid, RootRuntime>>>,
    /// One lock per root, serializing every write that reads this
    /// root's state before changing it: `checkpoint_now` (two
    /// concurrent checkpoints must not both read the same head and
    /// silently orphan one commit — PR #280 review), the curation
    /// writes (two namings must not claim one vault page path), and
    /// `gc_root` (a sweep must not miss a name that lands after it
    /// snapshotted its protect set). Created lazily, never removed
    /// (roots are not deleted in v1).
    root_locks: Arc<Mutex<HashMap<Uuid, Arc<Mutex<()>>>>>,
    /// The cadence state machine (issue #260): when each root's session
    /// snapshots, and when it ends in a checkpoint.
    cadence: Arc<CadenceEngine>,
    /// Per-root Ignore sets, compiled on first touch.
    ignores: Arc<Mutex<HashMap<Uuid, Arc<jj_lib::gitignore::GitIgnoreFile>>>>,
    /// Live filesystem watchers, one per watched root.
    watchers: Arc<Mutex<HashMap<Uuid, RootWatcher>>>,
    /// Set by [`FilesBackend::enable_watching`]: newly created roots
    /// start watched too, rather than only on the next restart.
    watch_new_roots: Arc<std::sync::atomic::AtomicBool>,
    /// The cadence driver task, kept so it can be stopped — see
    /// [`FilesBackend::spawn_cadence_driver`].
    driver: Arc<Mutex<Option<tokio::task::JoinHandle<()>>>>,
    /// Test seam — see [`FilesBackend::set_mid_hash_hook`].
    hook: Arc<Mutex<Option<MidHashHook>>>,
    /// Fan-out hub behind `#[subscribe] fn events` — every successful
    /// root creation / checkpoint publishes here. Sliding mailbox: a
    /// slow subscriber loses its *oldest* queued events, correct for
    /// these state-shaped payloads (same convention as
    /// `task::TaskBackend`).
    events: architect::PubSub<FilesEvent>,
}

// Manual impl: `PubSub` and the repo cache carry no `Debug`.
impl std::fmt::Debug for FilesBackend {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("FilesBackend")
            .field("data_dir", &self.data_dir)
            .finish_non_exhaustive()
    }
}

/// A shared-confinement refusal, in this crate's vocabulary. A rejected
/// or escaping path is a bad request; an I/O fault underneath is one.
fn confinement(err: task_files_util::PathError) -> Error {
    match err {
        task_files_util::PathError::Io(e) => Error::BadRequest(e.to_string()),
        other => Error::BadRequest(other.to_string()),
    }
}

fn to_files_error(err: Error) -> FilesError {
    match err {
        Error::NotFound(m) => FilesError::NotFound(m),
        Error::AlreadyExists(m) => FilesError::AlreadyExists(m),
        Error::BadRequest(m) => FilesError::BadRequest(m),
        Error::Io(e) => FilesError::Io(e.to_string()),
        Error::Json(e) => FilesError::Io(format!("registry json: {e}")),
        Error::VersionStore(e) => FilesError::Io(format!("version store: {e}")),
        Error::Repo(m) => FilesError::Io(format!("jj repo: {m}")),
        Error::JjBackend(e) => FilesError::Io(format!("jj backend: {e}")),
    }
}

/// Run a sync `*_inner` call on the blocking thread pool — the seam
/// every `FilesService` method below uses (see the module doc). The
/// closure captures a cheap `Clone` of `self` (every field is an
/// `Arc`/`PathBuf`), never `self` by reference, so it satisfies
/// `spawn_blocking`'s `'static` bound.
///
/// The seam itself lives in `task-files-util`, shared with
/// `files-storage` — it was a verbatim copy in both (PR #284 review).
async fn blocking<T, F>(f: F) -> Result<T, FilesError>
where
    F: FnOnce() -> Result<T, Error> + Send + 'static,
    T: Send + 'static,
{
    task_files_util::blocking(f, |e| Error::Io(std::io::Error::other(e)))
        .await
        .map_err(to_files_error)
}

impl FilesBackend {
    /// `data_dir` holds the root registry and (for roots the server
    /// hosts) their version stores; `vault_root` is the org vault the
    /// Named / Project Version entities are written into and scanned
    /// from. They are deliberately two directories: root *content* is
    /// never vault-replicated, curation always is.
    pub fn new(
        data_dir: impl Into<PathBuf>,
        vault_root: impl Into<PathBuf>,
    ) -> Result<Self, FilesError> {
        Self::with_cadence(
            data_dir,
            vault_root,
            CadenceConfig::default(),
            Arc::new(SystemClock),
        )
    }

    /// A backend whose cadence engine (issue #260) runs on `config` and
    /// `clock`. Tests use this with a [`crate::cadence::TestClock`]:
    /// quiescence and debounce are simulated, never slept (spec #255's
    /// Testing Decisions).
    pub fn with_cadence(
        data_dir: impl Into<PathBuf>,
        vault_root: impl Into<PathBuf>,
        config: CadenceConfig,
        clock: Arc<dyn Clock>,
    ) -> Result<Self, FilesError> {
        let data_dir = data_dir.into();
        let registry = Registry::open(&data_dir).map_err(to_files_error)?;
        let confine_root = data_dir
            .canonicalize()
            .map_err(|e| to_files_error(Error::Io(e)))?;
        Ok(Self {
            data_dir,
            confine_root,
            registry: Arc::new(registry),
            versions: VaultVersions::new(vault_root),
            repos: Arc::new(Mutex::new(HashMap::new())),
            root_locks: Arc::new(Mutex::new(HashMap::new())),
            cadence: Arc::new(CadenceEngine::new(config, clock)),
            ignores: Arc::new(Mutex::new(HashMap::new())),
            watchers: Arc::new(Mutex::new(HashMap::new())),
            watch_new_roots: Arc::new(std::sync::atomic::AtomicBool::new(false)),
            driver: Arc::new(Mutex::new(None)),
            hook: Arc::new(Mutex::new(None)),
            events: architect::PubSub::sliding(256),
        })
    }

    /// The cadence engine driving this backend's sessions.
    #[must_use]
    pub fn cadence(&self) -> &Arc<CadenceEngine> {
        &self.cadence
    }

    /// Install the certification test seam (see
    /// [`crate::certify::MidHashHook`]): a callback run between the
    /// pre-read `stat` of each file and the read itself, so a test can
    /// make a file change mid-hash deterministically. Production never
    /// calls this.
    #[doc(hidden)]
    pub fn set_mid_hash_hook(&self, hook: Option<MidHashHook>) {
        *self.hook.lock().expect("hook lock poisoned") = hook;
    }

    #[must_use]
    pub fn data_dir(&self) -> &Path {
        &self.data_dir
    }

    /// This org's files area — the boundary every caller-supplied path
    /// is confined to (see the module doc's "Filesystem confinement").
    ///
    /// Exposed as the boundary itself rather than as a
    /// `is_confined(path) -> bool` helper so that other surfaces over
    /// the same roots — the WebDAV bridge (`files-webdav`, issue #274)
    /// checks a root's live tree before handing a filesystem view of it
    /// to a network client — can call [`task_files_util::confine`]
    /// directly and *keep the error kind*. A `bool` collapses
    /// `PathError::Escapes` (a genuine confinement breach, alert-worthy)
    /// into `PathError::Io` (a temporarily-unmounted volume, EIO), and
    /// reporting the second as the first is both a false alarm and the
    /// wrong status code (PR #287 review).
    #[must_use]
    pub fn confine_root(&self) -> &Path {
        &self.confine_root
    }

    /// The org vault the curated version entities live in.
    #[must_use]
    pub fn vault_root(&self) -> &Path {
        self.versions.vault_root()
    }

    /// Run `f` against one root's live version-store backend — the
    /// spec's "secondary harness" seam (Testing Decisions), for the
    /// store-level properties that are invisible at the RPC surface:
    /// chunk presence after a GC pass, dedup ratios, streaming.
    ///
    /// It hands out the *cached* repo's backend rather than opening a
    /// second one, which matters: two `FsStore`s over one on-disk
    /// chunk store in a single process is the shape that used to hang
    /// (see `tests/rpc_surface.rs`). `f` is synchronous; drive any
    /// async work in it with `pollster::block_on`, as this crate does
    /// everywhere it touches jj-lib.
    ///
    /// Media roots only — a software root's objects are git's, and
    /// there is no [`VersionStoreBackend`] under it. Use
    /// [`FilesBackend::with_repo`] for anything flavor-agnostic.
    pub fn with_version_store<R>(
        &self,
        root_id: Uuid,
        f: impl FnOnce(&VersionStoreBackend) -> R,
    ) -> Result<R, FilesError> {
        self.with_repo(root_id, |repo| {
            let backend = repo
                .store()
                .backend_impl::<VersionStoreBackend>()
                .ok_or_else(|| {
                    to_files_error(Error::Repo(
                        "root's repo is not a VersionStoreBackend".into(),
                    ))
                })?;
            Ok(f(backend))
        })?
    }

    /// [`FilesBackend::with_version_store`] one level lower: the cached
    /// jj repo handle itself, for the store-level properties that need
    /// a transaction rather than just the backend.
    ///
    /// Deliberately the **cached** handle, never a reloaded one — a
    /// test that writes a commit through it and doesn't touch the cache
    /// reproduces exactly what a second process does to this one: the
    /// op log on disk moves forward while this backend's handle stays
    /// where it was. That is the condition [`FilesBackend::reload_repo`]
    /// exists for, and it cannot be built with two `FilesBackend`s in
    /// one process — two `FsStore`s over one store hangs (see
    /// `tests/rpc_surface.rs`).
    pub fn with_repo<R>(
        &self,
        root_id: Uuid,
        f: impl FnOnce(&Arc<ReadonlyRepo>) -> R,
    ) -> Result<R, FilesError> {
        let root = self.get_root_info(root_id).map_err(to_files_error)?;
        let (repo, _head) = self.ensure_repo(&root).map_err(to_files_error)?;
        Ok(f(&repo))
    }

    fn publish(&self, event: FilesEvent) {
        self.events.publish(event);
    }

    /// Best-effort flush of every cached root's chunk store
    /// (`ChunkStore::shutdown`) — call before dropping a `FilesBackend`
    /// whose process is about to reopen the same roots (a real server
    /// exit, or a test simulating a restart). Not required for the
    /// correctness of any RPC method — jj-lib's own commit path is
    /// already durable — but iroh-blobs' `FsStore` may hold buffered
    /// writes / file-backed resources open until this (or the process)
    /// actually exits; see `ChunkStore::shutdown`'s own doc.
    /// Also stops the cadence (issue #260): the driver task is aborted
    /// and every watcher dropped, so a backend that has been shut down
    /// is inert rather than still ticking against a store the next
    /// backend is about to open (PR #283 review).
    pub async fn shutdown(&self) {
        if let Some(driver) = self.driver.lock().expect("driver lock poisoned").take() {
            driver.abort();
        }
        self.watchers.lock().expect("watcher map poisoned").clear();
        let repos: Vec<Arc<ReadonlyRepo>> = self
            .repos
            .lock()
            .expect("repo cache lock poisoned")
            .values()
            .map(|rt| rt.repo.clone())
            .collect();
        for repo in repos {
            if let Some(backend) = repo.store().backend_impl::<VersionStoreBackend>() {
                let _ = backend.chunks().shutdown().await;
            }
        }
    }

    /// The registry's own record — no Vault lookups. Every inner
    /// caller (`browse` / `chain` / `checkpoint_now` / curation) wants
    /// exactly this; only `list_roots`/`get_root` project the lineage
    /// badge on top (see [`FilesBackend::with_project_version`]), so
    /// the hot paths never pay for a vault scan they don't read (PR
    /// #288 review).
    fn get_root_info(&self, id: Uuid) -> Result<FileRootInfo, Error> {
        self.registry
            .get(id)
            .ok_or_else(|| Error::NotFound(id.to_string()))
    }

    /// Project each root's CURRENT lineage — its highest-numbered
    /// [`ProjectVersion`] entity (issue #261) — onto the roots
    /// `list_roots`/`get_root` return. ONE vault scan for the whole
    /// list, not one per root, and a vault that can't be read degrades
    /// to un-badged roots rather than failing the listing: the badge is
    /// decoration on a registry-owned answer.
    fn with_project_version(&self, mut roots: Vec<FileRootInfo>) -> Vec<FileRootInfo> {
        let mut current: HashMap<Uuid, ProjectVersion> = HashMap::new();
        match self.versions.all_project_versions() {
            Ok(all) => {
                for pv in all {
                    current
                        .entry(pv.root_id)
                        .and_modify(|held| {
                            if pv.number > held.number {
                                *held = pv.clone();
                            }
                        })
                        .or_insert(pv);
                }
            }
            Err(e) => tracing::warn!(
                ?e,
                "reading Project Versions failed; listing roots without lineage badges"
            ),
        }
        for root in &mut roots {
            root.project_version = current.get(&root.id).cloned();
        }
        roots
    }

    /// Canonicalize `requested` and confirm it resolves inside
    /// [`FilesBackend::confine_root`] — the org-scoping check for
    /// `create_root` (a not-yet-existing marker means `requested`
    /// itself must exist as a directory, checked by the caller first)
    /// and `drive_browse`.
    ///
    /// The check itself is `task_files_util::confine`, shared with
    /// `files-storage`'s grant-prefix enforcement: it was written three
    /// times across the platform, so a hardening fix to one copy left
    /// the others escapable (PR #284 review).
    fn confine(&self, requested: &Path) -> Result<PathBuf, Error> {
        task_files_util::confine(requested, &self.confine_root).map_err(confinement)
    }

    /// The commit a checkpoint on this root builds on. Media roots read
    /// jj's own view head; software roots follow git's checked-out
    /// branch instead, so checkpoints continue the branch a developer
    /// (or CI) is actually on rather than an arbitrary head of a repo
    /// that may carry many (see [`git_root::head_commit`]).
    fn head_of(repo: &Arc<ReadonlyRepo>, flavor: RootFlavor) -> Result<CommitId, Error> {
        match flavor {
            RootFlavor::Software => git_root::head_commit(repo),
            RootFlavor::Media => Ok(repo
                .view()
                .heads()
                .iter()
                .next()
                .cloned()
                .unwrap_or_else(|| repo.store().root_commit_id().clone())),
        }
    }

    /// Backend + current head for `root`, opening (and caching) the
    /// repo on first touch.
    ///
    /// A software root's cached repo is *refreshed* on every call, not
    /// just opened once: its history has a second author (a human or CI
    /// running plain `git` in the same checkout), and serving a cached
    /// view would mean chains that never show their commits and
    /// checkpoints that parent onto a stale head, forking history behind
    /// git's back (PR #282 review). Refreshing is a git-ref read, which
    /// is what the colocated promise costs. Media roots have no second
    /// author — Files owns their store outright — so their cache stands.
    fn ensure_repo(&self, root: &FileRootInfo) -> Result<(Arc<ReadonlyRepo>, CommitId), Error> {
        let cached = {
            let repos = self.repos.lock().expect("repo cache lock poisoned");
            repos
                .get(&root.id)
                .map(|rt| (rt.repo.clone(), rt.head.clone()))
        };
        let repo = match (cached, root.flavor) {
            (Some((repo, head)), RootFlavor::Media) => return Ok((repo, head)),
            (Some((repo, _)), RootFlavor::Software) => git_root::import_from_git(repo)?,
            (None, _) => repo_open::open_or_init_repo(Path::new(&root.path), root.flavor)?,
        };
        let (head, snapshot_head) = Self::heads_of(&repo, root)?;
        self.repos.lock().expect("repo cache lock poisoned").insert(
            root.id,
            RootRuntime {
                repo: repo.clone(),
                head: head.clone(),
                snapshot_head,
            },
        );
        Ok((repo, head))
    }

    /// The root's `(checkpoint head, snapshot-branch tip)`.
    ///
    /// The cadence journal, not the view, is what says which head is the
    /// *checkpoint* head (issue #260): a root mid-session carries a
    /// snapshot branch alongside its checkpoint line, so "the first view
    /// head" is a coin flip between them — and picking the snapshot
    /// would put ephemeral captures straight into every version chain,
    /// which is exactly what branching them was for. The journal is also
    /// the right authority across processes: whoever writes a checkpoint
    /// rewrites it atomically in the same breath, so a second writer's
    /// checkpoint is visible here the moment it lands.
    ///
    /// Media only. Git is a software root's authority — [`head_of`]
    /// already follows its checked-out branch, and that flavor takes no
    /// auto-snapshots at all (see [`FilesBackend::capture_inner`]).
    fn heads_of(
        repo: &Arc<ReadonlyRepo>,
        root: &FileRootInfo,
    ) -> Result<(CommitId, Option<CommitId>), Error> {
        if root.flavor == RootFlavor::Software {
            return Ok((Self::head_of(repo, root.flavor)?, None));
        }
        let journal = Self::journal_of(root)?;
        let snapshot_head = journal
            .snapshot_head
            .as_deref()
            .and_then(CommitId::try_from_hex);
        let Some(recorded) = journal
            .checkpoint_head
            .as_deref()
            .and_then(CommitId::try_from_hex)
        else {
            // No journal (a root that has never captured, or one whose
            // journal was lost): the view is all there is.
            return Ok((Self::head_of(repo, root.flavor)?, snapshot_head));
        };

        // The journal names where *our* checkpoint line was; the view
        // may have moved past it. A writer with no journal of its own —
        // raw `jj`, a test writing straight through the `Backend` trait
        // — leaves a view head that descends from the recorded one, and
        // that head is the honest answer (#286's "a checkpoint written
        // behind the cache" case). Snapshot commits are excluded by id
        // rather than by ancestry: they descend from the recorded head
        // too, and following one would put every ephemeral capture back
        // into the version chain.
        let known_snapshots: std::collections::HashSet<String> = journal
            .snapshots
            .iter()
            .map(|s| s.snapshot_id.clone())
            .chain(journal.snapshot_head.clone())
            .collect();
        let mut head = recorded;
        for candidate in repo.view().heads() {
            if *candidate == head || known_snapshots.contains(&candidate.hex()) {
                continue;
            }
            let descends = pollster::block_on(repo.index().is_ancestor(&head, candidate))
                .map_err(|e| Error::Repo(format!("comparing heads: {e}")))?;
            if descends {
                head = candidate.clone();
            }
        }
        Ok((head, snapshot_head))
    }

    /// The root's cadence journal (issue #260).
    fn journal_of(root: &FileRootInfo) -> Result<Journal, Error> {
        Journal::load(&repo_open::store_dir(Path::new(&root.path)))
    }

    /// The tip of the root's auto-snapshot branch, if its session has
    /// taken one since the last checkpoint.
    fn snapshot_head_of(&self, root_id: Uuid) -> Option<CommitId> {
        self.repos
            .lock()
            .expect("repo cache lock poisoned")
            .get(&root_id)
            .and_then(|rt| rt.snapshot_head.clone())
    }

    /// [`FilesBackend::ensure_repo`], but re-read from the op log
    /// first — the only honest input for anything that walks the DAG.
    ///
    /// The cache is only ever advanced by *this* process's own writes
    /// (`create_root` / `checkpoint_now` call `set_head`), and
    /// `root_locks` is a `Mutex` in this process's memory, not a lock
    /// on disk. A second process writing the same store is a real,
    /// shipped path: `establish_for_url` falls back to the CLI's own
    /// embedded backend whenever the dial fails, so `task files
    /// checkpoint` can write commits the server's cached handle has
    /// never seen. Sweeping from that stale index would treat those
    /// commits as unreachable garbage, and `keep_newer` doesn't save
    /// them — it is a race guard against writes happening *now*, not
    /// against a handle that has been stale for an hour.
    ///
    /// `reload_at_head` goes through the repo's own `RepoLoader`, so
    /// it reuses this root's existing `Store` (and the one `FsStore`
    /// under it) rather than opening a second one — see
    /// `with_version_store`'s doc for why that distinction matters.
    ///
    /// **Software roots need nothing extra here.** Their authority is
    /// git, and [`FilesBackend::ensure_repo`] already re-imports its
    /// refs on every call for exactly the same reason this exists —
    /// that flavor's "second author" is a developer running plain
    /// `git`, ours is a second process on the same store. Re-reading
    /// the op log on top of a fresh import would be a second answer to
    /// a question git has already answered.
    fn reload_repo(&self, root: &FileRootInfo) -> Result<(Arc<ReadonlyRepo>, CommitId), Error> {
        let (cached, head) = self.ensure_repo(root)?;
        if root.flavor == RootFlavor::Software {
            return Ok((cached, head));
        }
        let repo = pollster::block_on(cached.reload_at_head())
            .map_err(|e| Error::Repo(format!("reloading {} at head: {e}", root.id)))?;
        let (head, snapshot_head) = Self::heads_of(&repo, root)?;
        self.set_heads(root.id, repo.clone(), head.clone(), snapshot_head);
        Ok((repo, head))
    }

    /// The read-path counterpart of [`FilesBackend::reload_repo`]:
    /// open the root's store **only if it already exists**, then
    /// re-read it at head. `Ok(None)` when the root has no store yet
    /// (or its volume is not mounted) — a read must never initialize
    /// one, and must never serve a snapshot frozen at this process's
    /// last write (PR #288 review; the same staleness `reload_repo`
    /// exists for on the write/GC side).
    fn reload_existing_repo(
        &self,
        root: &FileRootInfo,
    ) -> Result<Option<(Arc<ReadonlyRepo>, CommitId)>, Error> {
        // Disk first, cache second: a root whose volume went away
        // still has a live handle in this process, and reloading that
        // handle at head fails with a bare "Failed to read operation
        // heads" where the honest answer is "there is no store here
        // right now". Evict it so a remount reopens cleanly.
        if !repo_open::store_dir(Path::new(&root.path)).exists() {
            self.repos
                .lock()
                .expect("repo cache lock poisoned")
                .remove(&root.id);
            return Ok(None);
        }
        let cached = {
            let repos = self.repos.lock().expect("repo cache lock poisoned");
            repos.get(&root.id).map(|rt| rt.repo.clone())
        };
        let repo = match cached {
            Some(repo) => match root.flavor {
                // Git is the authority for a software root, and
                // importing its refs is how the jj view catches up.
                RootFlavor::Software => git_root::import_from_git(repo)?,
                RootFlavor::Media => pollster::block_on(repo.reload_at_head())
                    .map_err(|e| Error::Repo(format!("reloading {} at head: {e}", root.id)))?,
            },
            None => match repo_open::open_existing_repo(Path::new(&root.path), root.flavor)? {
                Some(repo) => repo,
                None => return Ok(None),
            },
        };
        let (head, snapshot_head) = Self::heads_of(&repo, root)?;
        self.set_heads(root.id, repo.clone(), head.clone(), snapshot_head);
        Ok(Some((repo, head)))
    }

    /// Advance the cached checkpoint head, leaving the snapshot branch
    /// where it is — every caller here is a checkpoint-line move, and a
    /// mid-session reload must not forget the session's snapshots.
    fn set_head(&self, root_id: Uuid, repo: Arc<ReadonlyRepo>, head: CommitId) {
        let mut repos = self.repos.lock().expect("repo cache lock poisoned");
        let snapshot_head = repos.get(&root_id).and_then(|rt| rt.snapshot_head.clone());
        repos.insert(
            root_id,
            RootRuntime {
                repo,
                head,
                snapshot_head,
            },
        );
    }

    /// Set both heads at once — what a capture does (issue #260): a
    /// checkpoint moves the line and closes the branch, a snapshot
    /// leaves the line alone and extends the branch.
    fn set_heads(
        &self,
        root_id: Uuid,
        repo: Arc<ReadonlyRepo>,
        head: CommitId,
        snapshot_head: Option<CommitId>,
    ) {
        self.repos.lock().expect("repo cache lock poisoned").insert(
            root_id,
            RootRuntime {
                repo,
                head,
                snapshot_head,
            },
        );
    }

    fn root_lock(&self, root_id: Uuid) -> Arc<Mutex<()>> {
        self.root_locks
            .lock()
            .expect("root lock map poisoned")
            .entry(root_id)
            .or_insert_with(|| Arc::new(Mutex::new(())))
            .clone()
    }

    fn create_root_inner(
        &self,
        path: String,
        name: String,
        flavor: RootFlavor,
    ) -> Result<FileRootInfo, Error> {
        let requested = PathBuf::from(&path);
        let metadata =
            std::fs::metadata(&requested).map_err(|e| Error::BadRequest(format!("{path}: {e}")))?;
        if !metadata.is_dir() {
            return Err(Error::BadRequest(format!("{path}: not a directory")));
        }
        // Org confinement (see module doc) — before anything else, so
        // a rejected path never even reaches the marker/registry
        // checks below.
        let canonical = self.confine(&requested)?;
        let canonical_str = canonical
            .to_str()
            .ok_or_else(|| Error::BadRequest(format!("{path}: not valid UTF-8")))?
            .to_string();

        if canonical.join(MARKER_FILE).exists() {
            return Err(Error::AlreadyExists(canonical_str));
        }
        // Ancestor/descendant containment, not just exact-path — roots
        // never overlap on disk (glossary "File Root"); an outer root
        // whose live tree contains an inner root's `.fts-files` would
        // otherwise ingest that inner root's entire version store as
        // ordinary content on every checkpoint.
        if let Some(existing) = self.registry.conflicting_root(&canonical) {
            return Err(Error::AlreadyExists(format!(
                "{canonical_str} overlaps existing root {} ({})",
                existing.id, existing.path
            )));
        }

        let repo = repo_open::open_or_init_repo(&canonical, flavor)?;
        let head = Self::head_of(&repo, flavor)?;

        let id = Uuid::new_v4();
        let created_at = Utc::now();
        let marker = serde_json::json!({ "id": id, "name": name });
        std::fs::write(
            canonical.join(MARKER_FILE),
            serde_json::to_vec_pretty(&marker)?,
        )?;

        let root = FileRootInfo {
            id,
            name,
            path: canonical_str,
            flavor,
            created_at,
            // A freshly created root is lineage 1 with no restart
            // behind it, so it wears no Project Version badge until
            // one is recorded in its marker (issue #261).
            project_version: None,
        };
        self.registry.insert(root.clone())?;
        self.set_heads(id, repo, head, None);
        // Compile the Ignore set now, at creation, so the very first
        // capture already excludes the flavor's junk (glossary: "seeded
        // by root flavor, edited per root").
        self.ignore_of(&root)?;
        if self
            .watch_new_roots
            .load(std::sync::atomic::Ordering::SeqCst)
        {
            if let Err(err) = self.watch_root(id) {
                tracing::warn!(root_id = %id, ?err, "files: new root not watched");
            }
        }
        self.publish(FilesEvent::RootCreated(root.clone()));
        Ok(root)
    }

    /// `hide_internals` hides the root's own bookkeeping (the marker
    /// file and the version store) — set only when listing a root's top
    /// level through `browse`, never through `drive_browse`, which shows
    /// the raw tree. `hide_git` additionally hides `.git`, which is a
    /// root's own object store on the software flavor but ordinary
    /// content on a media one.
    fn list_dir(
        dir: &Path,
        hide_internals: bool,
        hide_git: bool,
    ) -> Result<Vec<BrowseEntry>, Error> {
        let mut out = Vec::new();
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let name_os = entry.file_name();
            let Some(name) = name_os.to_str() else {
                continue; // non-UTF8 names are out of scope for v1
            };
            if hide_internals && (name == MARKER_FILE || name == STORE_DIR) {
                continue;
            }
            if hide_git && name == GIT_DIR {
                continue;
            }
            let file_type = entry.file_type()?;
            let size = if file_type.is_file() {
                Some(entry.metadata()?.len())
            } else {
                None
            };
            out.push(BrowseEntry {
                name: name.to_string(),
                is_dir: file_type.is_dir(),
                size,
                // Resident by definition (this listing is the live
                // tree); root browsing overlays the version store's
                // stub/divergence state in `browse_inner`, Drive
                // browsing has no root context and leaves both false.
                stub: false,
                divergent: false,
            });
        }
        out.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(out)
    }

    /// Root-scoped browse. The escape guard is canonicalize-then-
    /// prefix-check against the root's own (already-canonical)
    /// `path`, not a component-string scan: `root_path.join(subpath)`
    /// with an ABSOLUTE `subpath` replaces the base entirely (std
    /// `PathBuf::join` semantics), so a `..`-free string like `/etc`
    /// would otherwise sail through. Canonicalizing the resolved
    /// target also follows symlinks to their real location, so a
    /// symlink inside the root pointing outside it is caught by the
    /// same prefix check — resolving the true escape, not just the
    /// textual one.
    fn browse_inner(&self, root_id: Uuid, subpath: String) -> Result<Vec<BrowseEntry>, Error> {
        let root = self.get_root_info(root_id)?;
        let root_path = PathBuf::from(&root.path);
        let requested = if subpath.is_empty() {
            root_path.clone()
        } else {
            root_path.join(&subpath)
        };
        // A subpath that isn't on disk may still be TRACKED — a
        // directory whose whole content is pointer stubs (issue #266).
        // The store answers for it; the escape guard is the repo-path
        // parse (jj rejects `..` and absolute components), so this
        // branch can't reach outside the root either.
        //
        // ONLY `NotFound` falls through to the store: EACCES, ELOOP,
        // EIO or an unmounted volume mean we cannot see the live tree,
        // and answering from the store would report every resident file
        // as a stub (PR #288 review). Those propagate. An absolute
        // subpath is refused here too — `repo_dir` would otherwise trim
        // its leading `/` and answer with a root-relative listing.
        if Path::new(&subpath).is_absolute() {
            return Err(Error::BadRequest(format!(
                "subpath escapes the root: {subpath}"
            )));
        }
        let canonical_target = match requested.canonicalize() {
            Ok(target) => Some(target),
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => None,
            Err(e) => return Err(Error::Io(e)),
        };
        if let Some(target) = &canonical_target {
            // The resident case still goes through the platform's one
            // confinement check (PR #284 review) rather than an inline
            // prefix compare — same guard `files-storage` applies to a
            // Storage grant's prefix, so a hardening fix reaches both.
            task_files_util::confine(target, &root_path).map_err(|e| match e {
                task_files_util::PathError::Escapes { .. } => {
                    Error::BadRequest(format!("subpath escapes the root: {subpath}"))
                }
                other => Error::BadRequest(other.to_string()),
            })?;
            if !std::fs::metadata(target)?.is_dir() {
                return Err(Error::BadRequest(format!("{subpath}: not a directory")));
            }
        }
        // `.git` is hidden at every depth on a software root (a nested
        // one is a submodule's object store — not this root's content),
        // while the marker/store pair only ever exists at the top level.
        let mut entries = match &canonical_target {
            Some(target) => {
                let mut listed = Self::list_dir(
                    target,
                    *target == root_path,
                    root.flavor == RootFlavor::Software,
                )?;
                // On-disk pointer stubs (issue #263) are resident *files*
                // to the raw listing but stubs to the platform: flag them
                // and report the LOGICAL size the stub preserves, not the
                // placeholder's own few bytes. Detection is stat-bounded —
                // only a file small enough to be a stub has its header
                // read, so listing a directory of media opens nothing.
                for entry in &mut listed {
                    if entry.is_dir {
                        continue;
                    }
                    if let Some(len) = entry.size
                        && stub::candidate_len(len)
                        && let Some(s) = stub::read(&target.join(&entry.name))?
                    {
                        entry.stub = true;
                        entry.size = Some(s.size);
                    }
                }
                listed
            }
            None => Vec::new(),
        };
        // Overlay the version store's view: tracked-but-not-resident
        // paths join the listing as pointer stubs, and paths whose
        // state differs between visible heads wear the divergence badge
        // (issue #266's explorer renders both). Reading is
        // OPEN-ONLY — browsing must never initialize a store (PR #288
        // review) — and reloads to head first, because the cached
        // handle is only advanced by this process's own writes and a
        // second writer (the CLI's embedded backend, the cadence
        // engine) would otherwise stay invisible forever.
        let dir = badges::repo_dir(&subpath)?;
        let tracked = match self.reload_existing_repo(&root)? {
            Some((repo, head)) => {
                let heads: BTreeSet<CommitId> = match root.flavor {
                    // A software root's authority is git's refs, whose
                    // head `head_of` already resolved; jj's op-log view
                    // is an import of it, not a second opinion.
                    RootFlavor::Software => BTreeSet::from([head.clone()]),
                    RootFlavor::Media => repo.view().heads().iter().cloned().collect(),
                };
                badges::tracked_dir(repo.store().backend(), &head, &heads, &dir)?
            }
            // No store yet (never checkpointed, or the volume is not
            // mounted): the live tree is the whole truth.
            None => badges::TrackedDir::empty(),
        };
        if canonical_target.is_none() && tracked.is_empty() {
            // Neither on disk nor in any head's tree.
            return Err(Error::NotFound(format!("{root_id}:{subpath}")));
        }
        badges::annotate(&tracked, &mut entries);
        Ok(entries)
    }

    fn drive_browse_inner(&self, path: String) -> Result<Vec<BrowseEntry>, Error> {
        let confined = self.confine(Path::new(&path))?;
        let metadata = std::fs::metadata(&confined)?;
        if !metadata.is_dir() {
            return Err(Error::BadRequest(format!("{path}: not a directory")));
        }
        Self::list_dir(&confined, false, false)
    }

    fn chain_inner(&self, root_id: Uuid, path: String) -> Result<Vec<ChainEntry>, Error> {
        let root = self.get_root_info(root_id)?;
        let (repo, head) = self.ensure_repo(&root)?;
        // Both flavors derive chains through the same DAG walk, against
        // jj's `Backend` trait rather than either concrete backend —
        // that is what "the chain/history RPC works identically on a
        // software root" means in code (issue #273).
        let backend = repo.store().backend();
        let repo_path = RepoPathBuf::from_internal_string(&path)
            .map_err(|e| Error::BadRequest(format!("{path:?}: {e}")))?;
        let entries = pollster::block_on(task_files_version_store::chain::version_chain(
            backend, &head, &repo_path,
        ))?;
        // Curated metadata (issue #261): the Vault, not the store, is
        // where names live — so every chain read resolves them fresh
        // from the vault pages rather than caching a projection. That
        // costs one vault scan per call, the same live-scan bargain
        // every other vault-backed slice makes (`WorkstreamBackend`);
        // if it ever measures slow, the fix is a shared vault snapshot
        // on the backend, never a second authority on names.
        //
        // Names are decoration on a store-owned answer, so a vault
        // that can't be read degrades this call to an uncurated chain
        // rather than failing it — the opposite of `protected_commits`,
        // where an unreadable page must stop the sweep.
        let mut names_by_commit: HashMap<String, Vec<String>> = HashMap::new();
        match self.versions.named_versions(Some(root_id)) {
            Ok(named) => {
                for named in named {
                    names_by_commit
                        .entry(named.commit_id)
                        .or_default()
                        .push(named.name);
                }
            }
            Err(e) => tracing::warn!(
                %root_id,
                ?e,
                "reading Named Versions failed; serving the chain uncurated"
            ),
        }
        // Save points are the automatic counterpart of names: also
        // metadata the commit graph does not hold (glossary — "display
        // metadata, not a version"), joined on here from the root's
        // cadence journal, which records them against the checkpoint
        // that closed the session they were marked in (issue #260).
        let journal = Self::journal_of(&root).unwrap_or_default();
        Ok(entries
            .into_iter()
            .map(|e| {
                let commit_id = e.commit_id.hex();
                let mut names = names_by_commit.get(&commit_id).cloned().unwrap_or_default();
                names.sort();
                ChainEntry {
                    save_points: journal.save_points_for(&commit_id),
                    commit_id,
                    path: e.path.as_internal_file_string().to_string(),
                    file_id: e.file_id.hex(),
                    renamed_from: e
                        .renamed_from
                        .map(|p| p.as_internal_file_string().to_string()),
                    names,
                }
            })
            .collect())
    }

    /// The `(commit, change)` pair `commit_ref` names in `root`'s
    /// store — the validation every curation write does before writing
    /// a Vault entity, so a reference can never name a commit that
    /// isn't there.
    ///
    /// `commit_ref` may be a full hex id or an unambiguous hex prefix,
    /// because a prefix is what every human-facing surface prints
    /// (`task files chain` shows twelve characters, and jj itself is
    /// prefix-addressed throughout). An ambiguous prefix is a bad
    /// request, never a coin flip.
    ///
    /// Goes through jj's `Backend` trait rather than
    /// [`VersionStoreBackend`], so curation works the same on both
    /// flavors: a Named Version of a commit in a software root's
    /// colocated git repo is an ordinary Vault entity like any other
    /// (issue #273 generalized the chain and the checkpoint writer the
    /// same way — naming is no different).
    fn resolve_commit(
        &self,
        root: &FileRootInfo,
        commit_ref: &str,
    ) -> Result<(CommitId, ChangeId), Error> {
        let (repo, _head) = self.ensure_repo(root)?;
        let backend = repo.store().backend();

        // A full id is just an even-length hex string as far as
        // `CommitId::try_from_hex` is concerned — it happily decodes a
        // twelve-character prefix into a six-byte id that no object
        // will ever match. So the exact lookup has to be *tried*, not
        // assumed, with prefix resolution as the fallback.
        if let Some(id) = CommitId::try_from_hex(commit_ref) {
            if let Ok(commit) = pollster::block_on(backend.read_commit(&id)) {
                return Ok((id, commit.change_id));
            }
        }
        let prefix = HexPrefix::try_from_hex(commit_ref)
            .ok_or_else(|| Error::BadRequest(format!("{commit_ref:?}: not a hex commit id")))?;
        let commit_id = match repo.index().resolve_commit_id_prefix(&prefix) {
            Ok(PrefixResolution::SingleMatch(id)) => id,
            Ok(PrefixResolution::AmbiguousMatch) => {
                return Err(Error::BadRequest(format!(
                    "{commit_ref:?}: ambiguous commit prefix in root {}",
                    root.id
                )));
            }
            Ok(PrefixResolution::NoMatch) => {
                return Err(Error::NotFound(format!(
                    "commit {commit_ref} in root {}",
                    root.id
                )));
            }
            Err(e) => return Err(Error::Repo(format!("resolving {commit_ref:?}: {e}"))),
        };
        let commit = pollster::block_on(backend.read_commit(&commit_id))
            .map_err(|_| Error::NotFound(format!("commit {commit_ref} in root {}", root.id)))?;
        Ok((commit_id, commit.change_id))
    }

    fn name_version_inner(
        &self,
        root_id: Uuid,
        commit_id: String,
        name: String,
    ) -> Result<NamedVersion, Error> {
        let name = name.trim().to_string();
        if name.is_empty() {
            return Err(Error::BadRequest("a Named Version needs a name".into()));
        }
        let root = self.get_root_info(root_id)?;
        // Same lock a checkpoint and a GC pass take: it serializes the
        // read-then-write over the vault snapshot (so two namings can't
        // both claim one page path) *and* keeps a naming from landing
        // inside a sweep that has already snapshotted its protect set.
        let lock = self.root_lock(root_id);
        let _guard = lock.lock().expect("root lock poisoned");
        let (commit_id, change_id) = self.resolve_commit(&root, &commit_id)?;
        self.versions.create_named_version(
            root_id,
            &root.name,
            name,
            change_id.hex(),
            commit_id.hex(),
        )
    }

    fn unname_version_inner(&self, id: Uuid) -> Result<NamedVersion, Error> {
        let named = self.versions.named_version(id)?;
        let lock = self.root_lock(named.root_id);
        let _guard = lock.lock().expect("root lock poisoned");
        self.versions.delete_named_version(id)?;
        Ok(named)
    }

    /// Resolve a Named Version the way a share link must: prefer the
    /// stable `change_id` (so a rewritten change still lands on its
    /// current commit) and fall back to the recorded `commit_id`.
    /// Either way the answer is one exact change in this root's store,
    /// or [`Error::NotFound`].
    fn resolve_named_version_inner(&self, id: Uuid) -> Result<VersionRef, Error> {
        let named = self.versions.named_version(id)?;
        let root = self.get_root_info(named.root_id)?;
        let (repo, _head) = self.ensure_repo(&root)?;

        let by_change = ChangeId::try_from_hex(&named.change_id).and_then(|change_id| {
            repo.resolve_change_id(&change_id)
                .ok()
                .flatten()
                .and_then(|targets| {
                    targets
                        .visible_with_offsets()
                        .next()
                        .map(|(_, id)| id.clone())
                })
        });
        let (commit_id, change_id) = match by_change {
            Some(commit_id) if !named.change_id.is_empty() => (commit_id, named.change_id.clone()),
            // Either the change isn't in the current index (a Named
            // Version pointing at a commit no view head descends from
            // is a normal, supported shape — that's exactly what the GC
            // protect set exists for), or the page recorded no change
            // id to begin with. Both fall back to the exact commit the
            // entity recorded, validated against the store — one
            // lookup, which yields both halves of the answer.
            _ => {
                let (commit_id, change_id) = self.resolve_commit(&root, &named.commit_id)?;
                (commit_id, change_id.hex())
            }
        };
        Ok(VersionRef {
            root_id: named.root_id,
            change_id,
            commit_id: commit_id.hex(),
        })
    }

    fn start_project_version_inner(
        &self,
        root_id: Uuid,
        label: Option<String>,
    ) -> Result<ProjectVersion, Error> {
        let root = self.get_root_info(root_id)?;
        // See `name_version_inner` for why curation writes take the
        // root lock.
        let lock = self.root_lock(root_id);
        let _guard = lock.lock().expect("root lock poisoned");
        let (_repo, head) = self.ensure_repo(&root)?;
        let (commit_id, change_id) = self.resolve_commit(&root, &head.hex())?;
        let label = label
            .map(|l| l.trim().to_string())
            .filter(|l| !l.is_empty());
        self.versions.create_project_version(
            root_id,
            &root.name,
            label,
            change_id.hex(),
            commit_id.hex(),
        )
    }

    /// Every commit in `root_id`'s store the Vault currently
    /// references — the protect set ADR 0001 calls "Vault-referenced",
    /// resolved live from the vault pages on every pass so a name
    /// deleted (or replicated in) since the last one is honored.
    ///
    /// Three failure modes matter here and they don't all pull the same
    /// way, so each gets its own answer:
    ///
    /// - A page in **this root's own folder** that this process cannot
    ///   read, or whose `commitId` isn't hex at all, is a reference we
    ///   might be about to forfeit. It fails this root's pass
    ///   (`protect_refs` does the strict half) — GC is destructive and
    ///   unnamed content is cheap to keep one more day. Other roots
    ///   sweep normally; a page that is not identifiably this root's is
    ///   never allowed to wedge it.
    /// - A page naming a commit the store **doesn't have** protects
    ///   nothing: that content is already gone, and treating it as
    ///   fatal would wedge GC for the root forever (one stale page from
    ///   a replication reorder, and the store never gets swept again).
    ///   Logged and skipped.
    /// - A page with an **empty** `commitId` — which
    ///   `ProjectVersions::from_page` tolerates, so it exists — names
    ///   nothing at all. Same reasoning: logged and skipped, never
    ///   fatal. (`create_project_version` refuses to write one, so this
    ///   only ever arrives by hand or by replication.)
    ///
    /// Note what goes into `out`: the id `resolve_commit` **resolved**,
    /// never the one parsed off the page. A page may legitimately carry
    /// a twelve-character prefix — that is what every human-facing
    /// surface prints — and `CommitId::try_from_hex` would decode it
    /// into a six-byte id that the mark phase then chokes on. It also
    /// makes the dedup work across a page storing a prefix and another
    /// storing the full id of the same commit.
    fn protected_commits(&self, root: &FileRootInfo) -> Result<Vec<CommitId>, Error> {
        let (repo, _head) = self.ensure_repo(root)?;
        let full_hex_len = repo.store().root_commit_id().as_bytes().len() * 2;
        let mut out: Vec<CommitId> = Vec::new();
        for reference in self.versions.protect_refs(root.id, &root.name)? {
            if reference.commit_id.trim().is_empty() {
                tracing::warn!(
                    page = %reference.page,
                    "a Files version page carries no commit id; nothing to protect"
                );
                continue;
            }
            match self.resolve_commit(root, &reference.commit_id) {
                Ok((resolved, _change_id)) => {
                    if !out.contains(&resolved) {
                        out.push(resolved);
                    }
                }
                // "Not here" only means "already gone" for a full id.
                // An *abbreviation* that resolves to nothing means we
                // failed to interpret it — prefix lookup goes through
                // the index, and a Named Version's whole purpose is to
                // point at commits the index no longer reaches — so
                // treating it as stale would forfeit exactly the
                // content this set exists to keep. Fatal instead, with
                // the page named so a human can write the full id.
                Err(Error::NotFound(_)) if reference.commit_id.len() == full_hex_len => {
                    tracing::warn!(
                        page = %reference.page,
                        commit = %reference.commit_id,
                        "a Files version page references a commit this root's store doesn't have; \
                         nothing to protect"
                    );
                }
                // Not hex, or an ambiguous prefix: we cannot tell what
                // this page protects, so we refuse to sweep past it.
                Err(e) => {
                    return Err(Error::BadRequest(format!(
                        "{}: {:?} does not name a commit ({e}) — refusing to compute a GC protect \
                         set that might silently forfeit the version it references",
                        reference.page, reference.commit_id
                    )));
                }
            }
        }
        Ok(out)
    }

    fn gc_root_inner(
        &self,
        root_id: Uuid,
        keep_newer_secs: Option<u64>,
    ) -> Result<GcReport, Error> {
        let root = self.get_root_info(root_id)?;
        // A software root's objects are git's, and git collects its own
        // garbage (`git gc`, and every host runs it server-side).
        // Sweeping a colocated repository from here would mean deciding
        // reachability for a store whose other author is git itself —
        // exactly the thing issue #273's design refuses to do. Say so
        // plainly rather than failing later with a backend-type
        // mismatch, and leave the protect-set doctrine where it
        // belongs: on the store Files actually owns.
        if root.flavor == RootFlavor::Software {
            return Err(Error::BadRequest(format!(
                "root {root_id} is a software root: its objects live in a colocated git \
                 repository, which collects its own garbage (`git gc`). Files' Vault-protected \
                 sweep applies to media roots only."
            )));
        }
        // Hold the root lock for the whole pass. It blocks that root's
        // checkpoints (and curation writes) for the duration, which is
        // the deliberate trade: a sweep that raced a checkpoint could
        // read a head the checkpoint is still building on top of, or
        // miss a name that landed after the protect set was read, and
        // both of those lose data. GC is an occasional maintenance
        // verb; a checkpoint waiting on it is a delay, not a loss.
        let lock = self.root_lock(root_id);
        let _guard = lock.lock().expect("root lock poisoned");

        // Re-read the op log before deciding what is reachable: a
        // second process may have written checkpoints this handle has
        // never seen, and sweeping from a stale index would delete
        // them. See `reload_repo`.
        let (repo, _head) = self.reload_repo(&root)?;
        let protected = self.protected_commits(&root)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| Error::Repo("root's repo is not a VersionStoreBackend".into()))?;

        let keep_newer = std::time::SystemTime::now()
            .checked_sub(std::time::Duration::from_secs(
                keep_newer_secs.unwrap_or(DEFAULT_GC_KEEP_NEWER_SECS),
            ))
            .unwrap_or(std::time::UNIX_EPOCH);

        let stats = pollster::block_on(task_files_version_store::gc::sweep(
            backend,
            repo.readonly_index().as_index(),
            keep_newer,
            &protected,
        ))?;
        Ok(GcReport {
            objects_swept: stats.objects_swept as u64,
            manifests_swept: stats.chunks.manifests_swept as u64,
            protected_commits: protected.len() as u32,
        })
    }

    /// The one write path behind every capture (issue #260): an
    /// explicit `checkpoint_now`, a quiescence checkpoint, and a
    /// mid-session auto-snapshot all come through here. A checkpoint
    /// parents on the checkpoint head; a snapshot parents on the
    /// snapshot branch's tip (or, for a session's first, on the
    /// checkpoint head — which is what starts the branch). See
    /// [`crate::cadence`] on why snapshots branch rather than extend.
    ///
    /// Auto-snapshots are a **media-flavor** concept: a software root's
    /// history is git's, and hanging ephemeral commits off a branch a
    /// developer shares would be a surprise in `git log`. The cadence
    /// engine still checkpoints software roots at quiescence — that is
    /// an ordinary commit on the checked-out branch.
    fn capture_inner(
        &self,
        root_id: Uuid,
        kind: CaptureKind,
        description: String,
        save_points: Vec<SavePoint>,
    ) -> Result<Captured, Error> {
        let root = self.get_root_info(root_id)?;
        // Serialize captures on this root: held across the whole
        // read-diff-commit-publish sequence so two concurrent callers
        // can't both read the same head and each commit on top of it
        // (PR #280 review) — the second one now genuinely observes the
        // first's result as its parent instead of racing it.
        let lock = self.root_lock(root_id);
        let _guard = lock.lock().expect("checkpoint lock poisoned");

        // Same staleness as `gc_root_inner`, with a different symptom:
        // building on a cached head that another writer has already
        // moved past forks the chain instead of extending it.
        // `reload_repo` re-reads whichever authority this flavor has —
        // git's refs for a software root, the op log for a media one.
        let (repo, head) = self.reload_repo(&root)?;
        // Both flavors write through jj's `Backend` trait, not either
        // concrete backend (issue #273).
        let backend = repo.store().backend();

        let kind = match (kind, root.flavor) {
            // A snapshot on a software root would be a stray commit in
            // someone's git history: checkpoint instead (see the doc).
            (CaptureKind::Snapshot, RootFlavor::Software) => CaptureKind::Checkpoint,
            (kind, _) => kind,
        };
        let snapshot_head = self.snapshot_head_of(root_id);
        let parent_id = match kind {
            CaptureKind::Checkpoint => head.clone(),
            CaptureKind::Snapshot => snapshot_head.unwrap_or_else(|| head.clone()),
        };

        let parent_commit = pollster::block_on(backend.read_commit(&parent_id))?;
        let base_tree_id = parent_commit
            .root_tree
            .clone()
            .into_resolved()
            .map_err(|_| {
                Error::Repo("capturing onto a conflicted tree is unsupported (v1)".into())
            })?;
        let base_tree = pollster::block_on(backend.read_tree(RepoPath::root(), &base_tree_id))?;
        let mut base_paths: BTreeSet<RepoPathBuf> = BTreeSet::new();
        pollster::block_on(scan::walk_tree_paths(
            backend,
            &base_tree,
            RepoPath::root(),
            &mut base_paths,
        ))?;

        // The certifying full scan, with the root's Ignore set applied
        // at enumeration: an ignored *untracked* path is never offered
        // to the store, while an ignored path that is already tracked
        // keeps being versioned (see `crate::ignore`).
        let ignores = self.ignore_of(&root)?;
        let disk_files =
            scan::walk_live_tree(Path::new(&root.path), root.flavor, &ignores, &base_paths)?;
        let hook = self.hook.lock().expect("hook lock poisoned").clone();
        let result = crate::checkpoint::write_checkpoint(Capture {
            repo: &repo,
            backend,
            parent_id,
            base_tree_id,
            base_tree: &base_tree,
            disk_files: &disk_files,
            base_paths: &base_paths,
            description: description.clone(),
            attempts: self.cadence.config().certify_attempts,
            hook,
        })?;

        let at = self.cadence.now();
        let commit_hex = result.commit_id.hex();
        let store_dir = repo_open::store_dir(Path::new(&root.path));
        let mut journal = Journal::load(&store_dir)?;

        let captured = match kind {
            CaptureKind::Snapshot => {
                let checkpoint_head = head.hex();
                self.set_heads(root_id, result.repo, head, Some(result.commit_id));
                journal.record_snapshot(
                    SnapshotRecord {
                        snapshot_id: commit_hex.clone(),
                        at,
                        changed_paths: result.changed_paths.clone(),
                        save_points: save_points.clone(),
                    },
                    &checkpoint_head,
                    at,
                );
                Captured::Snapshot(SnapshotInfo {
                    root_id,
                    snapshot_id: commit_hex,
                    at,
                    changed_paths: result.changed_paths,
                    save_points,
                })
            }
            CaptureKind::Checkpoint => {
                // Software roots are colocated git: move the checked-out
                // branch and rewrite the index so the commit we just
                // wrote is what `git log` / `git status` / `git push`
                // see (issue #273).
                let repo = match root.flavor {
                    RootFlavor::Software => {
                        git_root::publish_checkpoint(result.repo, &result.commit_id)?
                    }
                    RootFlavor::Media => result.repo,
                };
                self.set_heads(root_id, repo, result.commit_id.clone(), None);
                journal.record_checkpoint(CheckpointRecord {
                    commit_id: commit_hex.clone(),
                    at,
                    save_points: save_points.clone(),
                    requeued_paths: result.requeued_paths.clone(),
                });
                Captured::Checkpoint(CheckpointInfo {
                    root_id,
                    commit_id: commit_hex,
                    description,
                    at,
                    changed_paths: result.changed_paths,
                    save_points,
                    requeued_paths: result.requeued_paths,
                })
            }
        };
        journal.save(&store_dir)?;

        self.publish(match &captured {
            Captured::Snapshot(info) => FilesEvent::Snapshotted(info.clone()),
            Captured::Checkpoint(info) => FilesEvent::Checkpointed(info.clone()),
        });
        Ok(captured)
    }

    fn checkpoint_now_inner(
        &self,
        root_id: Uuid,
        description: Option<String>,
    ) -> Result<CheckpointInfo, Error> {
        // An explicit checkpoint certifies the same live tree a
        // quiescence checkpoint would, so it ends the session: the save
        // points it collected ride onto this checkpoint, and the root
        // goes quiet until someone writes again.
        //
        // The session comes out of the engine *before* the capture that
        // needs its save points, so a failed capture has to put it back
        // — the out-of-band twin of `tick`'s `cadence.failed`. Without
        // this, a transient I/O error would silently cost the root both
        // its save points and its pending quiescence checkpoint (PR
        // #283 review).
        let ended = self.cadence.end_session(root_id);
        let save_points = ended.save_points();
        let description = description.unwrap_or_else(|| "checkpoint now".to_string());
        let captured =
            match self.capture_inner(root_id, CaptureKind::Checkpoint, description, save_points) {
                Ok(captured) => captured,
                Err(err) => {
                    self.cadence.restore_session(ended);
                    return Err(err);
                }
            };
        match captured {
            Captured::Checkpoint(info) => Ok(info),
            Captured::Snapshot(_) => unreachable!("a checkpoint capture returns a checkpoint"),
        }
    }

    /// The root's whole Ignore set, compiled on first touch and cached.
    fn ignore_of(
        &self,
        root: &FileRootInfo,
    ) -> Result<Arc<jj_lib::gitignore::GitIgnoreFile>, Error> {
        Hints::ignore_of(&self.ignores, root)
    }

    /// The registry + Ignore-set + cadence slice of this backend, as an
    /// [`ActivitySink`] a watcher can hold.
    fn hints(&self) -> Arc<Hints> {
        Arc::new(Hints {
            registry: self.registry.clone(),
            ignores: self.ignores.clone(),
            cadence: self.cadence.clone(),
        })
    }

    fn snapshots_inner(&self, root_id: Uuid) -> Result<Vec<SnapshotInfo>, Error> {
        let root = self.get_root_info(root_id)?;
        Ok(Self::journal_of(&root)?.snapshot_infos(root_id))
    }

    fn hint_activity_inner(&self, root_id: Uuid, paths: Vec<String>) -> Result<u32, Error> {
        self.hints().note(root_id, &paths)
    }

    fn ignore_set_inner(&self, root_id: Uuid) -> Result<Vec<String>, Error> {
        let root = self.get_root_info(root_id)?;
        ignore::stored_patterns(&repo_open::store_dir(Path::new(&root.path)))
    }

    fn set_ignore_set_inner(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, Error> {
        let root = self.get_root_info(root_id)?;
        let stored = ignore::save_patterns(&repo_open::store_dir(Path::new(&root.path)), patterns)?;
        // Drop the compiled cache so the next capture (and the next
        // hint) matches against the edit.
        self.ignores
            .lock()
            .expect("ignore cache lock poisoned")
            .remove(&root_id);
        Ok(stored)
    }

    /// Resolve + confine one root-relative file path for the hydration
    /// ops. Same double guard as `browse_inner`: refuse absolute
    /// subpaths before `join` (std `join` replaces the base), then
    /// canonicalize-and-prefix-check the platform way. The jj repo
    /// path is parsed too, which rejects `.`/`..` components.
    fn resolve_root_file(
        &self,
        root: &FileRootInfo,
        path: &str,
    ) -> Result<(PathBuf, RepoPathBuf), Error> {
        if Path::new(path).is_absolute() {
            return Err(Error::BadRequest(format!("path escapes the root: {path}")));
        }
        let repo_path =
            RepoPathBuf::from_internal_string(&path.replace(std::path::MAIN_SEPARATOR, "/"))
                .map_err(|e| Error::BadRequest(format!("{path:?}: {e}")))?;
        let root_path = PathBuf::from(&root.path);
        let disk_path = root_path.join(repo_path.as_internal_file_string());
        if let Ok(canonical) = disk_path.canonicalize() {
            task_files_util::confine(&canonical, &root_path).map_err(confinement)?;
        }
        Ok((disk_path, repo_path))
    }

    /// The checkpoint head's `TreeValue::File` fields for `repo_path`,
    /// or `None` when the head doesn't track it.
    fn head_file(
        repo: &Arc<ReadonlyRepo>,
        head: &CommitId,
        repo_path: &RepoPath,
    ) -> Result<Option<(jj_lib::backend::FileId, bool)>, Error> {
        let backend = repo.store().backend();
        let value = pollster::block_on(async {
            let commit = backend.read_commit(head).await?;
            let tree_id =
                commit.root_tree.clone().into_resolved().map_err(|_| {
                    jj_lib::backend::BackendError::Other("conflicted root tree".into())
                })?;
            let tree = backend.read_tree(RepoPath::root(), &tree_id).await?;
            task_files_version_store::chain::lookup_dyn(backend, &tree, repo_path).await
        })
        .map_err(|e| Error::Repo(format!("reading head tree: {e}")))?;
        Ok(match value {
            Some(jj_lib::backend::TreeValue::File { id, executable, .. }) => Some((id, executable)),
            _ => None,
        })
    }

    /// One file's `BrowseEntry` as the hydration ops report it.
    fn entry_for(disk_path: &Path, name: &str) -> Result<BrowseEntry, Error> {
        let len = std::fs::metadata(disk_path)?.len();
        let stub = if stub::candidate_len(len) {
            stub::read(disk_path)?
        } else {
            None
        };
        Ok(BrowseEntry {
            name: name.to_string(),
            is_dir: false,
            size: Some(stub.as_ref().map_or(len, |s| s.size)),
            stub: stub.is_some(),
            divergent: false,
        })
    }

    /// Media-only guard shared by the hydration ops — a software root's
    /// working tree belongs to its colocated git (same split as
    /// `gc_root`): a stub there would just be a modified file to git,
    /// and every git tool would happily commit it as content.
    fn require_media(root: &FileRootInfo, what: &str) -> Result<(), Error> {
        if root.flavor != RootFlavor::Media {
            return Err(Error::BadRequest(format!(
                "{what} is media-only: a software root's working tree belongs to its colocated git"
            )));
        }
        Ok(())
    }

    fn dehydrate_inner(&self, root_id: Uuid, path: String) -> Result<BrowseEntry, Error> {
        let root = self.get_root_info(root_id)?;
        Self::require_media(&root, "dehydrate")?;
        let (disk_path, repo_path) = self.resolve_root_file(&root, &path)?;
        let lock = self.root_lock(root_id);
        let _guard = lock.lock().expect("root lock poisoned");

        if !disk_path.exists() {
            return Err(Error::NotFound(format!("{root_id}:{path}")));
        }
        // Idempotent: already a stub — report it, touch nothing.
        let len = std::fs::metadata(&disk_path)?.len();
        if stub::candidate_len(len) && stub::read(&disk_path)?.is_some() {
            return Self::entry_for(&disk_path, &path);
        }

        // Reloaded head, not the cache: dehydration compares against
        // what is genuinely committed, wherever it was written.
        let (repo, head) = self.reload_repo(&root)?;
        let Some((head_id, executable)) = Self::head_file(&repo, &head, &repo_path)? else {
            return Err(Error::BadRequest(format!(
                "{path}: not tracked by the checkpoint head — checkpoint before dehydrating"
            )));
        };

        // The one rule that makes dehydration safe: on-disk content
        // must BE the committed content. Streaming the file through the
        // content store re-derives its id (a dedup no-op when it is
        // already there); any difference means unversioned work, which
        // a placeholder must never overwrite.
        let backend = repo.store().backend();
        let content = crate::content::ContentStore::for_repo(&repo, backend)?;
        let probed = content.probe(&disk_path)?;
        let disk_id = pollster::block_on(content.write(&repo_path, &disk_path, probed))?;
        if disk_id != head_id {
            return Err(Error::BadRequest(format!(
                "{path}: on-disk content differs from the checkpoint head — checkpoint first, then dehydrate"
            )));
        }

        stub::write(&disk_path, &stub::Stub::new(&head_id, len, executable))?;
        self.publish(FilesEvent::HydrationChanged(HydrationChange {
            root_id,
            path: repo_path.as_internal_file_string().to_string(),
            stub: true,
        }));
        Self::entry_for(&disk_path, &path)
    }

    fn hydrate_inner(&self, root_id: Uuid, path: String) -> Result<BrowseEntry, Error> {
        let root = self.get_root_info(root_id)?;
        Self::require_media(&root, "hydrate")?;
        let (disk_path, repo_path) = self.resolve_root_file(&root, &path)?;
        let lock = self.root_lock(root_id);
        let _guard = lock.lock().expect("root lock poisoned");

        if !disk_path.exists() {
            return Err(Error::NotFound(format!("{root_id}:{path}")));
        }
        let len = std::fs::metadata(&disk_path)?.len();
        let on_disk = if stub::candidate_len(len) {
            stub::read(&disk_path)?
        } else {
            None
        };
        // Idempotent: already resident — report it, touch nothing.
        let Some(recorded) = on_disk else {
            return Self::entry_for(&disk_path, &path);
        };

        // The id to restore: the checkpoint head's when it tracks the
        // path (the head may have moved since dehydration — "the live
        // tree shows the newest save" wins over a stale stub), the
        // stub's own recorded id otherwise.
        let (repo, head) = self.reload_repo(&root)?;
        let (target_id, executable) = match Self::head_file(&repo, &head, &repo_path)? {
            Some((id, exec)) => (id, exec),
            None => (recorded.file_id()?, recorded.executable),
        };

        self.restore_content(&repo, &repo_path, &disk_path, &target_id, executable)?;
        self.publish(FilesEvent::HydrationChanged(HydrationChange {
            root_id,
            path: repo_path.as_internal_file_string().to_string(),
            stub: false,
        }));
        Self::entry_for(&disk_path, &path)
    }

    /// Stream `target_id`'s content from the store to a temp file in
    /// the same directory, verify the bytes re-derive to exactly
    /// `target_id` (the acceptance criterion's "verified by FileId" —
    /// a truncated or corrupt restore never replaces the stub), set the
    /// executable bit, and rename into place.
    fn restore_content(
        &self,
        repo: &Arc<ReadonlyRepo>,
        repo_path: &RepoPath,
        disk_path: &Path,
        target_id: &jj_lib::backend::FileId,
        executable: bool,
    ) -> Result<(), Error> {
        use futures_util::io::AsyncReadExt as _;
        use std::io::Write as _;

        let backend = repo.store().backend();
        let dir = disk_path
            .parent()
            .ok_or_else(|| Error::BadRequest(format!("{}: no parent", disk_path.display())))?;
        let mut tmp = tempfile::NamedTempFile::new_in(dir)?;
        pollster::block_on(async {
            let mut reader = backend.read_file(repo_path, target_id).await?;
            let mut buf = vec![0u8; 128 * 1024];
            loop {
                let n = reader.read(&mut buf).await.map_err(|e| {
                    jj_lib::backend::BackendError::Other(
                        format!("reading {} from the store: {e}", target_id.hex()).into(),
                    )
                })?;
                if n == 0 {
                    break;
                }
                tmp.write_all(&buf[..n]).map_err(|e| {
                    jj_lib::backend::BackendError::Other(format!("writing restore: {e}").into())
                })?;
            }
            Ok::<(), jj_lib::backend::BackendError>(())
        })
        .map_err(Error::from)?;
        tmp.as_file().sync_all()?;

        // Verify by identity before the rename: re-derive the restored
        // bytes' id through the same content store and require it to be
        // the id we asked for.
        let content = crate::content::ContentStore::for_repo(repo, backend)?;
        let probed = content.probe(tmp.path())?;
        let restored_id = pollster::block_on(content.write(repo_path, tmp.path(), probed))?;
        if restored_id != *target_id {
            return Err(Error::Repo(format!(
                "{}: restored content re-derives to {} but the stub promised {} — store damage, stub left in place",
                disk_path.display(),
                restored_id.hex(),
                target_id.hex(),
            )));
        }

        #[cfg(unix)]
        {
            use std::os::unix::fs::PermissionsExt as _;
            let mode = if executable { 0o755 } else { 0o644 };
            std::fs::set_permissions(tmp.path(), std::fs::Permissions::from_mode(mode))?;
        }
        #[cfg(not(unix))]
        let _ = executable;
        tmp.persist(disk_path).map_err(|e| Error::Io(e.error))?;
        Ok(())
    }

    fn hydration_policy_inner(&self, root_id: Uuid) -> Result<Vec<String>, Error> {
        let root = self.get_root_info(root_id)?;
        hydration::stored_policy(&repo_open::store_dir(Path::new(&root.path)))
    }

    fn set_hydration_policy_inner(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, Error> {
        let root = self.get_root_info(root_id)?;
        Self::require_media(&root, "hydration policy")?;
        hydration::save_policy(&repo_open::store_dir(Path::new(&root.path)), patterns)
    }

    fn apply_hydration_policy_inner(&self, root_id: Uuid) -> Result<HydrationReport, Error> {
        let root = self.get_root_info(root_id)?;
        Self::require_media(&root, "hydration policy")?;
        let store_dir = repo_open::store_dir(Path::new(&root.path));
        let Some(policy) = hydration::matcher(&store_dir)? else {
            // Empty policy: opt-in means touch nothing.
            return Ok(HydrationReport {
                hydrated: Vec::new(),
                dehydrated: Vec::new(),
                skipped_dirty: Vec::new(),
            });
        };

        // One live-tree walk decides the whole pass; the per-file ops
        // then re-take the root lock each, so a checkpoint landing
        // mid-pass serializes between files rather than deadlocking
        // against a pass-wide lock.
        let ignores = self.ignore_of(&root)?;
        let (_, head) = self.reload_repo(&root)?;
        let tracked = self.tracked_paths(&root, &head)?;
        let files = scan::walk_live_tree(Path::new(&root.path), root.flavor, &ignores, &tracked)?;

        let mut report = HydrationReport {
            hydrated: Vec::new(),
            dehydrated: Vec::new(),
            skipped_dirty: Vec::new(),
        };
        for file in files {
            let rel = file.repo_path.as_internal_file_string().to_string();
            let keep = hydration::keeps_hydrated(&policy, &rel);
            if file.stub.is_some() {
                if keep {
                    self.hydrate_inner(root_id, rel.clone())?;
                    report.hydrated.push(rel);
                }
            } else if !keep && !file.ignored && tracked.contains(&file.repo_path) {
                match self.dehydrate_inner(root_id, rel.clone()) {
                    Ok(_) => report.dehydrated.push(rel),
                    // Dirty content is the expected, reportable case —
                    // everything else is a real fault.
                    Err(Error::BadRequest(m)) if m.contains("differs from the checkpoint head") => {
                        report.skipped_dirty.push(rel);
                    }
                    Err(e) => return Err(e),
                }
            }
        }
        report.hydrated.sort();
        report.dehydrated.sort();
        report.skipped_dirty.sort();
        Ok(report)
    }

    /// The checkpoint head's full tracked-path set (the scan walker's
    /// second input).
    fn tracked_paths(
        &self,
        root: &FileRootInfo,
        head: &CommitId,
    ) -> Result<std::collections::BTreeSet<RepoPathBuf>, Error> {
        let (repo, _) = self.ensure_repo(root)?;
        let backend = repo.store().backend();
        let mut out = std::collections::BTreeSet::new();
        pollster::block_on(async {
            let commit = backend.read_commit(head).await?;
            let tree_id =
                commit.root_tree.clone().into_resolved().map_err(|_| {
                    jj_lib::backend::BackendError::Other("conflicted root tree".into())
                })?;
            let tree = backend.read_tree(RepoPath::root(), &tree_id).await?;
            scan::walk_tree_paths(backend, &tree, RepoPath::root(), &mut out)
                .await
                .map_err(|e| jj_lib::backend::BackendError::Other(e.to_string().into()))
        })
        .map_err(|e| Error::Repo(format!("walking head tree: {e}")))?;
        Ok(out)
    }

    /// Run one cadence pass: perform every capture that has fallen due
    /// as of the engine's clock. This is what the driver task calls on
    /// a timer in production, and what a test calls after advancing its
    /// [`crate::cadence::TestClock`] — the same code path either way.
    pub async fn tick(&self) -> Vec<Captured> {
        let mut performed = Vec::new();
        for due in self.cadence.take_due() {
            match self.perform_due(&due).await {
                Ok(captured) => {
                    self.cadence.completed(&due);
                    performed.push(captured);
                }
                Err(err) => {
                    // Nothing is consumed on failure: the same capture
                    // falls due again next tick, so a transient I/O
                    // error costs a tick, not a session.
                    tracing::warn!(root_id = %due.root_id, kind = ?due.kind, %err, "files cadence capture failed");
                    self.cadence.failed(&due);
                }
            }
        }
        performed
    }

    async fn perform_due(&self, due: &Due) -> Result<Captured, FilesError> {
        let this = self.clone();
        let due = due.clone();
        let description = match due.kind {
            DueKind::Snapshot => "auto-snapshot".to_string(),
            DueKind::Checkpoint => "session checkpoint".to_string(),
        };
        blocking(move || {
            this.capture_inner(due.root_id, due.kind.into(), description, due.save_points)
        })
        .await
    }

    /// Drive the cadence forever on `interval` — one background task
    /// per backend, the production counterpart of a test calling
    /// [`FilesBackend::tick`] by hand. The interval only bounds how
    /// promptly a due capture happens; the cadence itself is the
    /// engine's, so a coarse interval is cheap.
    ///
    /// The handle is kept on the backend (and aborted by
    /// [`FilesBackend::shutdown`], or by a second call to this) rather
    /// than left to the caller: two drivers ticking one on-disk store
    /// would resurrect exactly the dual-capture race PR #280 closed, and
    /// a driver nobody holds is a driver nobody can stop (PR #283
    /// review).
    pub fn spawn_cadence_driver(&self, interval: std::time::Duration) {
        let this = self.clone();
        let handle = tokio::spawn(async move {
            loop {
                tokio::time::sleep(interval).await;
                let _ = this.tick().await;
            }
        });
        if let Some(previous) = self
            .driver
            .lock()
            .expect("driver lock poisoned")
            .replace(handle)
        {
            previous.abort();
        }
    }

    /// Start the server-side watcher for `root_id` — activity hints
    /// into the cadence engine (see [`crate::cadence::watcher`]).
    /// Idempotent: watching an already-watched root is a no-op.
    ///
    /// Blocking: establishing a recursive watch walks the whole tree
    /// (inotify is per-directory, so one watch per directory), which on
    /// a multi-GB media root with thousands of directories is real
    /// filesystem work. Callers on an async runtime must reach it
    /// through [`FilesBackend::enable_watching`] or their own
    /// `spawn_blocking` (PR #283 review). The watchers map is locked
    /// only around the lookup and the insert, never across that walk.
    pub fn watch_root(&self, root_id: Uuid) -> Result<(), FilesError> {
        let root = self.get_root_info(root_id).map_err(to_files_error)?;
        if self
            .watchers
            .lock()
            .expect("watcher map poisoned")
            .contains_key(&root_id)
        {
            return Ok(());
        }
        let watcher = RootWatcher::spawn(root_id, Path::new(&root.path), self.hints())
            .map_err(to_files_error)?;
        // Another caller may have won the race while the walk ran; the
        // first watch installed wins and ours is dropped (which stops
        // it), so a root never ends up with two.
        self.watchers
            .lock()
            .expect("watcher map poisoned")
            .entry(root_id)
            .or_insert(watcher);
        Ok(())
    }

    /// Watch every root this backend already knows about, and every
    /// root created from here on — what a server does at startup so
    /// sessions are detected without anyone having to call
    /// `hint_activity`. A root whose watch can't be established (an
    /// offline removable location, a platform limit) is logged and
    /// skipped: it still checkpoints on an explicit trigger, which is
    /// the whole reason watchers are hints.
    ///
    /// Async because [`FilesBackend::watch_root`] is blocking work: the
    /// whole sweep runs on `spawn_blocking` so establishing watches over
    /// a NAS full of media roots cannot stall an async worker during org
    /// startup (PR #283 review).
    pub async fn enable_watching(&self) {
        let this = self.clone();
        let _ = tokio::task::spawn_blocking(move || {
            this.watch_new_roots
                .store(true, std::sync::atomic::Ordering::SeqCst);
            for root in this.registry.list() {
                if let Err(err) = this.watch_root(root.id) {
                    tracing::warn!(root_id = %root.id, path = %root.path, %err, "files: root not watched");
                }
            }
        })
        .await;
    }

    /// Stop watching `root_id`.
    pub fn unwatch_root(&self, root_id: Uuid) {
        self.watchers
            .lock()
            .expect("watcher map poisoned")
            .remove(&root_id);
    }
}

impl FilesService for FilesBackend {
    async fn create_root(
        &self,
        path: String,
        name: String,
        flavor: RootFlavor,
    ) -> Result<FileRootInfo, FilesError> {
        let this = self.clone();
        blocking(move || this.create_root_inner(path, name, flavor)).await
    }

    async fn list_roots(&self) -> Result<Vec<FileRootInfo>, FilesError> {
        // On the blocking pool like every other method here: the
        // lineage overlay scans the vault, and one root on a sleeping
        // drive must not stall a runtime worker for every org.
        let this = self.clone();
        blocking(move || Ok(this.with_project_version(this.registry.list()))).await
    }

    async fn get_root(&self, id: Uuid) -> Result<FileRootInfo, FilesError> {
        let this = self.clone();
        blocking(move || {
            let root = this.get_root_info(id)?;
            Ok(this
                .with_project_version(vec![root])
                .pop()
                .expect("one root in, one root out"))
        })
        .await
    }

    async fn browse(&self, root_id: Uuid, subpath: String) -> Result<Vec<BrowseEntry>, FilesError> {
        let this = self.clone();
        blocking(move || this.browse_inner(root_id, subpath)).await
    }

    async fn dehydrate(&self, root_id: Uuid, path: String) -> Result<BrowseEntry, FilesError> {
        let this = self.clone();
        blocking(move || this.dehydrate_inner(root_id, path)).await
    }

    async fn hydrate(&self, root_id: Uuid, path: String) -> Result<BrowseEntry, FilesError> {
        let this = self.clone();
        blocking(move || this.hydrate_inner(root_id, path)).await
    }

    async fn hydration_policy(&self, root_id: Uuid) -> Result<Vec<String>, FilesError> {
        let this = self.clone();
        blocking(move || this.hydration_policy_inner(root_id)).await
    }

    async fn set_hydration_policy(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, FilesError> {
        let this = self.clone();
        blocking(move || this.set_hydration_policy_inner(root_id, patterns)).await
    }

    async fn apply_hydration_policy(&self, root_id: Uuid) -> Result<HydrationReport, FilesError> {
        let this = self.clone();
        blocking(move || this.apply_hydration_policy_inner(root_id)).await
    }

    async fn drive_browse(&self, path: String) -> Result<Vec<BrowseEntry>, FilesError> {
        let this = self.clone();
        blocking(move || this.drive_browse_inner(path)).await
    }

    async fn chain(&self, root_id: Uuid, path: String) -> Result<Vec<ChainEntry>, FilesError> {
        let this = self.clone();
        blocking(move || this.chain_inner(root_id, path)).await
    }

    async fn checkpoint_now(
        &self,
        root_id: Uuid,
        description: Option<String>,
    ) -> Result<CheckpointInfo, FilesError> {
        let this = self.clone();
        blocking(move || this.checkpoint_now_inner(root_id, description)).await
    }

    async fn hint_activity(&self, root_id: Uuid, paths: Vec<String>) -> Result<u32, FilesError> {
        // On the blocking pool like its neighbours: the first hint for a
        // root compiles (and may seed) its Ignore set off disk.
        let this = self.clone();
        blocking(move || this.hint_activity_inner(root_id, paths)).await
    }

    async fn snapshots(&self, root_id: Uuid) -> Result<Vec<SnapshotInfo>, FilesError> {
        let this = self.clone();
        blocking(move || this.snapshots_inner(root_id)).await
    }

    async fn ignore_set(&self, root_id: Uuid) -> Result<Vec<String>, FilesError> {
        let this = self.clone();
        blocking(move || this.ignore_set_inner(root_id)).await
    }

    async fn set_ignore_set(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, FilesError> {
        let this = self.clone();
        blocking(move || this.set_ignore_set_inner(root_id, patterns)).await
    }

    async fn name_version(
        &self,
        root_id: Uuid,
        commit_id: String,
        name: String,
    ) -> Result<NamedVersion, FilesError> {
        let this = self.clone();
        let named = blocking(move || this.name_version_inner(root_id, commit_id, name)).await?;
        self.publish(FilesEvent::VersionNamed(named.clone()));
        Ok(named)
    }

    async fn list_named_versions(
        &self,
        root_id: Option<Uuid>,
    ) -> Result<Vec<NamedVersion>, FilesError> {
        let this = self.clone();
        blocking(move || this.versions.named_versions(root_id)).await
    }

    async fn resolve_named_version(&self, id: Uuid) -> Result<VersionRef, FilesError> {
        let this = self.clone();
        blocking(move || this.resolve_named_version_inner(id)).await
    }

    async fn unname_version(&self, id: Uuid) -> Result<(), FilesError> {
        let this = self.clone();
        let removed = blocking(move || this.unname_version_inner(id)).await?;
        self.publish(FilesEvent::VersionUnnamed(removed));
        Ok(())
    }

    async fn start_project_version(
        &self,
        root_id: Uuid,
        label: Option<String>,
    ) -> Result<ProjectVersion, FilesError> {
        let this = self.clone();
        let pv = blocking(move || this.start_project_version_inner(root_id, label)).await?;
        self.publish(FilesEvent::ProjectVersionStarted(pv.clone()));
        Ok(pv)
    }

    async fn list_project_versions(
        &self,
        root_id: Uuid,
    ) -> Result<Vec<ProjectVersion>, FilesError> {
        let this = self.clone();
        blocking(move || this.versions.project_versions(root_id)).await
    }

    async fn gc_root(
        &self,
        root_id: Uuid,
        keep_newer_secs: Option<u64>,
    ) -> Result<GcReport, FilesError> {
        let this = self.clone();
        blocking(move || this.gc_root_inner(root_id, keep_newer_secs)).await
    }
}

/// The `#[subscribe]` backend contract: hand the emitted stream host
/// the hub it attaches subscriber sinks to. Publishing happens in the
/// `*_inner` methods above, on every successful mutation.
impl files_proto::service::FilesServiceStreamSource for FilesBackend {
    fn events_hub(&self) -> &architect::PubSub<FilesEvent> {
        &self.events
    }
}
