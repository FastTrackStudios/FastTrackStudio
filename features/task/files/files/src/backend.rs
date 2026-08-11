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
    RootFlavor, SavePoint, SnapshotInfo,
};
use jj_lib::backend::{CommitId, Tree, TreeId};
use jj_lib::object_id::ObjectId as _;
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::repo_path::{RepoPath, RepoPathBuf};
use task_files_version_store::VersionStoreBackend;
use uuid::Uuid;

use crate::cadence::journal::{CheckpointRecord, SnapshotRecord};
use crate::cadence::{
    ActivitySink, CadenceConfig, CadenceEngine, Clock, Due, DueKind, Journal, RootWatcher,
    SystemClock,
};
use crate::certify::MidHashHook;
use crate::checkpoint::{Capture, CaptureResult};
use crate::consts::{MARKER_FILE, STORE_DIR};
use crate::error::Error;
use crate::ignore::IgnoreSet;
use crate::registry::Registry;
use crate::repo_open;
use crate::scan;

/// One root's live jj state: the repo handle (reassigned after every
/// capture), its current checkpoint head, and the tip of the
/// auto-snapshot branch hanging off that head (if the session has taken
/// one). Both heads are tracked explicitly rather than re-derived from
/// `repo.view().heads()`: a root mid-session genuinely has two heads,
/// and only the journal knows which is which (see
/// [`crate::cadence::journal`]).
struct RootRuntime {
    repo: Arc<ReadonlyRepo>,
    head: CommitId,
    snapshot_head: Option<CommitId>,
}

/// Which kind of capture a write is — the one difference that decides
/// what it parents on and how it is recorded.
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
    ignores: Arc<Mutex<HashMap<Uuid, IgnoreSet>>>,
    cadence: Arc<CadenceEngine>,
}

impl Hints {
    /// The root's Ignore set, loaded from its store dir (and seeded from
    /// its flavor on first touch) then cached in `ignores`.
    fn ignore_of(
        ignores: &Mutex<HashMap<Uuid, IgnoreSet>>,
        root: &FileRootInfo,
    ) -> Result<IgnoreSet, Error> {
        if let Some(set) = ignores
            .lock()
            .expect("ignore cache lock poisoned")
            .get(&root.id)
        {
            return Ok(set.clone());
        }
        let set = IgnoreSet::load_or_seed(&Path::new(&root.path).join(STORE_DIR), root.flavor)?;
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
        let ignore = Self::ignore_of(&self.ignores, &root)?;
        Ok(self
            .cadence
            .note_activity(root_id, paths, &ignore, root.flavor))
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

/// What one performed capture produced — the wire payload plus which
/// kind it was, so [`FilesBackend::tick`] can report a cadence pass.
#[derive(Debug, Clone)]
pub enum Captured {
    Snapshot(SnapshotInfo),
    Checkpoint(CheckpointInfo),
}

#[derive(Clone, architect::HasDispatcher)]
pub struct FilesBackend {
    data_dir: PathBuf,
    /// Canonicalized once at construction — the boundary `create_root`
    /// / `drive_browse` path arguments must resolve inside (see the
    /// module doc's "Filesystem confinement" section).
    confine_root: PathBuf,
    registry: Arc<Registry>,
    repos: Arc<Mutex<HashMap<Uuid, RootRuntime>>>,
    /// One lock per root, serializing captures on that root so two
    /// concurrent checkpoints can't both read the same head and
    /// silently orphan one commit (PR #280 review) — created lazily,
    /// never removed (roots are not deleted in v1).
    checkpoint_locks: Arc<Mutex<HashMap<Uuid, Arc<Mutex<()>>>>>,
    /// The cadence state machine (issue #260): when each root's session
    /// snapshots, and when it ends in a checkpoint.
    cadence: Arc<CadenceEngine>,
    /// Per-root Ignore sets, loaded from each root's store dir on first
    /// touch.
    ignores: Arc<Mutex<HashMap<Uuid, IgnoreSet>>>,
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

fn to_files_error(err: Error) -> FilesError {
    match err {
        Error::NotFound(m) => FilesError::NotFound(m),
        Error::AlreadyExists(m) => FilesError::AlreadyExists(m),
        Error::BadRequest(m) => FilesError::BadRequest(m),
        Error::Io(e) => FilesError::Io(e.to_string()),
        Error::Json(e) => FilesError::Io(format!("registry json: {e}")),
        Error::VersionStore(e) => FilesError::Io(format!("version store: {e}")),
        Error::Repo(m) => FilesError::Io(format!("jj repo: {m}")),
    }
}

/// Run a sync `*_inner` call on the blocking thread pool — the seam
/// every `FilesService` method below uses (see the module doc). The
/// closure captures a cheap `Clone` of `self` (every field is an
/// `Arc`/`PathBuf`), never `self` by reference, so it satisfies
/// `spawn_blocking`'s `'static` bound.
async fn blocking<T, F>(f: F) -> Result<T, FilesError>
where
    F: FnOnce() -> Result<T, Error> + Send + 'static,
    T: Send + 'static,
{
    tokio::task::spawn_blocking(f)
        .await
        .map_err(|e| FilesError::Io(format!("blocking task panicked: {e}")))?
        .map_err(to_files_error)
}

impl FilesBackend {
    /// A backend on the real clock with the default cadence (10-minute
    /// auto-snapshots, 30-minute quiescence).
    pub fn new(data_dir: impl Into<PathBuf>) -> Result<Self, FilesError> {
        Self::with_cadence(data_dir, CadenceConfig::default(), Arc::new(SystemClock))
    }

    /// A backend whose cadence runs on `config` and `clock`. Tests use
    /// this with a [`crate::cadence::TestClock`]: quiescence and
    /// debounce are simulated, never slept (spec #255's Testing
    /// Decisions).
    pub fn with_cadence(
        data_dir: impl Into<PathBuf>,
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
            repos: Arc::new(Mutex::new(HashMap::new())),
            checkpoint_locks: Arc::new(Mutex::new(HashMap::new())),
            cadence: Arc::new(CadenceEngine::new(config, clock)),
            ignores: Arc::new(Mutex::new(HashMap::new())),
            watchers: Arc::new(Mutex::new(HashMap::new())),
            watch_new_roots: Arc::new(std::sync::atomic::AtomicBool::new(false)),
            driver: Arc::new(Mutex::new(None)),
            hook: Arc::new(Mutex::new(None)),
            events: architect::PubSub::sliding(256),
        })
    }

    #[must_use]
    pub fn data_dir(&self) -> &Path {
        &self.data_dir
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
    /// Also stops the cadence: the driver task is aborted and every
    /// watcher dropped, so a backend that has been shut down is inert
    /// rather than still ticking against a store the next backend is
    /// about to open (PR #283 review).
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

    fn get_root_info(&self, id: Uuid) -> Result<FileRootInfo, Error> {
        self.registry
            .get(id)
            .ok_or_else(|| Error::NotFound(id.to_string()))
    }

    fn store_dir(root_path: &Path) -> PathBuf {
        root_path.join(STORE_DIR)
    }

    /// Canonicalize `requested` and confirm it resolves inside
    /// [`FilesBackend::confine_root`] — the org-scoping check for
    /// `create_root` (a not-yet-existing marker means `requested`
    /// itself must exist as a directory, checked by the caller first)
    /// and `drive_browse`.
    fn confine(&self, requested: &Path) -> Result<PathBuf, Error> {
        let canonical = requested
            .canonicalize()
            .map_err(|e| Error::BadRequest(format!("{}: {e}", requested.display())))?;
        if canonical != self.confine_root && !canonical.starts_with(&self.confine_root) {
            return Err(Error::BadRequest(format!(
                "{}: outside this org's files area ({})",
                requested.display(),
                self.confine_root.display()
            )));
        }
        Ok(canonical)
    }

    fn head_of(repo: &Arc<ReadonlyRepo>) -> CommitId {
        repo.view()
            .heads()
            .iter()
            .next()
            .cloned()
            .unwrap_or_else(|| repo.store().root_commit_id().clone())
    }

    /// Parse a hex commit id recorded in the journal, ignoring one
    /// that no longer decodes (a hand-edited journal must not wedge a
    /// root — the commit graph is the authority, this is only labels).
    fn commit_id_from_hex(hex: &str) -> Option<CommitId> {
        CommitId::try_from_hex(hex)
    }

    /// Repo + checkpoint head + snapshot-branch tip for `root`, opening
    /// (and caching) the repo on first touch. The heads come from the
    /// root's cadence journal when it has one: a root mid-session has a
    /// snapshot branch alongside its checkpoint line, so
    /// [`FilesBackend::head_of`]'s "first view head" would be a coin
    /// flip between them after a restart.
    fn ensure_repo(
        &self,
        root: &FileRootInfo,
    ) -> Result<(Arc<ReadonlyRepo>, CommitId, Option<CommitId>), Error> {
        {
            let repos = self.repos.lock().expect("repo cache lock poisoned");
            if let Some(rt) = repos.get(&root.id) {
                return Ok((rt.repo.clone(), rt.head.clone(), rt.snapshot_head.clone()));
            }
        }
        let store_dir = Self::store_dir(Path::new(&root.path));
        let repo = repo_open::open_or_init_repo(&store_dir)?;
        let journal = Journal::load(&store_dir)?;
        let head = journal
            .checkpoint_head
            .as_deref()
            .and_then(Self::commit_id_from_hex)
            .unwrap_or_else(|| Self::head_of(&repo));
        let snapshot_head = journal
            .snapshot_head
            .as_deref()
            .and_then(Self::commit_id_from_hex);
        self.repos.lock().expect("repo cache lock poisoned").insert(
            root.id,
            RootRuntime {
                repo: repo.clone(),
                head: head.clone(),
                snapshot_head: snapshot_head.clone(),
            },
        );
        Ok((repo, head, snapshot_head))
    }

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

    /// The registry + Ignore-set + cadence slice of this backend, as an
    /// [`ActivitySink`] a watcher can hold.
    fn hints(&self) -> Arc<Hints> {
        Arc::new(Hints {
            registry: self.registry.clone(),
            ignores: self.ignores.clone(),
            cadence: self.cadence.clone(),
        })
    }

    /// The root's Ignore set, loaded from its store dir (and seeded
    /// from its flavor on first touch) then cached.
    fn ignore_of(&self, root: &FileRootInfo) -> Result<IgnoreSet, Error> {
        Hints::ignore_of(&self.ignores, root)
    }

    fn journal_of(root: &FileRootInfo) -> Result<Journal, Error> {
        Journal::load(&Self::store_dir(Path::new(&root.path)))
    }

    fn checkpoint_lock(&self, root_id: Uuid) -> Arc<Mutex<()>> {
        self.checkpoint_locks
            .lock()
            .expect("checkpoint lock map poisoned")
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
        if flavor != RootFlavor::Media {
            return Err(Error::BadRequest(
                "only RootFlavor::Media is implemented in v1 (issue #259); Software roots are \
                 colocated git, a distinct build (ADR 0001)"
                    .into(),
            ));
        }
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

        let store_dir = Self::store_dir(&canonical);
        let repo = repo_open::open_or_init_repo(&store_dir)?;
        let head = Self::head_of(&repo);

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
        };
        self.registry.insert(root.clone())?;
        self.set_heads(id, repo, head, None);
        // Seed the Ignore set from the flavor now, at creation, so the
        // very first capture already excludes the junk (glossary:
        // "seeded by root flavor, edited per root").
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

    fn list_dir(dir: &Path, hide_internals: bool) -> Result<Vec<BrowseEntry>, Error> {
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
        let canonical_target = requested
            .canonicalize()
            .map_err(|_| Error::NotFound(format!("{root_id}:{subpath}")))?;
        if canonical_target != root_path && !canonical_target.starts_with(&root_path) {
            return Err(Error::BadRequest(format!(
                "subpath escapes the root: {subpath}"
            )));
        }
        let metadata = std::fs::metadata(&canonical_target)?;
        if !metadata.is_dir() {
            return Err(Error::BadRequest(format!("{subpath}: not a directory")));
        }
        Self::list_dir(&canonical_target, canonical_target == root_path)
    }

    fn drive_browse_inner(&self, path: String) -> Result<Vec<BrowseEntry>, Error> {
        let confined = self.confine(Path::new(&path))?;
        let metadata = std::fs::metadata(&confined)?;
        if !metadata.is_dir() {
            return Err(Error::BadRequest(format!("{path}: not a directory")));
        }
        Self::list_dir(&confined, false)
    }

    fn chain_inner(&self, root_id: Uuid, path: String) -> Result<Vec<ChainEntry>, Error> {
        let root = self.get_root_info(root_id)?;
        let journal = Self::journal_of(&root)?;
        let (repo, head, _) = self.ensure_repo(&root)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| Error::Repo("root's repo is not a VersionStoreBackend".into()))?;
        let repo_path = RepoPathBuf::from_internal_string(&path)
            .map_err(|e| Error::BadRequest(format!("{path:?}: {e}")))?;
        let entries = pollster::block_on(task_files_version_store::chain::version_chain(
            backend, &head, &repo_path,
        ))?;
        Ok(entries
            .into_iter()
            .map(|e| {
                let commit_id = e.commit_id.hex();
                ChainEntry {
                    // Save points are display metadata rather than
                    // anything the commit graph holds (glossary), so
                    // they are joined on here from the root's cadence
                    // journal — the checkpoint that closed the session
                    // they were marked in.
                    save_points: journal.save_points_for(&commit_id),
                    commit_id,
                    path: e.path.as_internal_file_string().to_string(),
                    file_id: e.file_id.hex(),
                    renamed_from: e
                        .renamed_from
                        .map(|p| p.as_internal_file_string().to_string()),
                }
            })
            .collect())
    }

    /// The one write path behind every capture: an explicit
    /// `checkpoint_now`, a quiescence checkpoint, and a mid-session
    /// auto-snapshot all come through here. A checkpoint parents on the
    /// checkpoint head; a snapshot parents on the snapshot branch's tip
    /// (or, for a session's first, on the checkpoint head — which is
    /// what starts the branch). See [`crate::cadence`] on why snapshots
    /// branch rather than extend the line.
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
        let lock = self.checkpoint_lock(root_id);
        let _guard = lock.lock().expect("checkpoint lock poisoned");

        let (repo, head, snapshot_head) = self.ensure_repo(&root)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| Error::Repo("root's repo is not a VersionStoreBackend".into()))?;

        let parent_id = match kind {
            CaptureKind::Checkpoint => head.clone(),
            CaptureKind::Snapshot => snapshot_head.clone().unwrap_or_else(|| head.clone()),
        };
        let (base_tree_id, base_tree, base_paths) = Self::base_state(backend, &parent_id)?;

        // The full stat-scan that certifies the capture, with the
        // root's Ignore set applied at enumeration: an ignored path
        // never enters the store, because it is never even offered to
        // it (glossary "Ignore set").
        let ignore = self.ignore_of(&root)?;
        let disk_files = scan::walk_live_tree(Path::new(&root.path), &ignore)?;
        let hook = self.hook.lock().expect("hook lock poisoned").clone();
        let result: CaptureResult = crate::checkpoint::write_capture(Capture {
            repo: &repo,
            backend,
            parent_id,
            base_tree_id,
            base_tree: &base_tree,
            disk_files: &disk_files,
            base_paths: &base_paths,
            ignore: &ignore,
            description: description.clone(),
            attempts: self.cadence.config().certify_attempts,
            hook,
        })?;

        let at = self.cadence.now();
        let commit_hex = result.commit_id.hex();
        let store_dir = Self::store_dir(Path::new(&root.path));
        let mut journal = Journal::load(&store_dir)?;

        let captured = match kind {
            CaptureKind::Snapshot => {
                self.set_heads(root_id, result.repo, head, Some(result.commit_id));
                journal.record_snapshot(
                    SnapshotRecord {
                        snapshot_id: commit_hex.clone(),
                        at,
                        changed_paths: result.changed_paths.clone(),
                        save_points: save_points.clone(),
                    },
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
                self.set_heads(root_id, result.repo, result.commit_id, None);
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

    /// The tree a capture builds on: its id, its content, and every
    /// path it tracks (the set a capture removes from when a file has
    /// left the live tree).
    fn base_state(
        backend: &VersionStoreBackend,
        parent_id: &CommitId,
    ) -> Result<(TreeId, Tree, BTreeSet<RepoPathBuf>), Error> {
        let commit = pollster::block_on(backend.commit(parent_id))?;
        let tree_id = commit.root_tree.clone().into_resolved().map_err(|_| {
            Error::Repo("capturing onto a conflicted tree is unsupported (v1)".into())
        })?;
        let tree = pollster::block_on(backend.tree(&tree_id))?;
        let mut paths: BTreeSet<RepoPathBuf> = BTreeSet::new();
        pollster::block_on(scan::walk_tree_paths(
            backend,
            &tree,
            RepoPath::root(),
            &mut paths,
        ))?;
        Ok((tree_id, tree, paths))
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

    fn snapshots_inner(&self, root_id: Uuid) -> Result<Vec<SnapshotInfo>, Error> {
        let root = self.get_root_info(root_id)?;
        Ok(Self::journal_of(&root)?.snapshot_infos(root_id))
    }

    fn hint_activity_inner(&self, root_id: Uuid, paths: Vec<String>) -> Result<u32, Error> {
        self.hints().note(root_id, &paths)
    }

    fn set_ignore_set_inner(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, Error> {
        let root = self.get_root_info(root_id)?;
        let set = IgnoreSet::compile(patterns)?;
        set.save(&Self::store_dir(Path::new(&root.path)))?;
        let stored = set.patterns().to_vec();
        self.ignores
            .lock()
            .expect("ignore cache lock poisoned")
            .insert(root_id, set);
        Ok(stored)
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
        Ok(self.registry.list())
    }

    async fn get_root(&self, id: Uuid) -> Result<FileRootInfo, FilesError> {
        self.get_root_info(id).map_err(to_files_error)
    }

    async fn browse(&self, root_id: Uuid, subpath: String) -> Result<Vec<BrowseEntry>, FilesError> {
        let this = self.clone();
        blocking(move || this.browse_inner(root_id, subpath)).await
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
        // root loads (and may seed + write) its Ignore set.
        let this = self.clone();
        blocking(move || this.hint_activity_inner(root_id, paths)).await
    }

    async fn snapshots(&self, root_id: Uuid) -> Result<Vec<SnapshotInfo>, FilesError> {
        let this = self.clone();
        blocking(move || this.snapshots_inner(root_id)).await
    }

    async fn ignore_set(&self, root_id: Uuid) -> Result<Vec<String>, FilesError> {
        let this = self.clone();
        blocking(move || {
            let root = this.get_root_info(root_id)?;
            Ok(this.ignore_of(&root)?.patterns().to_vec())
        })
        .await
    }

    async fn set_ignore_set(
        &self,
        root_id: Uuid,
        patterns: Vec<String>,
    ) -> Result<Vec<String>, FilesError> {
        let this = self.clone();
        blocking(move || this.set_ignore_set_inner(root_id, patterns)).await
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
