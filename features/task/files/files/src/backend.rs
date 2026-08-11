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
    RootFlavor,
};
use jj_lib::backend::CommitId;
use jj_lib::object_id::ObjectId as _;
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::repo_path::{RepoPath, RepoPathBuf};
use task_files_version_store::VersionStoreBackend;
use uuid::Uuid;

use crate::consts::{MARKER_FILE, STORE_DIR};
use crate::error::Error;
use crate::registry::Registry;
use crate::repo_open;
use crate::scan;

/// One root's live jj state: the repo handle (reassigned after every
/// `checkpoint_now`) and its current checkpoint head. `head` is tracked
/// explicitly rather than re-derived from `repo.view().heads()` on
/// every call — see `checkpoint::checkpoint`'s own doc example, which
/// establishes this as the pattern for reading back the commit a
/// checkpoint just produced.
struct RootRuntime {
    repo: Arc<ReadonlyRepo>,
    head: CommitId,
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
    /// One lock per root, serializing `checkpoint_now` calls on that
    /// root so two concurrent checkpoints can't both read the same
    /// head and silently orphan one commit (PR #280 review) — created
    /// lazily, never removed (roots are not deleted in v1).
    checkpoint_locks: Arc<Mutex<HashMap<Uuid, Arc<Mutex<()>>>>>,
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
    pub fn new(data_dir: impl Into<PathBuf>) -> Result<Self, FilesError> {
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
            events: architect::PubSub::sliding(256),
        })
    }

    #[must_use]
    pub fn data_dir(&self) -> &Path {
        &self.data_dir
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
    pub async fn shutdown(&self) {
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
    ///
    /// The check itself is `task_files_util::confine`, shared with
    /// `files-storage`'s grant-prefix enforcement: it was written three
    /// times across the platform, so a hardening fix to one copy left
    /// the others escapable (PR #284 review).
    fn confine(&self, requested: &Path) -> Result<PathBuf, Error> {
        task_files_util::confine(requested, &self.confine_root).map_err(confinement)
    }

    fn head_of(repo: &Arc<ReadonlyRepo>) -> CommitId {
        repo.view()
            .heads()
            .iter()
            .next()
            .cloned()
            .unwrap_or_else(|| repo.store().root_commit_id().clone())
    }

    /// Backend + current head for `root`, opening (and caching) the
    /// repo on first touch.
    fn ensure_repo(&self, root: &FileRootInfo) -> Result<(Arc<ReadonlyRepo>, CommitId), Error> {
        {
            let repos = self.repos.lock().expect("repo cache lock poisoned");
            if let Some(rt) = repos.get(&root.id) {
                return Ok((rt.repo.clone(), rt.head.clone()));
            }
        }
        let store_dir = Self::store_dir(Path::new(&root.path));
        let repo = repo_open::open_or_init_repo(&store_dir)?;
        let head = Self::head_of(&repo);
        self.repos.lock().expect("repo cache lock poisoned").insert(
            root.id,
            RootRuntime {
                repo: repo.clone(),
                head: head.clone(),
            },
        );
        Ok((repo, head))
    }

    fn set_head(&self, root_id: Uuid, repo: Arc<ReadonlyRepo>, head: CommitId) {
        self.repos
            .lock()
            .expect("repo cache lock poisoned")
            .insert(root_id, RootRuntime { repo, head });
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
        self.set_head(id, repo, head);
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
        if !requested.exists() {
            return Err(Error::NotFound(format!("{root_id}:{subpath}")));
        }
        let canonical_target =
            task_files_util::confine(&requested, &root_path).map_err(|e| match e {
                task_files_util::PathError::Escapes { .. } => {
                    Error::BadRequest(format!("subpath escapes the root: {subpath}"))
                }
                other => Error::BadRequest(other.to_string()),
            })?;
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
        let (repo, head) = self.ensure_repo(&root)?;
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
            .map(|e| ChainEntry {
                commit_id: e.commit_id.hex(),
                path: e.path.as_internal_file_string().to_string(),
                file_id: e.file_id.hex(),
                renamed_from: e
                    .renamed_from
                    .map(|p| p.as_internal_file_string().to_string()),
            })
            .collect())
    }

    fn checkpoint_now_inner(
        &self,
        root_id: Uuid,
        description: Option<String>,
    ) -> Result<CheckpointInfo, Error> {
        let root = self.get_root_info(root_id)?;
        // Serialize checkpoints on this root: held across the whole
        // read-diff-commit-publish sequence so two concurrent callers
        // can't both read the same head and each commit on top of it
        // (PR #280 review) — the second one now genuinely observes the
        // first's result as its parent instead of racing it.
        let lock = self.checkpoint_lock(root_id);
        let _guard = lock.lock().expect("checkpoint lock poisoned");

        let (repo, head) = self.ensure_repo(&root)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| Error::Repo("root's repo is not a VersionStoreBackend".into()))?;

        let head_commit = pollster::block_on(backend.commit(&head))?;
        let head_tree_id = head_commit.root_tree.clone().into_resolved().map_err(|_| {
            Error::Repo("checkpoint onto a conflicted tree is unsupported (v1)".into())
        })?;
        let head_tree = pollster::block_on(backend.tree(&head_tree_id))?;
        let mut head_paths: BTreeSet<RepoPathBuf> = BTreeSet::new();
        pollster::block_on(scan::walk_tree_paths(
            backend,
            &head_tree,
            RepoPath::root(),
            &mut head_paths,
        ))?;

        let disk_files = scan::walk_live_tree(Path::new(&root.path))?;
        let description = description.unwrap_or_else(|| "checkpoint now".to_string());
        let result = crate::checkpoint::write_checkpoint(
            &repo,
            backend,
            head,
            head_tree_id,
            &head_tree,
            &disk_files,
            &head_paths,
            description.clone(),
        )?;
        self.set_head(root_id, result.repo, result.commit_id.clone());

        let info = CheckpointInfo {
            root_id,
            commit_id: result.commit_id.hex(),
            description,
            at: Utc::now(),
            changed_paths: result.changed_paths,
        };
        self.publish(FilesEvent::Checkpointed(info.clone()));
        Ok(info)
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
}

/// The `#[subscribe]` backend contract: hand the emitted stream host
/// the hub it attaches subscriber sinks to. Publishing happens in the
/// `*_inner` methods above, on every successful mutation.
impl files_proto::service::FilesServiceStreamSource for FilesBackend {
    fn events_hub(&self) -> &architect::PubSub<FilesEvent> {
        &self.events
    }
}
