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
    GcReport, NamedVersion, ProjectVersion, RootFlavor, VersionRef,
};
use jj_lib::backend::{ChangeId, CommitId};
use jj_lib::object_id::{HexPrefix, ObjectId as _, PrefixResolution};
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use jj_lib::repo_path::{RepoPath, RepoPathBuf};
use task_files_version_store::VersionStoreBackend;
use uuid::Uuid;

use crate::badges;
use crate::consts::{GIT_DIR, MARKER_FILE, STORE_DIR};
use crate::error::Error;
use crate::git_root;
use crate::registry::Registry;
use crate::repo_open;
use crate::scan;
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
        Error::JjBackend(e) => FilesError::Io(format!("jj backend: {e}")),
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
    /// `data_dir` holds the root registry and (for roots the server
    /// hosts) their version stores; `vault_root` is the org vault the
    /// Named / Project Version entities are written into and scanned
    /// from. They are deliberately two directories: root *content* is
    /// never vault-replicated, curation always is.
    pub fn new(
        data_dir: impl Into<PathBuf>,
        vault_root: impl Into<PathBuf>,
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
            events: architect::PubSub::sliding(256),
        })
    }

    #[must_use]
    pub fn data_dir(&self) -> &Path {
        &self.data_dir
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
        let head = Self::head_of(&repo, root.flavor)?;
        self.repos.lock().expect("repo cache lock poisoned").insert(
            root.id,
            RootRuntime {
                repo: repo.clone(),
                head: head.clone(),
            },
        );
        Ok((repo, head))
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
        let head = Self::head_of(&repo, root.flavor)?;
        self.set_head(root.id, repo.clone(), head.clone());
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
        let head = Self::head_of(&repo, root.flavor)?;
        self.set_head(root.id, repo.clone(), head.clone());
        Ok(Some((repo, head)))
    }

    fn set_head(&self, root_id: Uuid, repo: Arc<ReadonlyRepo>, head: CommitId) {
        self.repos
            .lock()
            .expect("repo cache lock poisoned")
            .insert(root_id, RootRuntime { repo, head });
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
        self.set_head(id, repo, head);
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
            if target != &root_path && !target.starts_with(&root_path) {
                return Err(Error::BadRequest(format!(
                    "subpath escapes the root: {subpath}"
                )));
            }
            if !std::fs::metadata(target)?.is_dir() {
                return Err(Error::BadRequest(format!("{subpath}: not a directory")));
            }
        }
        // `.git` is hidden at every depth on a software root (a nested
        // one is a submodule's object store — not this root's content),
        // while the marker/store pair only ever exists at the top level.
        let mut entries = match &canonical_target {
            Some(target) => Self::list_dir(
                target,
                *target == root_path,
                root.flavor == RootFlavor::Software,
            )?,
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
        Ok(entries
            .into_iter()
            .map(|e| {
                let commit_id = e.commit_id.hex();
                let mut names = names_by_commit.get(&commit_id).cloned().unwrap_or_default();
                names.sort();
                ChainEntry {
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

        let head_commit = pollster::block_on(backend.read_commit(&head))?;
        let head_tree_id = head_commit.root_tree.clone().into_resolved().map_err(|_| {
            Error::Repo("checkpoint onto a conflicted tree is unsupported (v1)".into())
        })?;
        let head_tree = pollster::block_on(backend.read_tree(RepoPath::root(), &head_tree_id))?;
        let mut head_paths: BTreeSet<RepoPathBuf> = BTreeSet::new();
        pollster::block_on(scan::walk_tree_paths(
            backend,
            &head_tree,
            RepoPath::root(),
            &mut head_paths,
        ))?;

        let disk_files = scan::walk_live_tree(Path::new(&root.path), root.flavor, &head_paths)?;
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
        // Software roots are colocated git: move the checked-out branch
        // and rewrite the index so the commit we just wrote is what
        // `git log` / `git status` / `git push` see (issue #273).
        let repo = match root.flavor {
            RootFlavor::Software => git_root::publish_checkpoint(result.repo, &result.commit_id)?,
            RootFlavor::Media => result.repo,
        };
        self.set_head(root_id, repo, result.commit_id.clone());

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
