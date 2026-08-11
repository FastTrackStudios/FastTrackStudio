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

use crate::consts::{MARKER_FILE, STORE_DIR};
use crate::error::Error;
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
    pub fn with_version_store<R>(
        &self,
        root_id: Uuid,
        f: impl FnOnce(&VersionStoreBackend) -> R,
    ) -> Result<R, FilesError> {
        let root = self.get_root_info(root_id).map_err(to_files_error)?;
        let (repo, _head) = self.ensure_repo(&root).map_err(to_files_error)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| {
                to_files_error(Error::Repo(
                    "root's repo is not a VersionStoreBackend".into(),
                ))
            })?;
        Ok(f(backend))
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
    fn resolve_commit(
        &self,
        root: &FileRootInfo,
        commit_ref: &str,
    ) -> Result<(CommitId, ChangeId), Error> {
        let (repo, _head) = self.ensure_repo(root)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| Error::Repo("root's repo is not a VersionStoreBackend".into()))?;

        // A full id is just an even-length hex string as far as
        // `CommitId::try_from_hex` is concerned — it happily decodes a
        // twelve-character prefix into a six-byte id that no object
        // will ever match. So the exact lookup has to be *tried*, not
        // assumed, with prefix resolution as the fallback.
        if let Some(id) = CommitId::try_from_hex(commit_ref) {
            if let Ok(commit) = pollster::block_on(backend.commit(&id)) {
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
        let commit = pollster::block_on(backend.commit(&commit_id))
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
        let commit_id = match by_change {
            Some(commit_id) => commit_id,
            // The change isn't in the current index (a Named Version
            // pointing at a commit no view head descends from is a
            // normal, supported shape — that's exactly what the GC
            // protect set exists for), so fall back to the exact
            // commit the entity recorded, validated against the store.
            None => self.resolve_commit(&root, &named.commit_id)?.0,
        };
        let change_id = if named.change_id.is_empty() {
            self.resolve_commit(&root, &commit_id.hex())?.1.hex()
        } else {
            named.change_id.clone()
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
    /// Two failure modes both matter here and pull in opposite
    /// directions, so each gets its own answer:
    ///
    /// - A page this process **cannot read or cannot parse as a commit
    ///   id** is a reference we might be about to forfeit. It fails the
    ///   whole pass (`protect_refs` is the strict walk; a malformed hex
    ///   is rejected here) — GC is destructive and unnamed content is
    ///   cheap to keep one more day.
    /// - A page naming a commit the store **doesn't have** protects
    ///   nothing: that content is already gone, and treating it as
    ///   fatal would wedge GC for the root forever (one stale page from
    ///   a replication reorder, and the store never gets swept again).
    ///   It is logged and skipped.
    fn protected_commits(&self, root: &FileRootInfo) -> Result<Vec<CommitId>, Error> {
        let mut out: Vec<CommitId> = Vec::new();
        for reference in self.versions.protect_refs(root.id)? {
            let id = CommitId::try_from_hex(&reference.commit_id)
                .filter(|id| !id.as_bytes().is_empty())
                .ok_or_else(|| {
                    Error::BadRequest(format!(
                        "{}: {:?} is not a commit id — refusing to compute a GC protect set that \
                         might silently forfeit the version it references",
                        reference.page, reference.commit_id
                    ))
                })?;
            match self.resolve_commit(root, &id.hex()) {
                Ok(_) => {
                    if !out.contains(&id) {
                        out.push(id);
                    }
                }
                Err(e) => tracing::warn!(
                    page = %reference.page,
                    commit = %reference.commit_id,
                    ?e,
                    "a Files version page references a commit this root's store doesn't have; \
                     nothing to protect"
                ),
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
        // Hold the root lock for the whole pass. It blocks that root's
        // checkpoints (and curation writes) for the duration, which is
        // the deliberate trade: a sweep that raced a checkpoint could
        // read a head the checkpoint is still building on top of, or
        // miss a name that landed after the protect set was read, and
        // both of those lose data. GC is an occasional maintenance
        // verb; a checkpoint waiting on it is a delay, not a loss.
        let lock = self.root_lock(root_id);
        let _guard = lock.lock().expect("root lock poisoned");

        let protected = self.protected_commits(&root)?;
        let (repo, _head) = self.ensure_repo(&root)?;
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
