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
//! inside an `async fn` without poisoning the RPC method's future. Each
//! `FilesService` method below is `async fn` only because the trait
//! requires it; its body never actually awaits anything.

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
use task_files_version_store::checkpoint::Change;
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
    registry: Arc<Registry>,
    repos: Arc<Mutex<HashMap<Uuid, RootRuntime>>>,
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

impl FilesBackend {
    pub fn new(data_dir: impl Into<PathBuf>) -> Result<Self, FilesError> {
        let data_dir = data_dir.into();
        let registry = Registry::open(&data_dir).map_err(to_files_error)?;
        Ok(Self {
            data_dir,
            registry: Arc::new(registry),
            repos: Arc::new(Mutex::new(HashMap::new())),
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

    fn get_root_info(&self, id: Uuid) -> Result<FileRootInfo, Error> {
        self.registry
            .get(id)
            .ok_or_else(|| Error::NotFound(id.to_string()))
    }

    fn store_dir(root_path: &Path) -> PathBuf {
        root_path.join(STORE_DIR)
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
        let canonical = requested
            .canonicalize()
            .map_err(|e| Error::BadRequest(format!("{path}: {e}")))?;
        let canonical_str = canonical
            .to_str()
            .ok_or_else(|| Error::BadRequest(format!("{path}: not valid UTF-8")))?
            .to_string();

        if canonical.join(MARKER_FILE).exists() {
            return Err(Error::AlreadyExists(canonical_str));
        }
        if self.registry.path_taken(&canonical) {
            return Err(Error::AlreadyExists(canonical_str));
        }

        let store_dir = Self::store_dir(&canonical);
        let repo = repo_open::open_or_init_repo(&store_dir)?;
        let head = Self::head_of(&repo);

        let id = Uuid::new_v4();
        let created_at = Utc::now();
        let marker = serde_json::json!({ "id": id, "name": name });
        std::fs::write(canonical.join(MARKER_FILE), serde_json::to_vec_pretty(&marker)?)?;

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

    fn browse_inner(&self, root_id: Uuid, subpath: String) -> Result<Vec<BrowseEntry>, Error> {
        let root = self.get_root_info(root_id)?;
        if subpath.split('/').any(|c| c == "..") {
            return Err(Error::BadRequest(format!("subpath escapes the root: {subpath}")));
        }
        let root_path = PathBuf::from(&root.path);
        let target = if subpath.is_empty() {
            root_path.clone()
        } else {
            root_path.join(&subpath)
        };
        let metadata = std::fs::metadata(&target).map_err(|_| Error::NotFound(format!("{root_id}:{subpath}")))?;
        if !metadata.is_dir() {
            return Err(Error::BadRequest(format!("{subpath}: not a directory")));
        }
        Self::list_dir(&target, target == root_path)
    }

    fn drive_browse_inner(&self, path: String) -> Result<Vec<BrowseEntry>, Error> {
        let target = PathBuf::from(&path);
        let metadata = std::fs::metadata(&target).map_err(|e| Error::BadRequest(format!("{path}: {e}")))?;
        if !metadata.is_dir() {
            return Err(Error::BadRequest(format!("{path}: not a directory")));
        }
        Self::list_dir(&target, false)
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
            backend,
            &head,
            &repo_path,
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
        let (repo, head) = self.ensure_repo(&root)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| Error::Repo("root's repo is not a VersionStoreBackend".into()))?;

        let head_commit = pollster::block_on(backend.commit(&head))?;
        let head_tree_id = head_commit
            .root_tree
            .clone()
            .into_resolved()
            .map_err(|_| Error::Repo("checkpoint onto a conflicted tree is unsupported (v1)".into()))?;
        let head_tree = pollster::block_on(backend.tree(&head_tree_id))?;
        let mut head_paths: BTreeSet<RepoPathBuf> = BTreeSet::new();
        pollster::block_on(scan::walk_tree_paths(
            backend,
            &head_tree,
            RepoPath::root(),
            &mut head_paths,
        ))?;

        let disk_files = scan::walk_live_tree(Path::new(&root.path))?;
        let changes = scan::diff_to_changes(&disk_files, &head_paths)?;
        let mut changed_paths: Vec<String> = changes
            .iter()
            .map(|c| match c {
                Change::Write { path, .. } | Change::Remove { path } => {
                    path.as_internal_file_string().to_string()
                }
                Change::Rename { to, .. } => to.as_internal_file_string().to_string(),
            })
            .collect();
        changed_paths.sort();

        let description = description.unwrap_or_else(|| "checkpoint now".to_string());
        let new_repo = pollster::block_on(task_files_version_store::checkpoint::checkpoint(
            &repo,
            head,
            changes,
            description.clone(),
        ))?;
        let new_head = new_repo
            .view()
            .heads()
            .iter()
            .next()
            .cloned()
            .ok_or_else(|| Error::Repo("checkpoint produced no head".into()))?;
        self.set_head(root_id, new_repo, new_head.clone());

        let info = CheckpointInfo {
            root_id,
            commit_id: new_head.hex(),
            description,
            at: Utc::now(),
            changed_paths,
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
        self.create_root_inner(path, name, flavor).map_err(to_files_error)
    }

    async fn list_roots(&self) -> Result<Vec<FileRootInfo>, FilesError> {
        Ok(self.registry.list())
    }

    async fn get_root(&self, id: Uuid) -> Result<FileRootInfo, FilesError> {
        self.get_root_info(id).map_err(to_files_error)
    }

    async fn browse(&self, root_id: Uuid, subpath: String) -> Result<Vec<BrowseEntry>, FilesError> {
        self.browse_inner(root_id, subpath).map_err(to_files_error)
    }

    async fn drive_browse(&self, path: String) -> Result<Vec<BrowseEntry>, FilesError> {
        self.drive_browse_inner(path).map_err(to_files_error)
    }

    async fn chain(&self, root_id: Uuid, path: String) -> Result<Vec<ChainEntry>, FilesError> {
        self.chain_inner(root_id, path).map_err(to_files_error)
    }

    async fn checkpoint_now(
        &self,
        root_id: Uuid,
        description: Option<String>,
    ) -> Result<CheckpointInfo, FilesError> {
        self.checkpoint_now_inner(root_id, description)
            .map_err(to_files_error)
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
