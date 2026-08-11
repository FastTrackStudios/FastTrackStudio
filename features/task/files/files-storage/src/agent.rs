//! The storage agent, in-server hosting (glossary "Storage agent": "one
//! protocol, three hostings" — this is the first, "in-process in
//! task-server for its own volumes").
//!
//! An agent does the three things the coordinator cannot, because the
//! coordinator is never the data path (issue #230): it **hosts** a
//! root's live tree (creating the tree and initializing the
//! authoritative version-store repo inside it, per ADR 0001),
//! **measures** the logical bytes that tree references (quota is charged
//! in logical bytes, and only the holder of the authoritative repo can
//! count them), and **replicates** the root's version-store blobs onto a
//! second location.
//!
//! Everything here is synchronous, driven through `pollster::block_on`
//! wherever it touches the version store / chunk store — the same
//! constraint `files`' backend documents: jj-lib's futures are not
//! `Send` on every path, so they must never be awaited from inside an
//! `#[architect::rpc]` method's own future. Callers run these on
//! `tokio::task::spawn_blocking`.

use std::collections::{BTreeSet, HashMap};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex};

use files_proto::consts::STORE_DIR;
use files_storage_proto::{AgentDirective, DirectiveKind, DirectiveOutcome};
use jj_lib::backend::TreeValue;
use jj_lib::object_id::ObjectId as _;
use jj_lib::repo::{ReadonlyRepo, Repo as _};
use task_files_chunk_store::{ChunkStore, FileId};
use task_files_version_store::VersionStoreBackend;
use uuid::Uuid;

use crate::error::{Error, Result};

/// What a local (in-process) agent can be asked to do. The wire protocol
/// ([`files_storage_proto::StorageAgentService`]) is the same contract
/// for remote hostings; this trait is how the coordinator reaches an
/// agent living in its own process without a round trip through vox.
pub trait LocalAgent: Send + Sync + 'static {
    fn id(&self) -> Uuid;
    /// Carry out `directive`, blocking until it is done. Errors are
    /// reported as [`DirectiveOutcome::Failed`] rather than returned —
    /// a failing directive is a placement outcome, not a coordinator
    /// fault.
    fn execute(&self, directive: &AgentDirective) -> DirectiveOutcome;
}

/// What one measurement pass found in a live tree.
#[derive(Debug, Clone, Default)]
pub struct Measured {
    /// Distinct version-store files reachable from the repo's heads.
    pub files: BTreeSet<FileId>,
    /// Their total length — logical bytes, counted once per distinct
    /// file version and NOT discounted for chunk-level dedup (dedup
    /// savings belong to the operator, issue #230).
    pub logical_bytes: u64,
}

/// The in-server agent: speaks for volumes the server itself owns.
///
/// It keeps one repo handle per live tree for the process's lifetime.
/// That is a cache, but it is also a correctness measure: two handles on
/// one version store in a single process is the shape that wedged
/// `files`' own restart test (PR #280 review), so this agent opens each
/// store exactly once.
pub struct InServerAgent {
    id: Uuid,
    repos: Mutex<HashMap<PathBuf, Arc<ReadonlyRepo>>>,
}

impl std::fmt::Debug for InServerAgent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("InServerAgent")
            .field("id", &self.id)
            .finish_non_exhaustive()
    }
}

impl InServerAgent {
    #[must_use]
    pub fn new(id: Uuid) -> Self {
        Self {
            id,
            repos: Mutex::new(HashMap::new()),
        }
    }

    /// The authoritative repo for a live tree — ADR 0001's "the Storage
    /// agent hosting a root's live tree owns the authoritative repo",
    /// made reachable to in-process consumers so they use *this* handle
    /// rather than opening a second one on the same store.
    ///
    /// The handle is a snapshot of the op log at the moment it was
    /// loaded; [`InServerAgent::repo_at_head`] is the one to use when
    /// newer operations (a checkpoint written since) must be visible.
    pub fn authoritative_repo(&self, live_tree: &Path) -> Result<Arc<ReadonlyRepo>> {
        self.repo(live_tree)
    }

    /// The authoritative repo, reloaded at the current operation head.
    /// Measurement and replication go through this: the cadence engine
    /// (issue #260) writes checkpoints through its own handle on the same
    /// store, and a stale snapshot would silently under-count them.
    /// Reloading reuses the repo's existing `Store`, so no second backend
    /// — and no second chunk store — is ever opened on the same
    /// directory.
    pub fn repo_at_head(&self, live_tree: &Path) -> Result<Arc<ReadonlyRepo>> {
        let repo = self.repo(live_tree)?;
        let reloaded = pollster::block_on(repo.loader().load_at_head())
            .map_err(|e| Error::Repo(e.to_string()))?;
        self.repos
            .lock()
            .expect("agent repo cache poisoned")
            .insert(live_tree.join(STORE_DIR), reloaded.clone());
        Ok(reloaded)
    }

    /// The authoritative repo for a live tree, opened (and initialized on
    /// first touch) exactly once per process.
    fn repo(&self, live_tree: &Path) -> Result<Arc<ReadonlyRepo>> {
        let store_dir = live_tree.join(STORE_DIR);
        {
            let repos = self.repos.lock().expect("agent repo cache poisoned");
            if let Some(repo) = repos.get(&store_dir) {
                return Ok(repo.clone());
            }
        }
        let repo = task_files_version_store::repo::open_or_init_repo_blocking(&store_dir)?;
        self.repos
            .lock()
            .expect("agent repo cache poisoned")
            .insert(store_dir, repo.clone());
        Ok(repo)
    }

    /// Create the live tree and initialize the authoritative repo inside
    /// it. Idempotent: hosting an already-hosted tree reopens it.
    pub fn host_live_tree(&self, live_tree: &Path) -> Result<()> {
        std::fs::create_dir_all(live_tree)?;
        self.repo(live_tree)?;
        Ok(())
    }

    /// Walk every head of the live tree's repo and total what it
    /// references.
    pub fn measure(&self, live_tree: &Path) -> Result<Measured> {
        let repo = self.repo_at_head(live_tree)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| Error::Repo("live tree's repo is not a VersionStoreBackend".into()))?;
        let heads: Vec<_> = repo.view().heads().iter().cloned().collect();

        pollster::block_on(async {
            let mut files: BTreeSet<FileId> = BTreeSet::new();
            let mut seen_trees = BTreeSet::new();
            for head in &heads {
                let commit = backend.commit(head).await?;
                // A conflicted (unresolved) root tree is a divergence the
                // UI resolves (ADR 0001); it carries no single tree to
                // walk, so it contributes nothing to this measurement.
                let Ok(tree_id) = commit.root_tree.clone().into_resolved() else {
                    continue;
                };
                let mut stack = vec![tree_id];
                while let Some(id) = stack.pop() {
                    if !seen_trees.insert(id.clone()) {
                        continue;
                    }
                    let tree = backend.tree(&id).await?;
                    for entry in tree.entries() {
                        match entry.value() {
                            TreeValue::Tree(sub) => stack.push(sub.clone()),
                            TreeValue::File { id, .. } => {
                                if let Ok(file_id) = FileId::from_hex(&id.hex()) {
                                    files.insert(file_id);
                                }
                            }
                            TreeValue::Symlink(_) | TreeValue::GitSubmodule(_) => {}
                        }
                    }
                }
            }
            let mut logical_bytes = 0u64;
            for file_id in &files {
                logical_bytes = logical_bytes
                    .saturating_add(backend.chunks().manifest(*file_id).await?.total_len());
            }
            Ok(Measured {
                files,
                logical_bytes,
            })
        })
    }

    /// Copy every version-store blob the live tree references into a
    /// chunk store at `dest`. Streaming, chunk at a time, through an
    /// in-memory pipe — a multi-GB file is never buffered whole, which
    /// is the whole point of the CAS substrate's streaming API.
    ///
    /// Content addressing makes this self-verifying: re-chunking the
    /// same bytes in the destination store must yield the same
    /// [`FileId`], so a silent corruption on the way over fails the copy
    /// rather than producing a plausible replica.
    pub fn replicate(&self, live_tree: &Path, dest: &Path) -> Result<Measured> {
        let measured = self.measure(live_tree)?;
        let repo = self.repo_at_head(live_tree)?;
        let backend = repo
            .store()
            .backend_impl::<VersionStoreBackend>()
            .ok_or_else(|| Error::Repo("live tree's repo is not a VersionStoreBackend".into()))?;
        let source = backend.chunks().clone();

        std::fs::create_dir_all(dest)?;
        let dest = dest.to_path_buf();
        pollster::block_on(async move {
            let target = ChunkStore::open(&dest).await?;
            for file_id in &measured.files {
                if target.has(*file_id).await {
                    continue; // already replicated — resumable by construction
                }
                copy_file(&source, &target, *file_id).await?;
            }
            target.shutdown().await?;
            Ok(measured)
        })
    }

    /// Flush every cached repo's chunk store — call before dropping an
    /// agent whose process is about to reopen the same live trees (a
    /// server exit, or a test simulating a restart). Mirrors
    /// `files::FilesBackend::shutdown`, and for the same reason:
    /// iroh-blobs' `FsStore` may hold buffered writes open until this.
    pub async fn shutdown(&self) {
        let repos: Vec<Arc<ReadonlyRepo>> = self
            .repos
            .lock()
            .expect("agent repo cache poisoned")
            .values()
            .cloned()
            .collect();
        for repo in repos {
            if let Some(backend) = repo.store().backend_impl::<VersionStoreBackend>() {
                let _ = backend.chunks().shutdown().await;
            }
        }
    }
}

/// Stream one file from `source` into `target` without ever holding it
/// whole in memory: the source writes chunks into one end of a duplex
/// pipe while the destination's chunker reads the other.
async fn copy_file(source: &ChunkStore, target: &ChunkStore, file_id: FileId) -> Result<()> {
    use tokio::io::AsyncWriteExt as _;

    let (mut writer, reader) = tokio::io::duplex(64 * 1024);
    let pump = async {
        let out = source.read_to(file_id, &mut writer).await;
        // Always close the pipe, success or not — otherwise the reader
        // below waits forever for an EOF that never comes.
        let _ = writer.shutdown().await;
        out
    };
    let (read_result, written) = tokio::join!(pump, target.write_stream(reader));
    read_result?;
    let written = written?;
    if written != file_id {
        return Err(Error::BadRequest(format!(
            "replicated content addressed to {written:?}, expected {file_id:?}"
        )));
    }
    Ok(())
}

impl LocalAgent for InServerAgent {
    fn id(&self) -> Uuid {
        self.id
    }

    fn execute(&self, directive: &AgentDirective) -> DirectiveOutcome {
        match &directive.kind {
            DirectiveKind::HostLiveTree { absolute_path, .. } => {
                match self.host_live_tree(Path::new(absolute_path)) {
                    Ok(()) => DirectiveOutcome::Hosted {
                        repo_initialized: true,
                    },
                    Err(e) => DirectiveOutcome::Failed {
                        reason: e.to_string(),
                    },
                }
            }
            DirectiveKind::MeasureLiveTree { live_tree_path, .. } => {
                match self.measure(Path::new(live_tree_path)) {
                    Ok(m) => DirectiveOutcome::Measured {
                        files: m.files.len() as u64,
                        logical_bytes: m.logical_bytes,
                    },
                    Err(e) => DirectiveOutcome::Failed {
                        reason: e.to_string(),
                    },
                }
            }
            DirectiveKind::ReplicateBlobs {
                source_path,
                dest_path,
                ..
            } => match self.replicate(Path::new(source_path), Path::new(dest_path)) {
                Ok(m) => DirectiveOutcome::Replicated {
                    files_present: m.files.len() as u64,
                    logical_bytes: m.logical_bytes,
                },
                Err(e) => DirectiveOutcome::Failed {
                    reason: e.to_string(),
                },
            },
        }
    }
}
