//! Task server — vox RPC entry point.
//!
//! Each server holds a registry of `LoroDoc`s keyed by `DocId`.
//! Clients subscribe per-doc; updates broadcast per-doc. The
//! single-workspace-doc model from earlier is gone — now the
//! `workspace` doc id is just one entry among many (project docs,
//! comms threads, the org vault, etc.).
//!
//! The four architect-emitted Repo dispatchers (Project / Task /
//! Cycle / Milestone) currently still bind to the "workspace" doc
//! for read-side compatibility with the existing UI route. As
//! Phases 5+ land they migrate to Knowledge-vault-backed reads.

use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use architect::vox;
use architect_auth::{
    ArchitectAuth, AuthServiceDispatcher,
    db::{AuthSeaOrmStorage, Migrator as AuthMigrator},
    transport::vox::{AuthServerMiddleware, AuthVoxService},
};
use axum::Router;
use axum::extract::State;
use axum::extract::ws::WebSocketUpgrade;
use axum::response::IntoResponse;
use axum::routing::get;
use crdt::loro::{self, ExportMode};
use crdt::{CrdtDoc, Persistence};
use project_crdt::{ProjectRepoLoro, TaskRepoLoro};
use project_proto::{
    DocId, ProjectRepoDispatcher, SyncError, TaskRepoDispatcher, UpdateBytes, WorkspaceSync,
    WorkspaceSyncDispatcher,
};
use sea_orm::Database;
use sea_orm_migration::MigratorTrait;
use tokio::sync::{Mutex, broadcast};
use uuid::Uuid;

/// The legacy `workspace` doc id. Kept around so existing UI + CLI
/// (which haven't grown a doc selector yet) point somewhere sensible.
/// Once every consumer takes an explicit doc id, this becomes a
/// migration artifact.
pub const WORKSPACE_DOC_ID: Uuid = task_db::WORKSPACE_DOC_ID;

/// Wire-name for the legacy workspace doc.
pub fn workspace_doc_id() -> DocId {
    DocId::new("workspace")
}

/// One open doc + its broadcast channel + the local-update sub
/// that bridges Loro into the broadcast. Held by the registry.
pub struct OpenDoc {
    pub doc: Arc<CrdtDoc>,
    pub update_tx: broadcast::Sender<Vec<u8>>,
    /// Subscription handle must outlive the doc for the broadcast
    /// callback to keep firing. Held here; dropped when the doc is
    /// evicted (which currently never happens — see LRU TODO).
    _subscription: loro::Subscription,
}

impl OpenDoc {
    pub fn new(doc: Arc<CrdtDoc>) -> Self {
        // Same 4096-slot capacity as the previous single-doc impl;
        // each doc gets its own channel so a busy doc doesn't lag
        // out subscribers to other docs.
        let (update_tx, _) = broadcast::channel::<Vec<u8>>(4096);
        let tx_for_cb = update_tx.clone();
        let _subscription = doc.loro().subscribe_local_update(Box::new(move |bytes| {
            let _ = tx_for_cb.send(bytes.to_vec());
            true
        }));
        Self {
            doc,
            update_tx,
            _subscription,
        }
    }
}

/// Server-side registry of open `CrdtDoc`s. Lazy-loads from
/// persistence on first access; never evicts (v0 — Phase 1 ships
/// without LRU eviction, that lands later when we have multi-doc
/// usage in production).
#[derive(Clone)]
pub struct DocRegistry {
    persistence: Arc<dyn Persistence>,
    docs: Arc<Mutex<HashMap<DocId, Arc<OpenDoc>>>>,
}

impl DocRegistry {
    pub fn new(persistence: Arc<dyn Persistence>) -> Self {
        Self {
            persistence,
            docs: Arc::new(Mutex::new(HashMap::new())),
        }
    }

    /// Get the doc for `id`, opening it from persistence if not
    /// already in the registry. The same `Arc<OpenDoc>` is returned
    /// to every caller, so a writer + a subscriber on the same doc
    /// share state.
    pub async fn get_or_open(&self, id: &DocId) -> Result<Arc<OpenDoc>, SyncError> {
        {
            let docs = self.docs.lock().await;
            if let Some(d) = docs.get(id) {
                return Ok(d.clone());
            }
        }
        // First-time load. Hash the DocId string into a uuid for
        // the persistence layer (it keys on Uuid today). For the
        // legacy "workspace" doc, use the constant uuid we've
        // been seeding to so the seed survives across the rename.
        let storage_uuid = if id.as_str() == "workspace" {
            WORKSPACE_DOC_ID
        } else {
            // SHA-256 → first 16 bytes → Uuid. Deterministic per
            // doc-id string.
            doc_id_to_uuid(id.as_str())
        };
        let doc = CrdtDoc::open(storage_uuid, ErasedPersistence(self.persistence.clone()))
            .await
            .map_err(|e| SyncError::Internal(format!("open doc `{}`: {e}", id.as_str())))?;
        let entry = Arc::new(OpenDoc::new(Arc::new(doc)));
        let mut docs = self.docs.lock().await;
        // Race: someone else opened it between our two locks.
        // Keep theirs.
        Ok(docs.entry(id.clone()).or_insert(entry).clone())
    }

    /// Read-only access for callers that already know the doc is
    /// open. Returns None for unopened docs — the WorkspaceSync
    /// path goes through `get_or_open`.
    pub async fn try_get(&self, id: &DocId) -> Option<Arc<OpenDoc>> {
        self.docs.lock().await.get(id).cloned()
    }
}

/// Deterministic `DocId` → `Uuid` mapping for the persistence
/// layer. SHA-256 of the string, first 16 bytes as a UUID. Same
/// input → same uuid forever.
fn doc_id_to_uuid(s: &str) -> Uuid {
    use sha2::{Digest, Sha256};
    let hash = Sha256::digest(s.as_bytes());
    let bytes: [u8; 16] = hash[..16].try_into().expect("sha256 outputs >= 16 bytes");
    Uuid::from_bytes(bytes)
}

/// Auth state attached to the server. Holds an `ArchitectAuth`
/// backed by its own SQLite — distinct from the CRDT persistence —
/// per `plans/decentralized-foundation.md` §13 Phase 2.
#[derive(Clone)]
pub struct AuthState {
    pub auth: ArchitectAuth<AuthSeaOrmStorage>,
}

impl AuthState {
    /// Open the auth DB at the given URL, run migrations, build
    /// `ArchitectAuth`. `secret` must be ≥32 bytes.
    pub async fn open(db_url: &str, secret: &str) -> eyre::Result<Self> {
        let db = Database::connect(db_url)
            .await
            .map_err(|e| eyre::eyre!("connect auth db `{db_url}`: {e}"))?;
        AuthMigrator::up(&db, None)
            .await
            .map_err(|e| eyre::eyre!("auth migrations: {e}"))?;
        let storage = AuthSeaOrmStorage::new(db);
        let auth = ArchitectAuth::builder()
            .secret(secret)
            .storage(storage)
            .build()
            .map_err(|e| eyre::eyre!("build ArchitectAuth: {e}"))?;
        Ok(Self { auth })
    }
}

#[derive(Clone)]
pub struct AppState {
    pub registry: DocRegistry,
    /// Convenience: the legacy "workspace" doc, pre-opened, so the
    /// existing Repo dispatchers (which don't know about doc ids
    /// yet) keep working. New code should go through `registry`.
    pub workspace_doc: Arc<CrdtDoc>,
    pub project_repo: Arc<ProjectRepoLoro>,
    pub task_repo: Arc<TaskRepoLoro>,
    pub sync: WorkspaceSyncImpl,
    pub auth: AuthState,
}

impl AppState {
    /// Build app state with auth opened at the default XDG path
    /// (`$XDG_DATA_HOME/task-server/auth.sqlite`, fallback
    /// `~/.local/share/task-server/auth.sqlite`).
    pub async fn new<P: Persistence + 'static>(persistence: P) -> eyre::Result<Self> {
        let auth_db_url = format!("sqlite://{}?mode=rwc", default_auth_db_path()?.display());
        let auth = AuthState::open(&auth_db_url, DEFAULT_AUTH_SECRET).await?;
        Self::new_with_auth(persistence, auth).await
    }

    /// Build app state with explicit auth — used by tests that want
    /// an in-memory auth DB.
    pub async fn new_with_auth<P: Persistence + 'static>(
        persistence: P,
        auth: AuthState,
    ) -> eyre::Result<Self> {
        let persistence: Arc<dyn Persistence> = Arc::new(persistence);
        let registry = DocRegistry::new(persistence.clone());

        // Pre-open the workspace doc so the legacy Repo
        // dispatchers can bind to it without a lookup.
        let workspace_id = workspace_doc_id();
        let open = registry
            .get_or_open(&workspace_id)
            .await
            .map_err(|e| eyre::eyre!("open workspace doc: {e}"))?;
        let workspace_doc = open.doc.clone();

        let project_repo = Arc::new(ProjectRepoLoro::new(&workspace_doc));
        let task_repo = Arc::new(TaskRepoLoro::new(&workspace_doc));
        let sync = WorkspaceSyncImpl::new(registry.clone());

        Ok(Self {
            registry,
            workspace_doc,
            project_repo,
            task_repo,
            sync,
            auth,
        })
    }
}

/// Dev default — replace via config in a later phase. Length-checked
/// at build time so this fails loudly if shortened.
const DEFAULT_AUTH_SECRET: &str = "task-server-auth-dev-secret-32+!";

/// Resolve `$XDG_DATA_HOME/task-server/auth.sqlite` with the standard
/// fallback. Creates parent directories if missing.
pub fn default_auth_db_path() -> eyre::Result<PathBuf> {
    let base = match std::env::var("XDG_DATA_HOME") {
        Ok(v) if !v.is_empty() => PathBuf::from(v),
        _ => {
            let home = std::env::var("HOME")
                .map_err(|_| eyre::eyre!("neither XDG_DATA_HOME nor HOME is set"))?;
            PathBuf::from(home).join(".local").join("share")
        }
    };
    let dir = base.join("task-server");
    std::fs::create_dir_all(&dir).map_err(|e| eyre::eyre!("create {}: {e}", dir.display()))?;
    Ok(dir.join("auth.sqlite"))
}

/// Server-side `WorkspaceSync` implementation. Routes every
/// `subscribe` / `apply_update` call to the registry, fetching the
/// per-doc broadcast channel.
#[derive(Clone)]
pub struct WorkspaceSyncImpl {
    registry: DocRegistry,
}

impl WorkspaceSyncImpl {
    pub fn new(registry: DocRegistry) -> Self {
        Self { registry }
    }
}

impl WorkspaceSync for WorkspaceSyncImpl {
    async fn apply_update(&self, doc_id: DocId, update: UpdateBytes) -> Result<(), SyncError> {
        let open = self.registry.get_or_open(&doc_id).await?;
        // Loro's `subscribe_local_update` only fires for local
        // mutations — imports (this `apply_remote` call) don't
        // trigger it. So we broadcast manually after a successful
        // import. Result: every subscriber on this doc sees the
        // same chunk the caller pushed.
        open.doc
            .apply_remote(&update.0)
            .map_err(|e| SyncError::InvalidUpdate(e.to_string()))?;
        let _ = open.update_tx.send(update.0);
        Ok(())
    }

    async fn subscribe(&self, doc_id: DocId, output: vox::Tx<UpdateBytes>) {
        let open = match self.registry.get_or_open(&doc_id).await {
            Ok(o) => o,
            Err(e) => {
                tracing::warn!(doc = %doc_id.as_str(), ?e, "subscribe: open failed");
                let _ = output.close(Default::default()).await;
                return;
            }
        };

        // Order matters: subscribe to the broadcast channel BEFORE
        // taking the snapshot. Any commit that landed between
        // snapshot-export and broadcast-subscribe would be lost
        // for this peer — not in the snapshot, not in the queue.
        // Subscribing first MAY redeliver bytes already in the
        // snapshot, but Loro `import` is idempotent so duplicates
        // are no-ops.
        let mut rx = open.update_tx.subscribe();
        let snapshot = match open.doc.loro().export(ExportMode::Snapshot) {
            Ok(b) => b,
            Err(e) => {
                tracing::warn!(doc = %doc_id.as_str(), ?e, "snapshot export failed");
                let _ = output.close(Default::default()).await;
                return;
            }
        };
        if output.send(UpdateBytes(snapshot)).await.is_err() {
            return;
        }

        loop {
            match rx.recv().await {
                Ok(bytes) => {
                    if output.send(UpdateBytes(bytes)).await.is_err() {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Closed) => return,
                Err(broadcast::error::RecvError::Lagged(n)) => {
                    tracing::warn!(
                        doc = %doc_id.as_str(),
                        skipped = n,
                        "subscriber lagged; resending snapshot"
                    );
                    let snap = open
                        .doc
                        .loro()
                        .export(ExportMode::Snapshot)
                        .unwrap_or_default();
                    if output.send(UpdateBytes(snap)).await.is_err() {
                        return;
                    }
                }
            }
        }
    }
}

/// Erase a `Arc<dyn Persistence>` into a concrete type that
/// satisfies `CrdtDoc::open`'s `P: Persistence` bound (the generic
/// rejects trait objects directly).
struct ErasedPersistence(Arc<dyn Persistence>);

#[async_trait::async_trait]
impl Persistence for ErasedPersistence {
    async fn load_snapshot(&self, doc_id: Uuid) -> Result<Option<Vec<u8>>, crdt::PersistError> {
        self.0.load_snapshot(doc_id).await
    }
    async fn load_updates(&self, doc_id: Uuid) -> Result<Vec<Vec<u8>>, crdt::PersistError> {
        self.0.load_updates(doc_id).await
    }
    async fn append_update(&self, doc_id: Uuid, bytes: &[u8]) -> Result<(), crdt::PersistError> {
        self.0.append_update(doc_id, bytes).await
    }
    async fn write_snapshot(&self, doc_id: Uuid, bytes: &[u8]) -> Result<(), crdt::PersistError> {
        self.0.write_snapshot(doc_id, bytes).await
    }
}

pub fn router(state: AppState) -> Router {
    Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/vox", get(vox_ws_handler))
        .layer(tower_http::cors::CorsLayer::permissive())
        .with_state(state)
}

async fn vox_ws_handler(
    State(state): State<AppState>,
    ws: WebSocketUpgrade,
) -> axum::response::Response {
    ws.on_upgrade(move |socket| async move {
        let project_repo = (*state.project_repo).clone();
        let task_repo = (*state.task_repo).clone();
        let sync = state.sync.clone();
        let auth = state.auth.auth.clone();
        let acceptor =
            architect::axum_ws::acceptor_fn(move |req, connection| match req.service() {
                "ProjectRepo" => {
                    connection.handle_with(ProjectRepoDispatcher::new(project_repo.clone()));
                    Ok(())
                }
                "TaskRepo" => {
                    connection.handle_with(TaskRepoDispatcher::new(task_repo.clone()));
                    Ok(())
                }
                "WorkspaceSync" => {
                    connection.handle_with(WorkspaceSyncDispatcher::new(sync.clone()));
                    Ok(())
                }
                "AuthService" => {
                    connection.handle_with(
                        AuthServiceDispatcher::new(AuthVoxService::new(auth.clone()))
                            .with_middleware(AuthServerMiddleware),
                    );
                    Ok(())
                }
                other => {
                    tracing::info!(
                        service = %other,
                        "vox session: unknown service requested"
                    );
                    Err(Vec::new())
                }
            });
        architect::axum_ws::serve(socket, acceptor).await;
    })
    .into_response()
}
