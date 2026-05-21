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

pub mod acl;
pub mod anonymous_claim;
pub mod attachments;
pub mod basename_index;
pub mod capability;
pub mod knowledge_index;
pub mod share_link;
pub mod vault_sync;

use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;
use std::sync::Arc;
use std::time::{SystemTime, UNIX_EPOCH};

use crate::anonymous_claim::{AnonymousClaimServiceImpl, InstallSessionMiddleware};
use crate::basename_index::MemoryBasenameIndex;
use crate::capability::{CapabilityScope, ServerKeypair, default_keypair_path};
use crate::knowledge_index::KnowledgeIndexer;
use crate::share_link::{RevocationList, ShareServiceImpl};
use project_proto::ShareServiceDispatcher;

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
use knowledge_proto::{
    BaseRepoDispatcher, BlockRepoDispatcher, FolderRepoDispatcher, KnowledgeTagRepoDispatcher,
    PageRepoDispatcher, VaultRepoDispatcher,
};
use project_crdt::{ProjectRepoLoro, TaskRepoLoro};
use project_proto::{
    AwarenessFrame, AwarenessPublish, AwarenessSubscribe, DocId, ProjectRepoDispatcher, SyncError,
    TaskRepoDispatcher, UpdateBytes, WorkspaceSync, WorkspaceSyncDispatcher,
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

/// Phase 10 — broadcast frame carries the bytes plus the set of
/// root container names touched by the commit. Subscribers that
/// declared a `kinds` filter check the intersection on the
/// server side before forwarding.
#[derive(Debug, Clone)]
pub struct UpdateFrame {
    pub bytes: Vec<u8>,
    /// Root container names touched. Empty = unknown (legacy /
    /// initial snapshot; filtered subscribers treat it as
    /// "everything" so they don't silently miss state).
    pub roots: Vec<String>,
}

/// One open doc + its broadcast channel + the local-update sub
/// that bridges Loro into the broadcast. Held by the registry.
pub struct OpenDoc {
    pub doc: Arc<CrdtDoc>,
    pub update_tx: broadcast::Sender<UpdateFrame>,
    /// The set of root container names touched by the *most recent*
    /// commit. Filled by the `subscribe_root` callback before
    /// `subscribe_local_update` fires, so the local-update path
    /// can read it. Phase 10 — drives per-kind filtering.
    pub last_touched_roots: Arc<std::sync::Mutex<Vec<String>>>,
    /// Per-doc awareness store — holds remote peers' ephemeral
    /// state (cursors, presence) keyed by `cursor::<peer_uuid>`.
    /// 30s timeout: peers that stop publishing are eligible for
    /// `remove_outdated()` purge.
    pub awareness: Arc<crdt::awareness::EphemeralStore>,
    /// Fan-out channel for awareness frames. `subscribe_awareness`
    /// returns a Tx that receives every published frame for this
    /// doc; `publish_awareness` pushes here after applying to
    /// the local store.
    pub awareness_tx: broadcast::Sender<AwarenessFrame>,
    /// Subscription handles must outlive the doc for the broadcast
    /// callbacks to keep firing. Held here; dropped when the doc is
    /// evicted.
    _local_update_subscription: loro::Subscription,
    _root_subscription: loro::Subscription,
}

impl OpenDoc {
    pub fn new(doc: Arc<CrdtDoc>) -> Self {
        let (update_tx, _) = broadcast::channel::<UpdateFrame>(4096);
        let (awareness_tx, _) = broadcast::channel::<AwarenessFrame>(1024);
        let awareness = Arc::new(crdt::awareness::EphemeralStore::new(30_000));
        let last_touched_roots = Arc::new(std::sync::Mutex::new(Vec::<String>::new()));

        // Subscribe to the root — fires BEFORE local-update for any
        // commit. We stash the touched roots so the local-update
        // callback can attach them to the broadcast frame.
        let roots_for_diff = last_touched_roots.clone();
        let _root_subscription = doc.loro().subscribe_root(Arc::new(move |event| {
            let mut names: Vec<String> = Vec::new();
            for diff in event.events.iter() {
                if let loro::ContainerID::Root { name, .. } = diff.target {
                    let s = name.to_string();
                    if !names.contains(&s) {
                        names.push(s);
                    }
                }
            }
            *roots_for_diff.lock().unwrap() = names;
        }));

        let tx_for_cb = update_tx.clone();
        let roots_for_local = last_touched_roots.clone();
        let _local_update_subscription =
            doc.loro().subscribe_local_update(Box::new(move |bytes| {
                let roots = roots_for_local.lock().unwrap().clone();
                let _ = tx_for_cb.send(UpdateFrame {
                    bytes: bytes.to_vec(),
                    roots,
                });
                true
            }));
        Self {
            doc,
            update_tx,
            last_touched_roots,
            awareness,
            awareness_tx,
            _local_update_subscription,
            _root_subscription,
        }
    }
}

/// Default LRU cap on the doc registry. Overridable via
/// `TASK_SERVER_DOC_CACHE_CAP` env var. Phase 3 brings up eviction
/// (Phase 1 deferred it).
pub const DEFAULT_DOC_CACHE_CAP: usize = 256;

/// Server-side registry of open `CrdtDoc`s. Lazy-loads from
/// persistence on first access. LRU-bounded: when the registry hits
/// `cap`, the least-recently-used doc is dropped from memory. Its
/// state survives in persistence so re-opening rehydrates it.
#[derive(Clone)]
pub struct DocRegistry {
    persistence: Arc<dyn Persistence>,
    inner: Arc<Mutex<RegistryInner>>,
    cap: usize,
}

struct RegistryInner {
    docs: HashMap<DocId, Arc<OpenDoc>>,
    /// MRU at the back, LRU at the front. On access, we re-push the
    /// id to the back; on eviction, we pop from the front.
    order: VecDeque<DocId>,
}

impl DocRegistry {
    pub fn new(persistence: Arc<dyn Persistence>) -> Self {
        Self::with_cap(persistence, default_doc_cache_cap())
    }

    pub fn with_cap(persistence: Arc<dyn Persistence>, cap: usize) -> Self {
        Self {
            persistence,
            inner: Arc::new(Mutex::new(RegistryInner {
                docs: HashMap::new(),
                order: VecDeque::new(),
            })),
            cap: cap.max(1),
        }
    }

    /// Get the doc for `id`, opening it from persistence if not
    /// already in the registry. The same `Arc<OpenDoc>` is returned
    /// to every caller, so a writer + a subscriber on the same doc
    /// share state.
    pub async fn get_or_open(&self, id: &DocId) -> Result<Arc<OpenDoc>, SyncError> {
        {
            let mut inner = self.inner.lock().await;
            if let Some(d) = inner.docs.get(id).cloned() {
                touch_order(&mut inner.order, id);
                return Ok(d);
            }
        }
        // First-time load. Hash the DocId string into a uuid for
        // the persistence layer (it keys on Uuid today). For the
        // legacy "workspace" doc, use the constant uuid we've
        // been seeding to so the seed survives across the rename.
        let storage_uuid = if id.as_str() == "workspace" {
            WORKSPACE_DOC_ID
        } else {
            doc_id_to_uuid(id.as_str())
        };
        let doc = CrdtDoc::open(storage_uuid, ErasedPersistence(self.persistence.clone()))
            .await
            .map_err(|e| SyncError::Internal(format!("open doc `{}`: {e}", id.as_str())))?;
        let entry = Arc::new(OpenDoc::new(Arc::new(doc)));
        let mut inner = self.inner.lock().await;
        // Race: someone else opened it between our two locks. Keep theirs.
        if let Some(d) = inner.docs.get(id).cloned() {
            touch_order(&mut inner.order, id);
            return Ok(d);
        }
        inner.docs.insert(id.clone(), entry.clone());
        inner.order.push_back(id.clone());
        // Evict LRUs until under cap.
        while inner.docs.len() > self.cap {
            if let Some(victim) = inner.order.pop_front() {
                if &victim == id {
                    // Don't evict the freshly-inserted entry; put it back.
                    inner.order.push_back(victim);
                    break;
                }
                if inner.docs.remove(&victim).is_some() {
                    tracing::debug!(doc = %victim.as_str(), "doc registry: evicted LRU");
                }
            } else {
                break;
            }
        }
        Ok(entry)
    }

    /// Read-only access for callers that already know the doc is
    /// open. Returns None for unopened docs — the WorkspaceSync
    /// path goes through `get_or_open`.
    pub async fn try_get(&self, id: &DocId) -> Option<Arc<OpenDoc>> {
        self.inner.lock().await.docs.get(id).cloned()
    }

    /// Current in-memory doc count. Tests use this to assert eviction.
    pub async fn open_count(&self) -> usize {
        self.inner.lock().await.docs.len()
    }

    /// Snapshot of every open doc id. Phase 8 — used by the
    /// federation UI's `list_docs` RPC to enumerate per-server
    /// state without exposing the full `OpenDoc` arc.
    pub async fn open_doc_ids(&self) -> Vec<DocId> {
        self.inner.lock().await.docs.keys().cloned().collect()
    }
}

fn touch_order(order: &mut VecDeque<DocId>, id: &DocId) {
    if let Some(pos) = order.iter().position(|d| d == id) {
        order.remove(pos);
    }
    order.push_back(id.clone());
}

fn default_doc_cache_cap() -> usize {
    std::env::var("TASK_SERVER_DOC_CACHE_CAP")
        .ok()
        .and_then(|s| s.parse().ok())
        .unwrap_or(DEFAULT_DOC_CACHE_CAP)
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
    pub keypair: ServerKeypair,
    /// When true, every vox connection must present `?cap=<token>`
    /// and the impl enforces the scope. When false (legacy / dev),
    /// the impl runs without enforcement so the existing UI + sync
    /// tests keep working until Phase 4 issues capabilities through
    /// the share-link service. Tests opt in.
    pub enforce_capability: bool,
    pub share_service: ShareServiceImpl,
    pub revocations: RevocationList,
    pub basename_index: MemoryBasenameIndex,
    /// Org vault doc, pre-opened. Phase 5 wires the Knowledge repo
    /// dispatchers against this single doc; later phases lift the
    /// single-doc binding so per-project vaults can be opened on
    /// demand.
    pub org_vault_doc: Arc<CrdtDoc>,
    pub vault_repo: Arc<knowledge_crdt::VaultRepoLoro>,
    pub folder_repo: Arc<knowledge_crdt::FolderRepoLoro>,
    pub page_repo: Arc<knowledge_crdt::PageRepoLoro>,
    pub block_repo: Arc<knowledge_crdt::BlockRepoLoro>,
    pub knowledge_tag_repo: Arc<knowledge_crdt::KnowledgeTagRepoLoro>,
    pub base_repo: Arc<knowledge_crdt::BaseRepoLoro>,
    pub indexer: KnowledgeIndexer,
    pub attachments: Arc<attachments::AttachmentServiceImpl>,
    pub anonymous_claim: AnonymousClaimServiceImpl<AuthSeaOrmStorage>,
    /// Phase 2 vault file-replication. Carries the per-vault
    /// filesystem root + the broadcast channels behind
    /// [`vault_sync_proto::VaultSync`]. Mounted as one more arm on
    /// the `/vox` route alongside every other architect/vox
    /// service.
    pub vault_sync: vault_sync::VaultSyncState,
}

impl AppState {
    /// Build app state with auth opened at the default XDG path
    /// (`$XDG_DATA_HOME/task-server/auth.sqlite`, fallback
    /// `~/.local/share/task-server/auth.sqlite`) and the server
    /// keypair at `$XDG_DATA_HOME/task-server/server-key.ed25519`
    /// (auto-generated on first boot). Capability enforcement off —
    /// flip via `with_capability_enforcement`.
    pub async fn new<P: Persistence + 'static>(persistence: P) -> eyre::Result<Self> {
        let auth_db_url = format!("sqlite://{}?mode=rwc", default_auth_db_path()?.display());
        let auth = AuthState::open(&auth_db_url, DEFAULT_AUTH_SECRET).await?;
        let keypair = ServerKeypair::load_or_generate(&default_keypair_path()?)
            .map_err(|e| eyre::eyre!("load server keypair: {e}"))?;
        Self::new_inner(persistence, auth, keypair, false).await
    }

    /// Build app state with explicit auth — used by tests that want
    /// an in-memory auth DB. Capability enforcement off.
    pub async fn new_with_auth<P: Persistence + 'static>(
        persistence: P,
        auth: AuthState,
    ) -> eyre::Result<Self> {
        let keypair = ServerKeypair::generate_ephemeral();
        Self::new_inner(persistence, auth, keypair, false).await
    }

    /// Build app state with capability enforcement on, using the
    /// caller's keypair. Used by Phase 3 + 4 tests.
    pub async fn new_with_capability<P: Persistence + 'static>(
        persistence: P,
        auth: AuthState,
        keypair: ServerKeypair,
    ) -> eyre::Result<Self> {
        Self::new_inner(persistence, auth, keypair, true).await
    }

    async fn new_inner<P: Persistence + 'static>(
        persistence: P,
        auth: AuthState,
        keypair: ServerKeypair,
        enforce_capability: bool,
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
        let revocations = RevocationList::new();
        let share_service = ShareServiceImpl::new(keypair.clone(), revocations.clone());
        let basename_index = MemoryBasenameIndex::new();

        // Open the org vault doc. Phase 5 wires the Knowledge
        // dispatchers against this single doc; later phases lift the
        // binding so per-project vaults can be opened on demand.
        let org_vault_open = registry
            .get_or_open(&DocId::org_vault())
            .await
            .map_err(|e| eyre::eyre!("open org vault: {e}"))?;
        let org_vault_doc = org_vault_open.doc.clone();
        let vault_repo = Arc::new(knowledge_crdt::VaultRepoLoro::new(&org_vault_doc));
        let folder_repo = Arc::new(knowledge_crdt::FolderRepoLoro::new(&org_vault_doc));
        let page_repo = Arc::new(knowledge_crdt::PageRepoLoro::new(&org_vault_doc));
        let block_repo = Arc::new(knowledge_crdt::BlockRepoLoro::new(&org_vault_doc));
        let knowledge_tag_repo =
            Arc::new(knowledge_crdt::KnowledgeTagRepoLoro::new(&org_vault_doc));
        let base_repo = Arc::new(knowledge_crdt::BaseRepoLoro::new(&org_vault_doc));

        // Knowledge indexer — rebuilds frontmatter / backlink /
        // basename indexes on every org-vault commit. The
        // subscribe_local_update callback fires inside Loro's
        // mutation path; we don't .await there, so kick the rebuild
        // off into a background task. Cheap rebuild for the
        // vertical-slice scale.
        let indexer = KnowledgeIndexer::new(
            (*page_repo).clone(),
            (*block_repo).clone(),
            basename_index.clone(),
        );
        {
            let indexer_for_cb = indexer.clone();
            let sub = org_vault_doc
                .loro()
                .subscribe_local_update(Box::new(move |_bytes| {
                    let idx = indexer_for_cb.clone();
                    tokio::spawn(async move {
                        if let Err(e) = idx.rebuild().await {
                            tracing::warn!(?e, "knowledge indexer rebuild failed");
                        }
                    });
                    true
                }));
            // Subscription handle lives as long as the doc, which
            // lives as long as the registry, which lives as long as
            // AppState. Leak deliberately — see the same pattern in
            // OpenDoc.
            std::mem::forget(sub);
        }

        // Phase 7 — attachments. Blob store lives at the standard
        // XDG path by default. Tests override via `with_capability`
        // / `new_with_auth` constructors only; the blob root is
        // computed inside `new_inner` so it's always created.
        let blob_root =
            attachments::default_blob_root().map_err(|e| eyre::eyre!("blob root: {e}"))?;
        let object_store: Arc<dyn attachments::ObjectStore> =
            Arc::new(attachments::LocalFsStore::new(blob_root));
        let public_base_url = std::env::var("TASK_SERVER_PUBLIC_URL").unwrap_or_default();
        let attachment_service = Arc::new(attachments::AttachmentServiceImpl::new(
            keypair.clone(),
            object_store,
            public_base_url,
        ));

        // Phase 2 — vault file-replication. Storage root defaults
        // to `$XDG_DATA_HOME/task-server/vaults` (or
        // `~/.local/share/task-server/vaults`), overridable via
        // `TASK_SERVER_VAULT_ROOT` for tests / containers.
        let vault_root = std::env::var("TASK_SERVER_VAULT_ROOT")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| {
                dirs_local_share()
                    .unwrap_or_else(|| std::path::PathBuf::from("./vaults"))
                    .join("task-server")
                    .join("vaults")
            });
        let vault_sync_state = vault_sync::VaultSyncState::new(vault_root)
            .map_err(|e| eyre::eyre!("vault_sync state: {e}"))?;

        // Phase 8 — anonymous claim service. Holds the in-memory
        // (peer_id -> user_id) table; the dispatcher's
        // `InstallSessionMiddleware` copies the caller's session
        // token into it before each method runs.
        let anonymous_claim_service =
            AnonymousClaimServiceImpl::new(auth.auth.clone(), share_service.clone());

        Ok(Self {
            registry,
            workspace_doc,
            project_repo,
            task_repo,
            sync,
            auth,
            keypair,
            enforce_capability,
            share_service,
            revocations,
            basename_index,
            org_vault_doc,
            vault_repo,
            folder_repo,
            page_repo,
            block_repo,
            knowledge_tag_repo,
            base_repo,
            indexer,
            attachments: attachment_service,
            anonymous_claim: anonymous_claim_service,
            vault_sync: vault_sync_state,
        })
    }
}

/// Resolve `now` as unix seconds. Tests stay deterministic by
/// passing `i64::MAX / 2` into scope expiries — verification only
/// compares.
fn now_unix() -> i64 {
    SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0)
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
///
/// Capability enforcement is per-connection: the `vox_ws_handler`
/// builds a fresh `WorkspaceSyncImpl` per WebSocket, optionally
/// scoped to a `CapabilityScope`. When `scope` is `Some`, calls
/// must target a doc the scope allows; `apply_update` also requires
/// `can_write`. `None` = no enforcement, which is the legacy
/// localhost-dev path (matching the pre-Phase-3 behavior).
#[derive(Clone)]
pub struct WorkspaceSyncImpl {
    registry: DocRegistry,
    scope: Option<CapabilityScope>,
}

impl WorkspaceSyncImpl {
    pub fn new(registry: DocRegistry) -> Self {
        Self {
            registry,
            scope: None,
        }
    }

    pub fn with_scope(registry: DocRegistry, scope: CapabilityScope) -> Self {
        Self {
            registry,
            scope: Some(scope),
        }
    }

    fn check_read(&self, doc_id: &DocId) -> Result<(), SyncError> {
        if let Some(scope) = &self.scope {
            // Phase 7 — attachments-only tokens explicitly forbid
            // sync (subscribe + apply_update), even when the doc
            // is in scope.
            if scope.attachments_only {
                return Err(SyncError::Forbidden);
            }
            if !scope.allows_doc(doc_id) {
                return Err(SyncError::Forbidden);
            }
        }
        Ok(())
    }

    fn check_write(&self, doc_id: &DocId) -> Result<(), SyncError> {
        if let Some(scope) = &self.scope {
            if scope.attachments_only {
                return Err(SyncError::Forbidden);
            }
            if !scope.allows_doc(doc_id) || !scope.can_write {
                return Err(SyncError::Forbidden);
            }
        }
        Ok(())
    }
}

impl WorkspaceSync for WorkspaceSyncImpl {
    async fn apply_update(&self, doc_id: DocId, update: UpdateBytes) -> Result<(), SyncError> {
        self.check_write(&doc_id)?;
        let open = self.registry.get_or_open(&doc_id).await?;
        // Loro's `subscribe_local_update` only fires for local
        // mutations — imports (this `apply_remote` call) don't
        // trigger it. So we broadcast manually after a successful
        // import. Phase 10: the touched-roots set comes from the
        // `subscribe_root` callback that fires *during* apply, so
        // by the time we get here `last_touched_roots` is up to
        // date.
        open.doc
            .apply_remote(&update.0)
            .map_err(|e| SyncError::InvalidUpdate(e.to_string()))?;
        let roots = open.last_touched_roots.lock().unwrap().clone();
        let _ = open.update_tx.send(UpdateFrame {
            bytes: update.0,
            roots,
        });
        Ok(())
    }

    async fn subscribe(&self, doc_id: DocId, output: vox::Tx<UpdateBytes>) {
        if self.check_read(&doc_id).is_err() {
            tracing::info!(doc = %doc_id.as_str(), "subscribe: forbidden by capability");
            let _ = output.close(Default::default()).await;
            return;
        }
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
                Ok(frame) => {
                    if output.send(UpdateBytes(frame.bytes)).await.is_err() {
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

    async fn subscribe_kinds(
        &self,
        filter: project_proto::KindFilter,
        output: vox::Tx<UpdateBytes>,
    ) {
        if self.check_read(&filter.doc_id).is_err() {
            tracing::info!(
                doc = %filter.doc_id.as_str(),
                "subscribe_kinds: forbidden by capability"
            );
            let _ = output.close(Default::default()).await;
            return;
        }
        let open = match self.registry.get_or_open(&filter.doc_id).await {
            Ok(o) => o,
            Err(e) => {
                tracing::warn!(doc = %filter.doc_id.as_str(), ?e, "subscribe_kinds: open failed");
                let _ = output.close(Default::default()).await;
                return;
            }
        };

        let mut rx = open.update_tx.subscribe();
        // Always send the snapshot — clients need full state up
        // front, regardless of which kinds they later care about.
        let snapshot = match open.doc.loro().export(ExportMode::Snapshot) {
            Ok(b) => b,
            Err(e) => {
                tracing::warn!(doc = %filter.doc_id.as_str(), ?e, "snapshot export failed");
                let _ = output.close(Default::default()).await;
                return;
            }
        };
        if output.send(UpdateBytes(snapshot)).await.is_err() {
            return;
        }

        let kinds_set: std::collections::HashSet<String> = filter.kinds.iter().cloned().collect();
        let forward_all = kinds_set.is_empty();
        loop {
            match rx.recv().await {
                Ok(frame) => {
                    let should_forward = forward_all
                        || frame.roots.is_empty()
                        || frame.roots.iter().any(|r| kinds_set.contains(r));
                    if !should_forward {
                        continue;
                    }
                    if output.send(UpdateBytes(frame.bytes)).await.is_err() {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Closed) => return,
                Err(broadcast::error::RecvError::Lagged(n)) => {
                    tracing::warn!(
                        doc = %filter.doc_id.as_str(),
                        skipped = n,
                        "subscribe_kinds lagged; resending snapshot"
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

    async fn list_docs(
        &self,
        _req: project_proto::ListDocsRequest,
    ) -> Result<project_proto::DocList, SyncError> {
        // Returns every currently-open doc id. Filtered by scope
        // when one is set — the federation UI can only see docs
        // the connection's capability allows. With no scope (dev
        // mode), every open doc is visible.
        let all = self.registry.open_doc_ids().await;
        let filtered: Vec<DocId> = match &self.scope {
            Some(scope) if scope.attachments_only => Vec::new(),
            Some(scope) => all.into_iter().filter(|d| scope.allows_doc(d)).collect(),
            None => all,
        };
        Ok(project_proto::DocList { doc_ids: filtered })
    }

    async fn subscribe_awareness(&self, sub: AwarenessSubscribe, output: vox::Tx<AwarenessFrame>) {
        if self.check_read(&sub.doc_id).is_err() {
            tracing::info!(doc = %sub.doc_id.as_str(), "subscribe_awareness: forbidden");
            let _ = output.close(Default::default()).await;
            return;
        }
        let open = match self.registry.get_or_open(&sub.doc_id).await {
            Ok(o) => o,
            Err(e) => {
                tracing::warn!(doc = %sub.doc_id.as_str(), ?e, "subscribe_awareness: open failed");
                let _ = output.close(Default::default()).await;
                return;
            }
        };
        let mut rx = open.awareness_tx.subscribe();
        // Snapshot first: every active peer's current state so
        // late joiners see existing cursors immediately.
        let snapshot = open.awareness.encode_all();
        if !snapshot.is_empty() {
            let _ = output
                .send(AwarenessFrame {
                    from_peer: Uuid::nil(),
                    bytes: snapshot,
                })
                .await;
        }
        loop {
            match rx.recv().await {
                Ok(frame) => {
                    // Server-side echo suppression — the publisher
                    // already has its own state locally.
                    if frame.from_peer == sub.peer_id {
                        continue;
                    }
                    if output.send(frame).await.is_err() {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Lagged(n)) => {
                    tracing::warn!(doc = %sub.doc_id.as_str(), %n, "awareness lagged; resending snapshot");
                    let snap = open.awareness.encode_all();
                    if !snap.is_empty()
                        && output
                            .send(AwarenessFrame {
                                from_peer: Uuid::nil(),
                                bytes: snap,
                            })
                            .await
                            .is_err()
                    {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Closed) => return,
            }
        }
    }

    async fn publish_awareness(&self, msg: AwarenessPublish) -> Result<(), SyncError> {
        self.check_write(&msg.doc_id)?;
        let open = self.registry.get_or_open(&msg.doc_id).await?;
        // Apply locally so late joiners get the merged state via
        // `encode_all()` on subscribe.
        if let Err(e) = open.awareness.apply(&msg.frame.bytes) {
            tracing::warn!(doc = %msg.doc_id.as_str(), %e, "awareness apply failed");
            return Err(SyncError::InvalidUpdate(e.to_string()));
        }
        let _ = open.awareness_tx.send(msg.frame);
        Ok(())
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
    use attachments::routes::AttachmentRouteState;

    // Mount the /blobs/* HTTP routes under their own sub-router so
    // they pass an `AttachmentRouteState` (sliver of `AppState`)
    // and don't drag the full `AppState` into the attachment
    // handlers' generic bound.
    let blob_state = AttachmentRouteState {
        service: state.attachments.clone(),
    };
    let blob_router = attachments::attachment_router().with_state(blob_state);

    // Vault file-replication (phase 2) is now another
    // `#[vox::service]` mounted as one more arm in `vox_ws_handler`
    // — no separate REST router, no separate WS upgrade. Storage
    // root + broadcast channels live on `state.vault_sync`.

    Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/vox", get(vox_ws_handler))
        .merge(blob_router)
        .layer(tower_http::cors::CorsLayer::permissive())
        .with_state(state)
}

/// Best-effort `$XDG_DATA_HOME` (falls back to `$HOME/.local/share`).
/// Skips a `dirs` crate dep — we only use this one path.
fn dirs_local_share() -> Option<std::path::PathBuf> {
    if let Some(xdg) = std::env::var_os("XDG_DATA_HOME") {
        return Some(std::path::PathBuf::from(xdg));
    }
    let home = std::env::var_os("HOME")?;
    Some(std::path::PathBuf::from(home).join(".local/share"))
}

async fn vox_ws_handler(
    State(state): State<AppState>,
    axum::extract::Query(params): axum::extract::Query<HashMap<String, String>>,
    ws: WebSocketUpgrade,
) -> axum::response::Response {
    // Capability extraction. If enforcement is on, missing/invalid
    // tokens get a `WorkspaceSyncImpl` whose scope rejects every
    // doc — but the auth + repo dispatchers still run, since
    // capability scoping is sync-only in Phase 3+4. Phase 5 widens
    // enforcement to project repos.
    let empty_scope = CapabilityScope {
        expires_unix: i64::MAX,
        can_write: false,
        doc_ids: Vec::new(),
        ..Default::default()
    };
    let connection_scope: Option<CapabilityScope> = if state.enforce_capability {
        match params.get("cap") {
            Some(token) => match state.keypair.verify(token, now_unix()) {
                Ok(s) => {
                    // Phase 4: revoked tokens act as empty-scope.
                    if let Some(tid) = s.token_id {
                        if state.revocations.is_revoked(&tid) {
                            tracing::info!(token_id = %tid, "vox: token revoked");
                            Some(empty_scope.clone())
                        } else {
                            Some(s)
                        }
                    } else {
                        Some(s)
                    }
                }
                Err(e) => {
                    tracing::warn!(?e, "vox: capability rejected");
                    Some(empty_scope.clone())
                }
            },
            None => Some(empty_scope.clone()),
        }
    } else {
        None
    };

    ws.on_upgrade(move |socket| async move {
        let project_repo = (*state.project_repo).clone();
        let task_repo = (*state.task_repo).clone();
        let sync = match connection_scope {
            Some(scope) => WorkspaceSyncImpl::with_scope(state.registry.clone(), scope),
            None => state.sync.clone(),
        };
        let auth = state.auth.auth.clone();
        let share_service = state.share_service.clone();
        let attachment_service = state.attachments.clone();
        let anonymous_claim = state.anonymous_claim.clone();
        let vault_sync_state = state.vault_sync.clone();
        let vault_repo = (*state.vault_repo).clone();
        let folder_repo = (*state.folder_repo).clone();
        let page_repo = (*state.page_repo).clone();
        let block_repo = (*state.block_repo).clone();
        let knowledge_tag_repo = (*state.knowledge_tag_repo).clone();
        let base_repo = (*state.base_repo).clone();
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
                "ShareService" => {
                    connection.handle_with(ShareServiceDispatcher::new(share_service.clone()));
                    Ok(())
                }
                "AttachmentService" => {
                    use attachments_proto::AttachmentServiceDispatcher;
                    connection.handle_with(AttachmentServiceDispatcher::new(
                        (*attachment_service).clone(),
                    ));
                    Ok(())
                }
                "AnonymousClaim" => {
                    use project_proto::AnonymousClaimDispatcher;
                    // Wrap with both AuthServerMiddleware (loads
                    // token into request extensions) AND our own
                    // InstallSessionMiddleware (copies it into the
                    // service's session_token slot).
                    let middleware = InstallSessionMiddleware {
                        service: anonymous_claim.clone(),
                    };
                    connection.handle_with(
                        AnonymousClaimDispatcher::new(anonymous_claim.clone())
                            .with_middleware(AuthServerMiddleware)
                            .with_middleware(middleware),
                    );
                    Ok(())
                }
                "VaultRepo" => {
                    connection.handle_with(VaultRepoDispatcher::new(vault_repo.clone()));
                    Ok(())
                }
                "FolderRepo" => {
                    connection.handle_with(FolderRepoDispatcher::new(folder_repo.clone()));
                    Ok(())
                }
                "PageRepo" => {
                    connection.handle_with(PageRepoDispatcher::new(page_repo.clone()));
                    Ok(())
                }
                "BlockRepo" => {
                    connection.handle_with(BlockRepoDispatcher::new(block_repo.clone()));
                    Ok(())
                }
                "KnowledgeTagRepo" => {
                    connection
                        .handle_with(KnowledgeTagRepoDispatcher::new(knowledge_tag_repo.clone()));
                    Ok(())
                }
                "BaseRepo" => {
                    connection.handle_with(BaseRepoDispatcher::new(base_repo.clone()));
                    Ok(())
                }
                "VaultSync" => {
                    use vault_sync_proto::VaultSyncDispatcher;
                    connection.handle_with(VaultSyncDispatcher::new(vault_sync_state.clone()));
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
