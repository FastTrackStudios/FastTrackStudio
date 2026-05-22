//! `task-server` — minimal vox endpoint.
//!
//! Surface after the knowledge + project-CRDT rip:
//! - `/health` — liveness probe.
//! - `/vox` — architect/vox WebSocket endpoint hosting three
//!   services: `AuthService` (architect-auth),
//!   `AttachmentService` (signed upload/download), and
//!   `VaultSyncRpc` (file replication backed by
//!   `vault::Backend`).
//! - `/blobs/*` — signed-URL endpoint for attachment uploads
//!   and downloads, mounted via `attachments::routes`.
//!
//! The previous CRDT machinery (`DocRegistry`, `OpenDoc`,
//! `WorkspaceSyncImpl`, `task-db` / `crdt-seaorm` persistence,
//! `*RepoLoro` dispatchers, capability / share-link / claim
//! services) was ripped along with the `project-proto` /
//! `project-crdt` crates. CRDT now lives only at the per-file
//! editor layer (future); vault is the sole storage path.

pub mod attachments;
pub mod capability;

use std::path::PathBuf;
use std::sync::Arc;

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
use sea_orm::Database;
use sea_orm_migration::MigratorTrait;

use crate::capability::{ServerKeypair, default_keypair_path};

#[derive(Clone)]
pub struct AuthState {
    pub auth: ArchitectAuth<AuthSeaOrmStorage>,
}

impl AuthState {
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
    pub auth: AuthState,
    /// Ed25519 keypair used to sign blob URLs. Loaded from
    /// `$XDG_DATA_HOME/task-server/server-key.ed25519`,
    /// generated on first boot. Tests use
    /// `ServerKeypair::generate_ephemeral()`.
    pub keypair: ServerKeypair,
    pub attachments: Arc<attachments::AttachmentServiceImpl>,
    /// File-replication backend. `vault::Backend` mounted on the
    /// `VaultSyncRpc` arm. `under_parent` layout: every `vault_id`
    /// resolves to a subdir of the configured parent (created on
    /// demand on first write).
    pub vault_sync: vault::Backend,
    /// Wiki feature backend. `under_parent` layout — each
    /// `wiki_id` maps to `<vault_root>/{wiki_id}/`. Mounted
    /// on `/vox` as the 13 per-capability traits.
    pub wiki: wiki_live::WikiBackend,
    /// Agent-task queue store. One SQLite DB shared across
    /// all queues — partitioned by `queue_id` column. Mounted
    /// on `/vox` as `AgentTaskQueue` (the slim domain trait;
    /// CRUD is handled by the architect-emitted per-entity
    /// Repo traits, which we don't currently mount).
    pub agent_tasks: agent_tasks::Store,
    /// Vault root used by agent-dispatch when folding agent
    /// task completions back into source task notes. Shared
    /// with `vault_sync` so the same on-disk layout is used.
    pub agent_dispatch_vault_root: PathBuf,
    /// Timer store — billable time tracking. SQLite at
    /// `$XDG_DATA_HOME/task-server/timer.sqlite` (override
    /// via `TASK_SERVER_TIMER_URL`). Project markdown
    /// defaults are read from the same vault root as
    /// `vault_sync`, so the rate cascade resolves on disk.
    pub timer: timer::Store,
}

impl AppState {
    /// Build app state with auth opened at the default XDG path
    /// (`$XDG_DATA_HOME/task-server/auth.sqlite`, fallback
    /// `~/.local/share/task-server/auth.sqlite`).
    pub async fn new() -> eyre::Result<Self> {
        let auth_db_url = format!("sqlite://{}?mode=rwc", default_auth_db_path()?.display());
        let auth = AuthState::open(&auth_db_url, DEFAULT_AUTH_SECRET).await?;
        let keypair = ServerKeypair::load_or_generate(&default_keypair_path()?)
            .map_err(|e| eyre::eyre!("load server keypair: {e}"))?;
        Self::new_with_auth(auth, keypair).await
    }

    /// Build app state with explicit auth + keypair — tests pass
    /// in-memory `AuthState` + `ServerKeypair::generate_ephemeral()`
    /// so they don't touch the user's XDG dir.
    pub async fn new_with_auth(auth: AuthState, keypair: ServerKeypair) -> eyre::Result<Self> {
        // Attachments — local blob store under the standard XDG
        // path; the keypair signs upload/download URLs.
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

        // Vault file-replication. Storage root defaults to
        // `$XDG_DATA_HOME/task-server/vaults`, overridable via
        // `TASK_SERVER_VAULT_ROOT` for tests / containers.
        let vault_root = std::env::var("TASK_SERVER_VAULT_ROOT").map_or_else(
            |_| {
                dirs_local_share()
                    .unwrap_or_else(|| PathBuf::from("./vaults"))
                    .join("task-server")
                    .join("vaults")
            },
            PathBuf::from,
        );
        let vault_sync_state = vault::Backend::under_parent(vault_root.clone())
            .map_err(|e| eyre::eyre!("vault backend: {e}"))?;
        let wiki = wiki_live::WikiBackend::under_parent(vault_root.clone())
            .map_err(|e| eyre::eyre!("wiki backend: {e}"))?;

        // Agent-task queue store. SQLite at
        // `$XDG_DATA_HOME/task-server/agent-tasks.sqlite`
        // (override via `TASK_SERVER_AGENT_TASKS_URL`); shared
        // across queues, partitioned by `queue_id`.
        let agent_tasks_url = std::env::var("TASK_SERVER_AGENT_TASKS_URL").unwrap_or_else(|_| {
            format!(
                "sqlite://{}?mode=rwc",
                default_agent_tasks_db_path()
                    .map_or_else(|_| ":memory:".into(), |p| p.display().to_string())
            )
        });
        let agent_tasks_conn = Database::connect(&agent_tasks_url)
            .await
            .map_err(|e| eyre::eyre!("connect agent-tasks db `{agent_tasks_url}`: {e}"))?;
        agent_tasks::Migrator::up(&agent_tasks_conn, None)
            .await
            .map_err(|e| eyre::eyre!("agent-tasks migrations: {e}"))?;
        let agent_tasks = agent_tasks::Store::new(agent_tasks_conn);

        // Timer store. SQLite at
        // `$XDG_DATA_HOME/task-server/timer.sqlite`
        // (override via `TASK_SERVER_TIMER_URL`). Project
        // defaults are resolved off the same vault root the
        // rest of the server uses — the rate cascade calls
        // `VaultProjectDefaults::lookup` to read each
        // session's project markdown on close.
        let timer_url = std::env::var("TASK_SERVER_TIMER_URL").unwrap_or_else(|_| {
            format!(
                "sqlite://{}?mode=rwc",
                default_timer_db_path()
                    .map_or_else(|_| ":memory:".into(), |p| p.display().to_string())
            )
        });
        let timer_conn = Database::connect(&timer_url)
            .await
            .map_err(|e| eyre::eyre!("connect timer db `{timer_url}`: {e}"))?;
        timer::Migrator::up(&timer_conn, None)
            .await
            .map_err(|e| eyre::eyre!("timer migrations: {e}"))?;
        let timer_defaults = std::sync::Arc::new(timer::store::VaultProjectDefaults {
            vault_root: vault_root.clone(),
        });
        let timer = timer::Store::new(timer_conn, timer_defaults);

        // Auto-retry any wiki ingest tasks the previous
        // backend left stuck mid-flight. Best-effort —
        // failures here shouldn't block startup.
        if let Ok(entries) = std::fs::read_dir(&vault_root) {
            for entry in entries.flatten() {
                if !entry.path().is_dir() {
                    continue;
                }
                let wiki_handle = wiki_live::WikiLive::open(entry.path());
                if !wiki_handle.is_bootstrapped() {
                    continue;
                }
                if let Ok((retried, failed)) = wiki_handle.auto_retry_stuck_tasks(3) {
                    if !retried.is_empty() || !failed.is_empty() {
                        tracing::info!(
                            vault = %entry.path().display(),
                            retried = retried.len(),
                            failed = failed.len(),
                            "wiki auto-retry: revived stuck tasks"
                        );
                    }
                }
            }
        }

        Ok(Self {
            auth,
            keypair,
            attachments: attachment_service,
            vault_sync: vault_sync_state,
            wiki,
            agent_tasks,
            agent_dispatch_vault_root: vault_root,
            timer,
        })
    }
}

/// Resolve `$XDG_DATA_HOME/task-server/timer.sqlite`. Mirror
/// of [`default_agent_tasks_db_path`]; kept separate so the
/// DBs can be swapped/backed up independently.
pub fn default_timer_db_path() -> eyre::Result<PathBuf> {
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
    Ok(dir.join("timer.sqlite"))
}

/// Resolve `$XDG_DATA_HOME/task-server/agent-tasks.sqlite`.
/// Mirror of [`default_auth_db_path`]; kept separate so the
/// two DBs can be swapped/backed up independently.
pub fn default_agent_tasks_db_path() -> eyre::Result<PathBuf> {
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
    Ok(dir.join("agent-tasks.sqlite"))
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

/// Best-effort `$XDG_DATA_HOME` (falls back to `$HOME/.local/share`).
/// Skips a `dirs` crate dep — we only use this one path.
fn dirs_local_share() -> Option<PathBuf> {
    if let Some(xdg) = std::env::var_os("XDG_DATA_HOME") {
        return Some(PathBuf::from(xdg));
    }
    let home = std::env::var_os("HOME")?;
    Some(PathBuf::from(home).join(".local/share"))
}

pub fn router(state: AppState) -> Router {
    use attachments::routes::AttachmentRouteState;

    // Mount the /blobs/* HTTP routes under their own sub-router
    // so they pass a small `AttachmentRouteState` sliver and
    // don't drag the full `AppState` into the attachment
    // handlers' generic bound.
    let blob_state = AttachmentRouteState {
        service: state.attachments.clone(),
    };
    let blob_router = attachments::attachment_router().with_state(blob_state);

    Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/vox", get(vox_ws_handler))
        .merge(blob_router)
        .layer(tower_http::cors::CorsLayer::permissive())
        .with_state(state)
}

async fn vox_ws_handler(
    State(state): State<AppState>,
    ws: WebSocketUpgrade,
) -> axum::response::Response {
    ws.on_upgrade(move |socket| async move {
        let auth = state.auth.auth.clone();
        let attachment_service = state.attachments.clone();
        let vault_sync_state = state.vault_sync.clone();
        let wiki = state.wiki.clone();
        let agent_tasks_store = state.agent_tasks.clone();
        let timer_store = state.timer.clone();
        let acceptor =
            architect::axum_ws::acceptor_fn(move |req, connection| match req.service() {
                "AuthService" => {
                    connection.handle_with(
                        AuthServiceDispatcher::new(AuthVoxService::new(auth.clone()))
                            .with_middleware(AuthServerMiddleware),
                    );
                    Ok(())
                }
                "AttachmentService" => {
                    use attachments_proto::AttachmentServiceDispatcher;
                    connection.handle_with(AttachmentServiceDispatcher::new(
                        (*attachment_service).clone(),
                    ));
                    Ok(())
                }
                // architect-emitted mount: `serve` wraps the
                // backend in `VaultSyncRpcDispatcher` and pulls
                // its `TokioBlockingDispatcher` via
                // `HasDispatcher`. Wire-level service name from
                // `vault_proto::descriptor()`.
                name if name == vault_proto::descriptor().service_name => {
                    connection.handle_with(vault_proto::serve(vault_sync_state.clone()));
                    Ok(())
                }
                // Agent-task queue — slim domain trait (claim,
                // complete, set-status). Plain CRUD is the
                // architect-emitted per-entity Repo traits, not
                // mounted here yet.
                name if name
                    == agent_proto::service::tasks::agent_task_queue_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(agent_proto::service::tasks::serve(
                        agent_tasks_store.clone(),
                    ));
                    Ok(())
                }
                // Timer — billable time tracking. The slim
                // TimerService trait (start/stop/active/
                // switch/log/resolve_rate). Plain CRUD on
                // Client/Tag/Rate/WorkSession entities goes
                // through their architect-emitted Repo
                // traits, not mounted here yet.
                name if name
                    == timer_proto::service::timer_service_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(timer_proto::service::serve(timer_store.clone()));
                    Ok(())
                }
                // Wiki feature — 13 per-capability traits, one
                // descriptor each. `wiki_proto::service::*`.
                name if name
                    == wiki_proto::service::schema::schema_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::schema::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::catalog::catalog_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::catalog::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::raw_layer::raw_layer_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::raw_layer::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::graph::graph_rpc_service_descriptor().service_name =>
                {
                    connection.handle_with(wiki_proto::service::graph::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::ingest::ingest_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::ingest::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::lint::lint_rpc_service_descriptor().service_name =>
                {
                    connection.handle_with(wiki_proto::service::lint::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::search::search_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::search::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::watcher::watcher_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::watcher::serve(wiki.clone()));
                    Ok(())
                }
                name if name
                    == wiki_proto::service::multimodal::multimodal_rpc_service_descriptor()
                        .service_name =>
                {
                    connection.handle_with(wiki_proto::service::multimodal::serve(wiki.clone()));
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
