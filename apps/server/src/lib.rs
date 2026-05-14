//! Task server — vox RPC entry point.
//!
//! Vertical-slice scope: axum router with `/health` + `/vox`. Every
//! per-entity Repo defined by `project-proto` (Project / Task /
//! Cycle / Milestone) is mounted as a vox dispatcher backed by the
//! shared workspace `CrdtDoc`. The legacy `/sync/{doc_id}` byte
//! relay is gone — Loro updates flow through the auto-generated
//! `*Repo` services instead.

use std::sync::Arc;

use architect::vox;
use axum::Router;
use axum::extract::State;
use axum::extract::ws::WebSocketUpgrade;
use axum::response::IntoResponse;
use axum::routing::get;
use crdt::loro::{self, ExportMode};
use crdt::{CrdtDoc, Persistence};
use project_crdt::{CycleRepoLoro, MilestoneRepoLoro, ProjectRepoLoro, TaskRepoLoro};
use project_proto::{
    CycleRepoDispatcher, MilestoneRepoDispatcher, ProjectRepoDispatcher, SyncError,
    TaskRepoDispatcher, UpdateBytes, WorkspaceSync, WorkspaceSyncDispatcher,
};
use tokio::sync::broadcast;
use uuid::Uuid;

/// Shared workspace doc id — every entity in the vertical slice
/// lives in this single Loro doc so the four `*RepoLoro` instances
/// see each other's writes. Same constant `task_db::WORKSPACE_DOC_ID`
/// uses for seeding.
pub const WORKSPACE_DOC_ID: Uuid = task_db::WORKSPACE_DOC_ID;

#[derive(Clone)]
pub struct AppState {
    #[allow(dead_code)]
    persistence: Arc<dyn Persistence>,
    pub workspace_doc: Arc<CrdtDoc>,
    pub project_repo: Arc<ProjectRepoLoro>,
    pub task_repo: Arc<TaskRepoLoro>,
    pub cycle_repo: Arc<CycleRepoLoro>,
    pub milestone_repo: Arc<MilestoneRepoLoro>,
    pub sync: WorkspaceSyncImpl,
}

impl AppState {
    pub async fn new<P: Persistence + 'static>(persistence: P) -> eyre::Result<Self> {
        let persistence: Arc<dyn Persistence> = Arc::new(persistence);
        let workspace_doc = CrdtDoc::open(WORKSPACE_DOC_ID, ErasedPersistence(persistence.clone()))
            .await
            .map_err(|e| eyre::eyre!("open workspace CrdtDoc: {e}"))?;
        let workspace_doc = Arc::new(workspace_doc);
        let project_repo = Arc::new(ProjectRepoLoro::new(&workspace_doc));
        let task_repo = Arc::new(TaskRepoLoro::new(&workspace_doc));
        let cycle_repo = Arc::new(CycleRepoLoro::new(&workspace_doc));
        let milestone_repo = Arc::new(MilestoneRepoLoro::new(&workspace_doc));
        let sync = WorkspaceSyncImpl::new(workspace_doc.clone());
        Ok(Self {
            persistence,
            workspace_doc,
            project_repo,
            task_repo,
            cycle_repo,
            milestone_repo,
            sync,
        })
    }
}

/// Server-side `WorkspaceSync` implementation. Owns a broadcast
/// channel of raw Loro update bytes; every committed local change
/// on `workspace_doc` (via `subscribe_local_update`) is fanned out
/// to every active `subscribe()` caller.
///
/// Channel capacity is 256 — a slow subscriber that lags out gets
/// `RecvError::Lagged` and we recover by sending a fresh full
/// snapshot. Snapshot is much bigger than a delta but
/// idempotent-merge-safe; importing twice is a no-op.
#[derive(Clone)]
pub struct WorkspaceSyncImpl {
    doc: Arc<CrdtDoc>,
    update_tx: broadcast::Sender<Vec<u8>>,
    // The Subscription handle from `subscribe_local_update` must
    // stay alive for the callback to fire. Wrapped in Arc so
    // `WorkspaceSyncImpl` stays `Clone`.
    _subscription: Arc<loro::Subscription>,
}

impl WorkspaceSyncImpl {
    pub fn new(doc: Arc<CrdtDoc>) -> Self {
        let (update_tx, _) = broadcast::channel::<Vec<u8>>(256);
        let tx_for_cb = update_tx.clone();
        let subscription = doc.loro().subscribe_local_update(Box::new(move |bytes| {
            // `send` errors when there are no active subscribers —
            // that's fine; we'll still get fresh receivers later
            // and they'll catch up via the snapshot path.
            let _ = tx_for_cb.send(bytes.to_vec());
            true
        }));
        Self {
            doc,
            update_tx,
            _subscription: Arc::new(subscription),
        }
    }
}

impl WorkspaceSync for WorkspaceSyncImpl {
    async fn apply_update(&self, update: UpdateBytes) -> Result<(), SyncError> {
        self.doc
            .apply_remote(&update.0)
            .map_err(|e| SyncError::InvalidUpdate(e.to_string()))
    }

    async fn subscribe(&self, output: vox::Tx<UpdateBytes>) {
        // Catch the new subscriber up with one full snapshot, then
        // bridge live updates from the broadcast channel until the
        // client drops its receiver (`output.send` returns Err).
        let snapshot = match self.doc.loro().export(ExportMode::Snapshot) {
            Ok(b) => b,
            Err(e) => {
                tracing::warn!(?e, "snapshot export failed");
                let _ = output.close(Default::default()).await;
                return;
            }
        };
        if output.send(UpdateBytes(snapshot)).await.is_err() {
            return;
        }

        let mut rx = self.update_tx.subscribe();
        loop {
            match rx.recv().await {
                Ok(bytes) => {
                    if output.send(UpdateBytes(bytes)).await.is_err() {
                        return;
                    }
                }
                Err(broadcast::error::RecvError::Closed) => return,
                Err(broadcast::error::RecvError::Lagged(n)) => {
                    tracing::warn!(skipped = n, "subscriber lagged; resending snapshot");
                    let snap = self
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
        let cycle_repo = (*state.cycle_repo).clone();
        let milestone_repo = (*state.milestone_repo).clone();
        let sync = state.sync.clone();
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
                "CycleRepo" => {
                    connection.handle_with(CycleRepoDispatcher::new(cycle_repo.clone()));
                    Ok(())
                }
                "MilestoneRepo" => {
                    connection.handle_with(MilestoneRepoDispatcher::new(milestone_repo.clone()));
                    Ok(())
                }
                "WorkspaceSync" => {
                    connection.handle_with(WorkspaceSyncDispatcher::new(sync.clone()));
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
