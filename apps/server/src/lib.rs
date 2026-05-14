//! Task server — vox RPC entry point.
//!
//! Vertical-slice scope: axum router with `/health` + `/vox`. Every
//! per-entity Repo defined by `project-proto` (Project / Task /
//! Cycle / Milestone) is mounted as a vox dispatcher backed by the
//! shared workspace `CrdtDoc`. The legacy `/sync/{doc_id}` byte
//! relay is gone — Loro updates flow through the auto-generated
//! `*Repo` services instead.

use std::sync::Arc;

use axum::Router;
use axum::extract::State;
use axum::extract::ws::WebSocketUpgrade;
use axum::response::IntoResponse;
use axum::routing::get;
use crdt::{CrdtDoc, Persistence};
use project_crdt::{CycleRepoLoro, MilestoneRepoLoro, ProjectRepoLoro, TaskRepoLoro};
use project_proto::{
    CycleRepoDispatcher, MilestoneRepoDispatcher, ProjectRepoDispatcher, TaskRepoDispatcher,
};
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
        Ok(Self {
            persistence,
            workspace_doc,
            project_repo,
            task_repo,
            cycle_repo,
            milestone_repo,
        })
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
