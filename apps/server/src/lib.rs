//! Task server — vox RPC entry point.
//!
//! Vertical-slice scope: this crate owns the axum router, a healthz
//! probe and the `/vox` WebSocket endpoint. Every legacy
//! `/sync/{doc_id}` byte-relay was deleted as part of the reset —
//! vox is the sole transport. Per-feature service dispatchers land
//! in Phase D; until then `vox_ws_handler` rejects every requested
//! service so failures surface as a clean error rather than a
//! fall-through hang.

use std::sync::Arc;

use axum::Router;
use axum::extract::State;
use axum::extract::ws::WebSocketUpgrade;
use axum::response::IntoResponse;
use axum::routing::get;
use crdt::Persistence;
use sea_orm::DatabaseConnection;

#[derive(Clone)]
pub struct AppState {
    #[allow(dead_code)]
    persistence: Arc<dyn Persistence>,
    pub vox: Option<VoxState>,
}

/// Shared per-process state used by every vox session. Currently
/// just the database handle — per-service dispatchers (Task, Inbox,
/// Project) get added back as the new feature trios re-introduce
/// their service surfaces.
#[derive(Clone)]
pub struct VoxState {
    pub db: DatabaseConnection,
}

impl AppState {
    pub async fn new<P: Persistence>(persistence: P) -> eyre::Result<Self> {
        let persistence: Arc<dyn Persistence> = Arc::new(persistence);
        Ok(Self {
            persistence,
            vox: None,
        })
    }

    pub fn wire_vox(&mut self, db: DatabaseConnection) {
        self.vox = Some(VoxState { db });
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
        let _vox = state.vox.clone();
        let acceptor = architect::axum_ws::acceptor_fn(move |req, _connection| {
            tracing::info!(
                service = %req.service(),
                "vox session: dispatcher not yet wired (vertical-slice reset, Phase D pending)"
            );
            Err(Vec::new())
        });
        architect::axum_ws::serve(socket, acceptor).await;
    })
    .into_response()
}
