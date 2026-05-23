//! Reference axum + vox server.
//!
//! Backend is chosen at compile time via cargo features on the `example`
//! facade (`backend-db` or `backend-memory`). The dispatcher wiring is
//! identical either way — the contract from `example_proto` is what
//! holds it all together.

mod service_impl;

use axum::{
    Router,
    extract::{State, WebSocketUpgrade},
    response::{IntoResponse, Response},
    routing::get,
};
use example::axum_ws;
use example::{ExampleRepoDispatcher, ExampleServiceDispatcher};
use service_impl::ExampleServiceImpl;
use std::sync::Arc;
use tower_http::cors::CorsLayer;
use tracing::{info, warn};

// ── Backend-specific state ────────────────────────────────────────────

#[cfg(feature = "backend-db")]
mod state {
    use example::backend_db::{ExampleRepoStorage, Migrator};
    use sea_orm::{Database, DatabaseConnection};
    use sea_orm_migration::MigratorTrait;

    #[derive(Clone)]
    pub struct AppState {
        pub repo: ExampleRepoStorage<DatabaseConnection>,
    }

    pub async fn init() -> eyre::Result<(AppState, String)> {
        let database_url = std::env::var("DATABASE_URL")
            .unwrap_or_else(|_| "sqlite://./example.db?mode=rwc".into());
        let db = Database::connect(&database_url).await?;
        Migrator::up(&db, None).await?;
        Ok((
            AppState {
                repo: ExampleRepoStorage::new(db),
            },
            format!("sqlite ({database_url})"),
        ))
    }
}

// Picks backend-memory only when backend-db isn't also enabled — keeps
// `cargo build --all-features` resolvable. In practice each binary
// enables exactly one backend feature.
#[cfg(all(not(feature = "backend-db"), feature = "backend-memory"))]
mod state {
    use example::backend_memory::ExampleRepoMemory;

    #[derive(Clone)]
    pub struct AppState {
        pub repo: ExampleRepoMemory,
    }

    pub async fn init() -> eyre::Result<(AppState, String)> {
        Ok((
            AppState {
                repo: ExampleRepoMemory::new(),
            },
            "in-memory".into(),
        ))
    }
}

use state::AppState;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info,app_server=debug".into()),
        )
        .init();

    let bind_addr = std::env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:4040".into());
    let (state, backend_label) = state::init().await?;
    info!(backend = %backend_label, %bind_addr, "starting example server");

    let app = Router::new()
        .route("/api/health", get(health))
        .route("/vox", get(vox_ws_handler))
        .layer(CorsLayer::permissive())
        .with_state(Arc::new(state));

    let listener = tokio::net::TcpListener::bind(&bind_addr).await?;
    info!("HTTP listening on http://{bind_addr}");
    info!("  Health:  http://{bind_addr}/api/health");
    info!("  Vox WS:  ws://{bind_addr}/vox");
    axum::serve(listener, app).await?;
    Ok(())
}

async fn health() -> &'static str {
    "ok"
}

async fn vox_ws_handler(ws: WebSocketUpgrade, State(state): State<Arc<AppState>>) -> Response {
    ws.on_upgrade(move |socket| async move {
        let repo = state.repo.clone();
        let factory = axum_ws::acceptor_fn(move |req, connection| match req.service() {
            "ExampleRepo" => {
                connection.handle_with(ExampleRepoDispatcher::new(repo.clone()));
                Ok(())
            }
            "ExampleService" => {
                connection.handle_with(ExampleServiceDispatcher::new(ExampleServiceImpl::new(
                    repo.clone(),
                )));
                Ok(())
            }
            other => {
                warn!(service = other, "unknown vox service requested");
                Err(vec![])
            }
        });
        axum_ws::serve(socket, factory).await;
    })
    .into_response()
}
