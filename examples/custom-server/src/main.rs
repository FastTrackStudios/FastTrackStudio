//! Custom server — assembled by hand from independent pieces.
//!
//! Demonstrates the full DI shape architect supports:
//!
//! 1. Pick a **repo** impl. Anything that satisfies `ExampleRepo` works
//!    — the architect-emitted SeaORM `ExampleRepoStorage`, the in-tree
//!    `ExampleRepoMemory`, or (as here) a third-party-shaped
//!    `StubBackend` from `examples/external-stub/`.
//!
//! 2. Pick (or write) a **service** impl. Architect doesn't emit
//!    one — service traits are hand-written `#[vox::service]` traits.
//!    The impl can take any `ExampleRepo` by trait bound, so it
//!    composes with whatever repo you chose in step 1.
//!
//! 3. Mount both dispatchers on the same `/vox` route. Clients call
//!    either surface — both wire the same `Example` types.
//!
//! What's NOT here:
//!
//! - No dependency on the `example` facade. Facades are for first-party
//!   builds; a third party assembling their own binary skips them.
//! - No `architect/server` feature on `example-proto`. Without that
//!   feature, the architect derive doesn't emit `ExampleRepoStorage`
//!   at all — we get just the trait + payload types, plus the
//!   auto-generated `ExampleRepoDispatcher` for mounting.
//!
//! Run with:
//!   cargo run -p example-custom-server

use std::sync::Arc;

use architect::axum_ws;
use axum::{
    Router,
    extract::{State, WebSocketUpgrade},
    response::{IntoResponse, Response},
    routing::get,
};
use example_proto::{
    Example, ExampleRepo, ExampleRepoDispatcher, ExampleService, ExampleServiceDispatcher,
    ExampleServiceError,
};
use example_stub_backend::StubBackend;
use tower_http::cors::CorsLayer;
use tracing::{info, warn};
use uuid::Uuid;

// ── Service impl ──────────────────────────────────────────────────────
//
// Generic over the repo trait, exactly like apps/app/server's
// ExampleServiceImpl. The only thing tying this to StubBackend is
// the type chosen at assembly time below.

#[derive(Clone)]
struct CustomExampleService<R: ExampleRepo + Clone + Send + Sync + 'static> {
    repo: R,
}

impl<R: ExampleRepo + Clone + Send + Sync + 'static> CustomExampleService<R> {
    fn new(repo: R) -> Self {
        Self { repo }
    }
}

impl<R: ExampleRepo + Clone + Send + Sync + 'static> ExampleService for CustomExampleService<R> {
    async fn search(
        &self,
        query: String,
        _limit: u32,
    ) -> Result<Vec<Example>, ExampleServiceError> {
        if query.trim().is_empty() {
            return Err(ExampleServiceError::InvalidInput("query is empty".into()));
        }
        let _ = &self.repo;
        Err(ExampleServiceError::Internal(
            "search: implement against your repo of choice".into(),
        ))
    }

    async fn duplicate(
        &self,
        _id: Uuid,
        _new_name: Option<String>,
    ) -> Result<Example, ExampleServiceError> {
        Err(ExampleServiceError::Internal(
            "duplicate: implement against your repo of choice".into(),
        ))
    }
}

// ── Assembly ──────────────────────────────────────────────────────────

#[derive(Clone)]
struct AppState {
    repo: StubBackend,
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info,example_custom_server=debug".into()),
        )
        .init();

    let bind_addr = std::env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:4041".into());

    // Step 1: pick a repo. Swap this line for any other ExampleRepo
    // impl — ExampleRepoStorage<DatabaseConnection>, your own
    // sqlx-backed type, an HTTP-proxy adapter, anything.
    let repo = StubBackend::with_seed_data();

    info!(%bind_addr, "starting custom-server with StubBackend (seeded)");

    let state = AppState { repo };

    let app = Router::new()
        .route("/api/health", get(health))
        .route("/vox", get(vox_ws_handler))
        .layer(CorsLayer::permissive())
        .with_state(Arc::new(state));

    let listener = tokio::net::TcpListener::bind(&bind_addr).await?;
    info!("HTTP listening on http://{bind_addr}");
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
                // Step 2a: mount the repo dispatcher directly. Any
                // type implementing ExampleRepo works.
                connection.handle_with(ExampleRepoDispatcher::new(repo.clone()));
                Ok(())
            }
            "ExampleService" => {
                // Step 2b: build the service from the same repo and
                // mount it. The service is generic over R, so the
                // concrete repo type just flows through.
                connection.handle_with(ExampleServiceDispatcher::new(CustomExampleService::new(
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
