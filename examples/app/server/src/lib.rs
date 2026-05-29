//! Reusable server wiring for the example app.
//!
//! The HTTP + vox surface is generic over the `ExampleRepo` backend, so
//! the same router serves the sqlite, in-memory, or any future backend —
//! and the e2e tests mount it with the in-memory repo instead of
//! re-deriving the wiring. The bin (`main.rs`) only owns backend
//! selection + migrations; everything else lives here.

pub mod service_impl;

use std::sync::Arc;

use axum::{
    Router,
    extract::{State, WebSocketUpgrade},
    response::{IntoResponse, Response},
    routing::get,
};
use example::axum_ws;
use example::{ExampleRepo, ExampleRepoDispatcher, ExampleServiceDispatcher};
use service_impl::ExampleServiceImpl;
use tower_http::cors::CorsLayer;
use tracing::warn;

/// Build the app's axum router for any `ExampleRepo` backend: a health
/// endpoint plus the `/vox` WebSocket that multiplexes the `ExampleRepo`
/// (CRUD) and `ExampleService` (search/duplicate) dispatchers by service
/// name — exactly what every vox client establishes against.
pub fn vox_router<R>(repo: R) -> Router
where
    R: ExampleRepo + Clone + Send + Sync + 'static,
{
    Router::new()
        .route("/api/health", get(health))
        .route("/vox", get(vox_ws_handler::<R>))
        .layer(CorsLayer::permissive())
        .with_state(Arc::new(repo))
}

async fn health() -> &'static str {
    "ok"
}

async fn vox_ws_handler<R>(ws: WebSocketUpgrade, State(repo): State<Arc<R>>) -> Response
where
    R: ExampleRepo + Clone + Send + Sync + 'static,
{
    ws.on_upgrade(move |socket| async move {
        let repo = (*repo).clone();
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
