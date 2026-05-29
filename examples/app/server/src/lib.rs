//! Reusable server wiring for the example app.
//!
//! The HTTP + vox surface is generic over the `ExampleRepo` backend, so
//! the same router serves the sqlite, in-memory, or any future backend —
//! and the e2e tests mount it with the in-memory repo instead of
//! re-deriving the wiring. The bin (`main.rs`) only owns backend
//! selection + migrations; everything else lives here.
//!
//! The vox surface is composed through architect's `Layer` system: the
//! backend's own bundle (`ExampleRepo`, via the derive-emitted
//! `ExampleRepoLayer` token) is merged with the hand-written
//! `ExampleService`, producing one `LayerRouter` that dispatches every
//! method by id. Swapping the repo backend changes nothing here — any
//! `ExampleRepo + Services` value drops in.

pub mod service_impl;

use std::sync::Arc;

use axum::{
    Router,
    extract::{State, WebSocketUpgrade},
    response::{IntoResponse, Response},
    routing::get,
};
use example::architect::Services;
use example::axum_ws;
use example::{ExampleRepo, ExampleServiceDispatcher};
use service_impl::ExampleServiceImpl;
use tower_http::cors::CorsLayer;

/// Build the app's axum router for any `ExampleRepo` backend that also
/// declares its `Services` bundle: a health endpoint plus the `/vox`
/// WebSocket serving the composed `LayerRouter` (repo CRUD +
/// search/duplicate).
pub fn vox_router<R>(repo: R) -> Router
where
    R: ExampleRepo + Services + Clone + Send + Sync + 'static,
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
    R: ExampleRepo + Services + Clone + Send + Sync + 'static,
{
    ws.on_upgrade(move |socket| async move {
        let repo = (*repo).clone();
        // Compose the whole vox surface once via the Layer system: the
        // repo's canonical bundle, plus `ExampleService` (which is a
        // separate hand-written trait backed by the same repo) mounted
        // alongside. The resulting `LayerRouter` routes every service's
        // methods by id, so each connection gets the same handler.
        let router = repo.clone().into_router().with(
            example::example_service_service_descriptor(),
            ExampleServiceDispatcher::new(ExampleServiceImpl::new(repo)),
        );
        let factory = axum_ws::acceptor_fn(move |_req, connection| {
            connection.handle_with(router.clone());
            Ok(())
        });
        axum_ws::serve(socket, factory).await;
    })
    .into_response()
}
