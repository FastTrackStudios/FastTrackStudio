//! Reference axum + vox server.
//!
//! Architecture in one paragraph: `apps/server` owns the I/O surface
//! (HTTP listener + vox WebSocket endpoint) and the concrete
//! [`ExampleService`] implementation. The service composes the
//! crudcrate-generated [`ExampleRepo`] (CRUD over SeaORM) — anything
//! that *is* business logic lives here; anything that's pure CRUD
//! sits in the repo. Clients (wasm web, native desktop, future iOS)
//! call `ExampleService` over the same `/vox` connection.

mod service_impl;

use axum::{Router, routing::get};
use sea_orm::{Database, DatabaseConnection};
use std::sync::Arc;
use tower_http::cors::CorsLayer;
use tracing::info;

#[derive(Clone)]
struct AppState {
    db: DatabaseConnection,
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info,example_app_server=debug".into()),
        )
        .init();

    let database_url = std::env::var("DATABASE_URL")
        .unwrap_or_else(|_| "sqlite://./example.db?mode=rwc".into());
    let bind_addr = std::env::var("BIND_ADDR").unwrap_or_else(|_| "0.0.0.0:4040".into());

    info!(%database_url, %bind_addr, "starting example server");

    let db = Database::connect(&database_url).await?;
    // Run pending migrations on boot so a fresh `cargo run` doesn't
    // require a separate migrate step.
    use sea_orm_migration::MigratorTrait;
    example_db::Migrator::up(&db, None).await?;

    let state = Arc::new(AppState { db });

    let app = Router::new()
        .route("/api/health", get(health))
        // Vox WebSocket endpoint is mounted below once the service
        // dispatcher wiring lands. Left as a stub today.
        .layer(CorsLayer::permissive())
        .with_state(state);

    let listener = tokio::net::TcpListener::bind(&bind_addr).await?;
    info!("HTTP listening on http://{bind_addr}");
    info!("  Health:  http://{bind_addr}/api/health");
    info!("  Vox WS:  ws://{bind_addr}/vox  (TODO: mount)");
    axum::serve(listener, app).await?;
    Ok(())
}

async fn health() -> &'static str {
    "ok"
}
