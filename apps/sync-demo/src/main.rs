//! Thin shell — env-driven config, then hands off to `sync_demo::router`.

use std::net::SocketAddr;

use crdt_seaorm::SeaOrmPersistence;
use eyre::WrapErr;
use sea_orm::Database;
use sea_orm_migration::MigratorTrait;
use sync_demo::{AppState, router};
use tracing::info;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "sync_demo=info,tower_http=info".into()),
        )
        .init();

    let database_url = std::env::var("SYNC_DEMO_DATABASE_URL")
        .unwrap_or_else(|_| "sqlite://./sync-demo.db?mode=rwc".into());
    let bind: SocketAddr = std::env::var("SYNC_DEMO_BIND")
        .unwrap_or_else(|_| "0.0.0.0:9090".into())
        .parse()
        .wrap_err("invalid SYNC_DEMO_BIND")?;

    info!(%database_url, "connecting");
    let db = Database::connect(&database_url).await?;
    crdt_seaorm::Migrator::up(&db, None).await?;
    let persistence = SeaOrmPersistence::new(db);

    let state = AppState::new(persistence);
    let app = router(state);

    info!(%bind, "listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app).await?;
    Ok(())
}
