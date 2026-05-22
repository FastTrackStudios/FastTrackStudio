//! Thin shell — env-driven config, then hands off to `task_server::router`.

use std::net::SocketAddr;

use eyre::WrapErr;
use task_db::{WORKSPACE_DOC_ID, default_database_url, open_and_migrate, seed};
use task_server::{AppState, router};
use tracing::info;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "task_server=info,task_db=info,tower_http=info".into()),
        )
        .init();

    let database_url = default_database_url();
    let bind: SocketAddr = std::env::var("TASK_SERVER_BIND")
        .unwrap_or_else(|_| "0.0.0.0:9090".into())
        .parse()
        .wrap_err("invalid TASK_SERVER_BIND")?;
    let seed_on_start = env_truthy("TASK_SERVER_SEED");

    info!(%database_url, "connecting");
    let persistence = open_and_migrate(&database_url).await?;

    if seed_on_start {
        info!("TASK_SERVER_SEED=1 — seeding workspace doc before listening");
        seed::run(persistence.clone(), WORKSPACE_DOC_ID).await?;
    }

    // Org-vault / formatting-demo seeding was ripped along with the
    // Loro-backed Knowledge entity layer. Vault content lives in
    // the file-backed vault (`vault::Backend` mounted on the
    // `VaultSyncRpc` arm); seed it by dropping files into
    // `$TASK_SERVER_VAULT_ROOT` rather than via the server boot.

    let state = AppState::new(persistence).await?;
    let app = router(state);

    info!(%bind, "listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

fn env_truthy(key: &str) -> bool {
    matches!(
        std::env::var(key).ok().as_deref(),
        Some("1" | "true" | "True" | "TRUE" | "yes" | "Yes" | "YES" | "on")
    )
}
