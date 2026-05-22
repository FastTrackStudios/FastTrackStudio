//! Thin shell — env-driven config, then hands off to `task_server::router`.

use std::net::SocketAddr;

use eyre::WrapErr;
use task_server::{AppState, router};
use tracing::info;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "task_server=info,tower_http=info".into()),
        )
        .init();

    let bind: SocketAddr = std::env::var("TASK_SERVER_BIND")
        .unwrap_or_else(|_| "0.0.0.0:9090".into())
        .parse()
        .wrap_err("invalid TASK_SERVER_BIND")?;

    // Vault content lives in `$TASK_SERVER_VAULT_ROOT` (file
    // system). The auth DB lives at the standard XDG path. No
    // CRDT persistence to set up here.
    let state = AppState::new().await?;
    let app = router(state);

    info!(%bind, "listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
