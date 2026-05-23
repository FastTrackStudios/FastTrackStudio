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

    // Per-org data root: `$TASK_DATA_ROOT/orgs/<slug>/` holds
    // this server's auth/timer/finance sqlites + vault. The
    // slug comes from `$TASK_SERVER_ORG`; if unset, falls back
    // to single-org auto-pick (or auto-bootstraps `default`).
    // PR 4 will scan the orgs dir and serve all of them at
    // `/org/<slug>/...`.
    let org_slug = std::env::var("TASK_SERVER_ORG").ok();
    let state = AppState::new(org_slug.as_deref()).await?;
    let app = router(state);

    info!(%bind, "listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app).await?;

    Ok(())
}
