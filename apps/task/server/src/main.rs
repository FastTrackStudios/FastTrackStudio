//! Thin shell — env-driven config, then hands off to `task_server::router`.

use std::net::SocketAddr;

use eyre::WrapErr;
use task_server::{AppState, router};
use tracing::info;
use tracing_subscriber::layer::SubscriberExt;
use tracing_subscriber::util::SubscriberInitExt;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    // Sentry error/crash telemetry — hold the guard for all of `main`.
    let _sentry = task_telemetry::init("task-server");

    let env_filter = tracing_subscriber::EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| "task_server=info,tower_http=info".into());
    tracing_subscriber::registry()
        .with(env_filter)
        .with(tracing_subscriber::fmt::layer())
        .with(task_telemetry::tracing_layer())
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
    // Hold the construction scope so DB pools tear down in LIFO order
    // on shutdown; `router` takes ownership of `state`.
    let scope = state.scope.clone();
    let app = router(state);

    info!(%bind, "listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    info!("shutdown — closing backend resources");
    scope.close().await;
    Ok(())
}

/// Resolve when the process receives Ctrl-C (or SIGTERM on unix), so
/// axum stops accepting and in-flight requests drain before the scope
/// finalizers run.
async fn shutdown_signal() {
    let ctrl_c = async {
        let _ = tokio::signal::ctrl_c().await;
    };
    #[cfg(unix)]
    let terminate = async {
        if let Ok(mut sig) =
            tokio::signal::unix::signal(tokio::signal::unix::SignalKind::terminate())
        {
            sig.recv().await;
        }
    };
    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        () = ctrl_c => {},
        () = terminate => {},
    }
}
