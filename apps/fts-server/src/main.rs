//! fts-server — the public FastTrackStudio.app platform server.
//!
//! Thin shell over `task_server::{AppState, router}` (same architecture as
//! task-server: per-org vox router serving org / vault / attachments /
//! collection), but a DISTINCT server: its own bind + data root, and
//! federatable with a task-server. This is where song libraries, setlists
//! (Collections), and — soon — the session engine + multitrack audio
//! streaming live for fasttrackstudio.app.
//!
//! Config (env):
//!   FTS_SERVER_BIND   socket addr to bind        (default 127.0.0.1:18080)
//!   FTS_DATA_ROOT     per-org data root          (maps to TASK_DATA_ROOT)
//!   FTS_SERVER_ORG    single org slug to serve   (optional; else all/auto)

use std::net::SocketAddr;

use eyre::WrapErr;
use task_server::{AppState, router};
use tracing::info;
use tracing_subscriber::EnvFilter;

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let env_filter = EnvFilter::try_from_default_env()
        .unwrap_or_else(|_| "fts_server=info,task_server=info,tower_http=info".into());
    tracing_subscriber::fmt().with_env_filter(env_filter).init();

    // fts-server owns its data root; map FTS_DATA_ROOT onto the var the
    // org layer reads (`TASK_DATA_ROOT`) so a distinct fts instance never
    // shares a task-server's orgs unless deliberately pointed at the same root.
    if let Ok(root) = std::env::var("FTS_DATA_ROOT") {
        // SAFETY: set before any async/threaded work reads the env.
        unsafe { std::env::set_var("TASK_DATA_ROOT", root) };
    }

    let bind: SocketAddr = std::env::var("FTS_SERVER_BIND")
        .unwrap_or_else(|_| "127.0.0.1:18080".into())
        .parse()
        .wrap_err("invalid FTS_SERVER_BIND")?;

    let org_slug = std::env::var("FTS_SERVER_ORG").ok();
    let state = AppState::new(org_slug.as_deref()).await?;
    let scope = state.scope.clone();
    let mut app = router(state);

    // Serve the fasttrackstudio.app web bundle SAME-ORIGIN alongside vox, so the
    // browser reaches `/org/{slug}/vox` on the very host that served the page
    // (architect's same-origin remote pattern — no second port to reach). The
    // vox / health / blob routes above take precedence; everything else falls
    // through to the SPA (index.html) so the client-side router handles deep
    // links like `/session/{org}/{collection}`. Set FTS_WEB_DIR to the dx build
    // output (the dir containing index.html + wasm/ + assets/).
    if let Ok(web_dir) = std::env::var("FTS_WEB_DIR") {
        use axum::response::Html;
        use tower_http::services::ServeDir;
        let web = std::path::PathBuf::from(&web_dir);
        // Asset dirs served as files; everything else (`/`, and SPA deep links
        // like `/session/{org}/{collection}`) returns index.html so the wasm
        // router takes over. Version-independent SPA fallback.
        let index_body = std::fs::read_to_string(web.join("index.html")).unwrap_or_default();
        app = app
            .nest_service("/wasm", ServeDir::new(web.join("wasm")))
            .nest_service("/assets", ServeDir::new(web.join("assets")))
            .fallback(move || {
                let b = index_body.clone();
                async move { Html(b) }
            });
        info!(%web_dir, "serving web bundle (SPA) same-origin");
    }

    info!(%bind, "fts-server listening");
    let listener = tokio::net::TcpListener::bind(bind).await?;
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await?;

    info!("shutdown — closing backend resources");
    scope.close().await;
    Ok(())
}

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
