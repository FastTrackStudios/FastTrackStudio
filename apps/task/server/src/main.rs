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
    // Base dir for the always-on media route below (`<data_root>/orgs`).
    let orgs_dir = state.data_root.orgs_dir();
    let mut app = router(state);

    // Song media at `/media/…`, ALWAYS ON, filesystem-first: a request for
    // `/media/songs/<slug>/manifest.json` (or a stem) is resolved against
    // EVERY org's `resources/` dir — the first org that has the file wins.
    // No ingest, no content-addressing, no env var: drop a file into an
    // org's `resources/` (e.g. over network storage) and it's served. Same
    // `/media/…` URLs the whole UI already builds (org-less, same-origin).
    {
        use axum::extract::Path as AxPath;
        use axum::http::{StatusCode, header};
        use axum::response::IntoResponse;
        use axum::routing::get;
        let media_orgs = orgs_dir.clone();
        app = app.route(
            "/media/{*path}",
            get(move |AxPath(rel): AxPath<String>| {
                let orgs = media_orgs.clone();
                async move {
                    // Reject traversal; `rel` is already percent-decoded.
                    if rel.split('/').any(|seg| seg == ".." || seg.is_empty()) {
                        return StatusCode::NOT_FOUND.into_response();
                    }
                    let mut hit = None;
                    if let Ok(entries) = std::fs::read_dir(&orgs) {
                        for e in entries.flatten() {
                            let cand = e.path().join("resources").join(&rel);
                            if cand.is_file() {
                                hit = Some(cand);
                                break;
                            }
                        }
                    }
                    let Some(path) = hit else {
                        return StatusCode::NOT_FOUND.into_response();
                    };
                    match tokio::fs::read(&path).await {
                        Ok(bytes) => {
                            let ct = match path.extension().and_then(|x| x.to_str()) {
                                Some("json") => "application/json",
                                Some("ogg") => "audio/ogg",
                                Some("wav") => "audio/wav",
                                Some("mp3") => "audio/mpeg",
                                Some("webm") => "audio/webm",
                                Some("kf") | Some("txt") | Some("md") => {
                                    "text/plain; charset=utf-8"
                                }
                                _ => "application/octet-stream",
                            };
                            ([(header::CONTENT_TYPE, ct)], bytes).into_response()
                        }
                        Err(_) => StatusCode::NOT_FOUND.into_response(),
                    }
                }
            }),
        );
        info!(orgs_dir = %orgs_dir.display(), "serving song media at /media (org-resolved)");
    }

    // Optional web bundle (the fasttrackstudio.app single-binary deploy):
    // the dx web build served from the very host that serves
    // `/org/{slug}/vox`, so the browser's same-origin vox URL works with no
    // second port. Off unless TASK_SERVER_WEB_DIR points somewhere.

    // TASK_SERVER_WEB_DIR — a dx web build (dir with index.html + wasm/ +
    // assets/). Asset dirs are served as files; everything else falls
    // through to index.html so the wasm router handles SPA deep links
    // like `/session/{org}/{collection}`. Vox/health/blob routes above
    // take precedence.
    if let Ok(web_dir) = std::env::var("TASK_SERVER_WEB_DIR") {
        use axum::response::Html;
        use tower_http::services::ServeDir;
        let web = std::path::PathBuf::from(&web_dir);
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
