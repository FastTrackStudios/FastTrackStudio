//! signal-engine — the headless guitar-rig core, served over a WebSocket.
//!
//! Opens the live audio engine (guitar in → NAM chain → out), loads the
//! profile, and mounts the rig's vox `LayerRouter` at `ws://<host>:4040/vox`.
//! Remotes (the `apps/web` browser UI, other machines on the network) drive
//! it through the exact same generated clients the desktop app uses over its
//! in-process link — the core cannot tell the difference.

use axum::extract::{State, WebSocketUpgrade};
use axum::response::{IntoResponse, Response};
use axum::routing::get;
use axum::Router;

use architect::axum_ws;
use signal_guitar::GuitarRigBackend;
use signal_guitar::proto::rig::Rig as _;
use tower_http::services::{ServeDir, ServeFile};

/// Default bind address; override with `SIGNAL_ENGINE_ADDR` (or the legacy
/// `RIGD_ADDR`, still honored so existing live setups keep working).
const DEFAULT_ADDR: &str = "0.0.0.0:4040";

/// Bind address: `SIGNAL_ENGINE_ADDR` wins, then the legacy `RIGD_ADDR`,
/// then the default.
fn bind_addr() -> String {
    std::env::var("SIGNAL_ENGINE_ADDR")
        .or_else(|_| std::env::var("RIGD_ADDR"))
        .unwrap_or_else(|_| DEFAULT_ADDR.to_string())
}

/// Locate the built signal-web bundle (the browser remote) so the engine can
/// serve it itself — any device on the LAN opens `http://<host>:4040/` and
/// gets the control UI. First match wins:
///
/// 1. `SIGNAL_WEB_DIST` env var (explicit override)
/// 2. `<exe_dir>/signal-web` (deployed layout — the bundle sits next to the
///    binary; `just signal-web-sync` puts it there)
/// 3. dx dev build output relative to the workspace `target/` the engine was
///    built into: `target/dx/signal-web/{release,debug}/web/public`
///
/// A candidate only counts if it contains an `index.html`. Returns `None`
/// when no bundle exists — the engine runs fine headless (embedded case),
/// serving only `/health` + `/vox`.
fn web_dist_dir() -> Option<std::path::PathBuf> {
    if let Ok(dir) = std::env::var("SIGNAL_WEB_DIST") {
        let p = std::path::PathBuf::from(&dir);
        if p.join("index.html").is_file() {
            return Some(p);
        }
        tracing::warn!("SIGNAL_WEB_DIST={dir} has no index.html — ignoring");
    }

    let exe_dir = std::env::current_exe()
        .ok()
        .and_then(|p| p.parent().map(|d| d.to_path_buf()))?;
    let candidates = [
        // Deployed: bundle beside the binary.
        exe_dir.join("signal-web"),
        // Dev: exe lives in target/{debug,release}; dx output is a sibling
        // under target/dx/.
        exe_dir.join("../dx/signal-web/release/web/public"),
        exe_dir.join("../dx/signal-web/debug/web/public"),
    ];
    candidates
        .into_iter()
        .find(|p| p.join("index.html").is_file())
}

#[derive(Clone)]
struct AppState {
    router: architect::LayerRouter,
}

/// One vox connection per WebSocket upgrade; the shared [`architect::LayerRouter`]
/// dispatches every service (Rig, RigStream, AudioSettings) by method id.
async fn vox_handler(ws: WebSocketUpgrade, State(state): State<AppState>) -> Response {
    ws.on_upgrade(move |socket| async move {
        let router = state.router.clone();
        let acceptor = axum_ws::lane_acceptor_fn(move |_req, connection| {
            connection.handle_with(router.clone());
            Ok(())
        });
        axum_ws::serve(socket, acceptor).await;
    })
    .into_response()
}

/// Serve the same router over an iroh p2p endpoint — remotes dial the
/// engine by bare endpoint id from any network, no port forwarding. The
/// endpoint's secret key persists at `<config>/iroh.key` so the id is
/// stable across restarts; the id itself is logged and written to
/// `<config>/iroh-endpoint-id` for other devices/agents to read.
#[cfg(feature = "iroh")]
async fn serve_iroh(router: architect::LayerRouter) {
    use architect::iroh_link;

    let config_dir = signal_sampler::rig_prefs::signal_config_dir();
    let secret_key = match iroh_link::load_or_create_secret_key(&config_dir.join("iroh.key")) {
        Ok(k) => k,
        Err(e) => {
            tracing::error!(error = %e, "iroh secret key unavailable; p2p transport disabled");
            return;
        }
    };
    let endpoint = match iroh_link::bind_endpoint(secret_key).await {
        Ok(ep) => ep,
        Err(e) => {
            tracing::error!(error = %e, "iroh endpoint bind failed; p2p transport disabled");
            return;
        }
    };
    tracing::info!("iroh endpoint id: {}", endpoint.id());
    if let Err(e) = std::fs::write(
        config_dir.join("iroh-endpoint-id"),
        format!("{}\n", endpoint.id()),
    ) {
        tracing::warn!(error = %e, "could not write iroh-endpoint-id");
    }

    let acceptor = iroh_link::lane_acceptor_fn(move |_req, connection| {
        connection.handle_with(router.clone());
        Ok(())
    });
    iroh_link::serve_endpoint(&endpoint, acceptor).await;
}

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info".into()),
        )
        .init();

    // Log every panic loudly (thread name + backtrace) before the default
    // hook runs. Panics stay unwinding — control-plane panics are caught
    // and survived (the rig's meter pump self-heals; audio keeps playing)
    // — but none may die silently mid-service.
    let default_hook = std::panic::take_hook();
    std::panic::set_hook(Box::new(move |info| {
        let thread = std::thread::current();
        let backtrace = std::backtrace::Backtrace::force_capture();
        tracing::error!(
            thread = thread.name().unwrap_or("<unnamed>"),
            "panic: {info}\n{backtrace}"
        );
        default_hook(info);
    }));

    // The headless core: open the audio device + load the profile off-thread,
    // exactly like the desktop app's auto-start.
    let backend = GuitarRigBackend::new();
    backend.start();

    let state = AppState {
        router: backend.router(),
    };

    #[cfg(feature = "iroh")]
    tokio::spawn(serve_iroh(state.router.clone()));

    let mut app = Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/vox", get(vox_handler))
        .with_state(state);

    let addr = bind_addr();

    // Serve the browser remote (signal-web) as static files from the same
    // router, with an index.html fallback so client-side routes deep-link.
    let web_dist = web_dist_dir();
    match &web_dist {
        Some(dist) => {
            let serve = ServeDir::new(dist).fallback(ServeFile::new(dist.join("index.html")));
            app = app.fallback_service(serve);
        }
        None => {
            tracing::warn!(
                "signal-web bundle not found (SIGNAL_WEB_DIST, <exe_dir>/signal-web, \
                 target/dx/signal-web) — serving /health + /vox only"
            );
        }
    }

    let listener = tokio::net::TcpListener::bind(&addr)
        .await
        .unwrap_or_else(|e| panic!("bind {addr}: {e}"));
    tracing::info!("signal-engine serving ws://{addr}/vox");
    if let Some(dist) = &web_dist {
        tracing::info!("web remote: http://{addr}/ (bundle: {})", dist.display());
    }
    axum::serve(listener, app).await.expect("axum serve");
}
