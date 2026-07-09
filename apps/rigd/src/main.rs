//! signal-rigd — the headless guitar-rig core, served over a WebSocket.
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

/// Default bind address; override with `RIGD_ADDR`.
const DEFAULT_ADDR: &str = "0.0.0.0:4040";

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

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| "info".into()),
        )
        .init();

    // The headless core: open the audio device + load the profile off-thread,
    // exactly like the desktop app's auto-start.
    let backend = GuitarRigBackend::new();
    backend.start();

    let state = AppState {
        router: backend.router(),
    };
    let app = Router::new()
        .route("/health", get(|| async { "ok" }))
        .route("/vox", get(vox_handler))
        .with_state(state);

    let addr = std::env::var("RIGD_ADDR").unwrap_or_else(|_| DEFAULT_ADDR.to_string());
    let listener = tokio::net::TcpListener::bind(&addr)
        .await
        .unwrap_or_else(|e| panic!("bind {addr}: {e}"));
    tracing::info!("signal-rigd serving ws://{addr}/vox");
    axum::serve(listener, app).await.expect("axum serve");
}
