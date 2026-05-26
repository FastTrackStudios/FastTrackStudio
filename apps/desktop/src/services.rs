//! Session Services
//!
//! Connects to the REAPER-hosted `fts-extensions` over its Unix socket
//! (via the shared `session_cli::connection::connect` helper) and points the
//! Session UI at the remote `SetlistService` running inside REAPER. The
//! in-process WebSocket gateway re-exposes those same services to browsers by
//! forwarding their RPC over the same connection.

use std::path::Path;

use eyre::{Result, WrapErr};
use session::{setlist_service_service_descriptor, song_service_service_descriptor, SetlistServiceClient};
use session_ui::Session;

use crate::gateway;

/// Start the WebSocket gateway immediately (does not require REAPER).
///
/// Browser RPC for the session/song services is forwarded over the shared
/// REAPER connection (see [`gateway::ForwardingHandler`]). The connection is
/// late-bound: until [`connect_to_reaper`] succeeds, forwarded calls return a
/// retryable `ConnectionClosed` error, and browsers still receive pushed
/// setlist events via the `WebClientService` registry.
pub async fn start_gateway() -> Result<gateway::GatewayInfo> {
    let handler = forwarding_handler();

    let bind_addr = std::env::var("GATEWAY_WS_ADDR").unwrap_or_else(|_| "0.0.0.0:3030".to_string());
    let static_dir = std::env::var("GATEWAY_WS_STATIC_DIR")
        .ok()
        .or_else(discover_web_static_dir);
    if let Some(ref dir) = static_dir {
        tracing::info!("Serving web app from: {dir}");
    }

    let (info_tx, info_rx) = tokio::sync::oneshot::channel();
    tokio::spawn(async move {
        if let Err(e) =
            gateway::start_gateway(handler, &bind_addr, static_dir.as_deref(), info_tx).await
        {
            tracing::error!("WebSocket gateway error: {e}");
        }
    });

    let gw_info = info_rx
        .await
        .map_err(|_| eyre::eyre!("Gateway failed to start"))?;
    Ok(gw_info)
}

/// Build a [`gateway::RoutedHandler`] that forwards `SetlistService` and
/// `SongService` request/response RPC to the live REAPER connection.
///
/// The wrapped [`gateway::ForwardingHandler`] reads `remote_conn` *per call*,
/// so a single handler keeps working after REAPER restarts and the published
/// caller is swapped. Shared by the browser gateway and the in-process session
/// proxy (see [`init_session_proxy`]).
fn forwarding_handler() -> gateway::RoutedHandler {
    let forward = gateway::ForwardingHandler::new(gateway::remote_conn());
    gateway::RoutedHandler::new()
        .with(&setlist_service_service_descriptor(), forward.clone())
        .with(&song_service_service_descriptor(), forward)
}

/// Connect to the REAPER-hosted `fts-extensions` and publish the live caller.
///
/// Uses the shared `session_cli::connection::connect` helper (the same path
/// the CLI uses): it discovers the newest live `/tmp/fts-daw-*.sock`, performs
/// the vox handshake, and returns a `vox::Caller`. The setlist is owned and
/// built by the session module running inside REAPER — the desktop is a pure
/// client.
///
/// The caller is published to `remote_conn` so both the browser gateway and the
/// in-process session proxy forward over it, and is returned so the connection
/// manager can park on [`vox::Caller::closed`] and reconnect when REAPER
/// restarts. Can be retried until REAPER is available.
pub async fn connect_to_reaper() -> Result<vox::Caller> {
    let caller = session_cli::connection::connect(None)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    tracing::info!("Connected to fts-extensions in REAPER");

    // Publish the live caller. The gateway forwarder reads this per call, so
    // swapping it here is all the browser path needs for reconnection.
    *gateway::remote_conn().lock().expect("remote conn poisoned") = Some(caller.clone());

    // Point the Session UI at the live caller. `Session::init` is set-once on
    // native, so this only takes effect on the first connect; later reconnects
    // log the (ignored) "already initialized" error. Interactive controls thus
    // reconnect only on the first connection — tracked separately.
    let client = SetlistServiceClient::new(caller.clone());
    if let Err(e) = Session::init(client) {
        tracing::debug!("Session::init skipped (already initialized): {e:?}");
    }

    Ok(caller)
}

/// Try to find the web app's `dx build` output directory, relative to the
/// cargo workspace root (release first, then debug).
fn discover_web_static_dir() -> Option<String> {
    let workspace_root = Path::new(env!("CARGO_MANIFEST_DIR")).parent()?.parent()?;

    let candidates = [
        workspace_root.join("target/dx/fasttrackstudio-web/release/web/public"),
        workspace_root.join("target/dx/fasttrackstudio-web/debug/web/public"),
    ];

    for candidate in &candidates {
        if candidate.join("index.html").exists() {
            return Some(candidate.to_string_lossy().into_owned());
        }
    }

    None
}
