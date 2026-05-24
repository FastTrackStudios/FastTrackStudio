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
    let forward = gateway::ForwardingHandler::new(gateway::remote_conn());

    let handler = gateway::RoutedHandler::new()
        .with(&setlist_service_service_descriptor(), forward.clone())
        .with(&song_service_service_descriptor(), forward);

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

/// Connect to the REAPER-hosted `fts-extensions` and point the Session UI at
/// its remote `SetlistService`.
///
/// Uses the shared `session_cli::connection::connect` helper (the same path
/// the CLI uses): it discovers the newest live `/tmp/fts-daw-*.sock`, performs
/// the vox handshake, and returns a `vox::Caller`. The setlist is owned and
/// built by the session module running inside REAPER — the desktop is a pure
/// client. The caller is also published to the gateway so browser RPC can be
/// forwarded over the same connection. Can be retried until REAPER is available.
pub async fn connect_to_reaper() -> Result<()> {
    let caller = session_cli::connection::connect(None)
        .await
        .wrap_err("connect to fts-extensions socket")?;
    tracing::info!("Connected to fts-extensions in REAPER");

    // Publish the connection so the gateway can forward browser RPC to REAPER.
    *gateway::remote_conn().lock().expect("remote conn poisoned") = Some(caller.clone());

    let client = SetlistServiceClient::new(caller.clone());
    Session::init(client)?;
    tracing::info!("Session UI initialized against remote SetlistService");
    Ok(())
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
