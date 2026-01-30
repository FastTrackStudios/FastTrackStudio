//! Embedded HTTP server for serving control-web and bridging to ROAM
//!
//! Provides:
//! - Static file serving for control-web WASM app
//! - WebSocket endpoint for real-time transport streaming to browsers
//! - REST endpoints for transport commands (play, stop, record)

use crate::roam_client::ConnectionStatus;
use crate::shared_state::get_shared_state;
use axum::extract::ws::{Message, WebSocket, WebSocketUpgrade};
use axum::extract::State;
use axum::response::IntoResponse;
use axum::routing::get;
use axum::Router;
use futures_util::{SinkExt, StreamExt};
use host_proto::PlaybackState;
use std::net::SocketAddr;
use std::sync::Arc;
use tower_http::cors::{Any, CorsLayer};
use tower_http::services::{ServeDir, ServeFile};
use tracing::{debug, error, info, warn};

/// Find the control-web dist directory
fn find_control_web_dist() -> Option<std::path::PathBuf> {
    // Check common locations relative to the executable
    let candidates = [
        // Development: sibling in target/dx
        "../control-web/debug/web/public",
        "../control-web/release/web/public",
        // Development: relative to workspace root
        "../../target/dx/control-web/debug/web/public",
        "../../target/dx/control-web/release/web/public",
        // Deployed: sibling directory
        "./control-web",
        "../control-web",
    ];

    // Try relative to current exe
    if let Ok(exe_path) = std::env::current_exe() {
        if let Some(exe_dir) = exe_path.parent() {
            for candidate in &candidates {
                let path = exe_dir.join(candidate);
                if path.exists() && path.join("index.html").exists() {
                    return Some(path.canonicalize().unwrap_or(path));
                }
            }
        }
    }

    // Try relative to current working directory
    if let Ok(cwd) = std::env::current_dir() {
        for candidate in &candidates {
            let path = cwd.join(candidate);
            if path.exists() && path.join("index.html").exists() {
                return Some(path.canonicalize().unwrap_or(path));
            }
        }

        // Also try target/dx directly from workspace root
        let target_debug = cwd.join("target/dx/control-web/debug/web/public");
        if target_debug.exists() && target_debug.join("index.html").exists() {
            return Some(target_debug);
        }
        let target_release = cwd.join("target/dx/control-web/release/web/public");
        if target_release.exists() && target_release.join("index.html").exists() {
            return Some(target_release);
        }
    }

    // Fall back to environment variable
    if let Ok(dist_path) = std::env::var("FTS_CONTROL_WEB_DIST") {
        let path = std::path::PathBuf::from(&dist_path);
        if path.exists() && path.join("index.html").exists() {
            return Some(path);
        }
    }

    None
}

/// Shared state for HTTP handlers
#[derive(Clone)]
struct ServerState {
    shared: Arc<crate::shared_state::SharedState>,
}

/// Run the HTTP server
pub async fn run_server() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Get shared state
    let shared = get_shared_state();
    let state = ServerState { shared: shared.clone() };

    // CORS layer for cross-origin requests from phones/tablets
    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any);

    // Build router with API routes
    let mut app = Router::new()
        .route("/health", get(health_handler))
        .route("/api/status", get(status_handler))
        .route("/api/transport/play", get(play_handler))
        .route("/api/transport/stop", get(stop_handler))
        .route("/api/transport/record", get(record_handler))
        .route("/ws", get(ws_handler))
        .with_state(state)
        .layer(cors);

    // Serve control-web static files if found
    if let Some(dist_path) = find_control_web_dist() {
        let index_path = dist_path.join("index.html");
        info!("Serving control-web from: {}", dist_path.display());

        // ServeDir with SPA fallback - unknown routes return index.html
        let static_service = ServeDir::new(&dist_path).fallback(ServeFile::new(&index_path));
        app = app.fallback_service(static_service);
    } else {
        info!("control-web not found - run 'just build-control-web-debug' first");
        // Serve a simple "not found" page
        app = app.fallback(|| async {
            axum::response::Html(
                r#"<!DOCTYPE html>
<html>
<head><title>FTS Control</title></head>
<body style="font-family: system-ui; padding: 2rem; text-align: center;">
<h1>Control Web UI Not Found</h1>
<p>Build the web UI first:</p>
<pre>just build-control-web-debug</pre>
<p>Then restart control-desktop.</p>
</body>
</html>"#,
            )
        });
    }

    // Bind to all interfaces for network access
    let port = shared.server_port;
    let addr = SocketAddr::from(([0, 0, 0, 0], port));
    let listener = tokio::net::TcpListener::bind(addr).await?;

    info!("════════════════════════════════════════════════════════════");
    info!("  HTTP Server listening at: http://0.0.0.0:{}", port);
    info!("  WebSocket endpoint: ws://0.0.0.0:{}/ws", port);
    info!("  Access from other devices on your network");
    info!("════════════════════════════════════════════════════════════");

    axum::serve(listener, app).await?;

    Ok(())
}

/// Health check endpoint
async fn health_handler() -> impl IntoResponse {
    axum::Json(serde_json::json!({"status": "ok"}))
}

/// Connection status endpoint
async fn status_handler(State(state): State<ServerState>) -> impl IntoResponse {
    let status = state.shared.roam_client.status().await;
    let (connected, reaper_pid) = match status {
        ConnectionStatus::Connected(inst) => (true, Some(inst.pid)),
        _ => (false, None),
    };

    axum::Json(serde_json::json!({
        "connected": connected,
        "reaper_pid": reaper_pid
    }))
}

/// Play transport
async fn play_handler(State(state): State<ServerState>) -> impl IntoResponse {
    match state.shared.roam_client.play().await {
        Ok(_) => axum::Json(serde_json::json!({"success": true})),
        Err(e) => axum::Json(serde_json::json!({"success": false, "error": e})),
    }
}

/// Stop transport
async fn stop_handler(State(state): State<ServerState>) -> impl IntoResponse {
    match state.shared.roam_client.stop().await {
        Ok(_) => axum::Json(serde_json::json!({"success": true})),
        Err(e) => axum::Json(serde_json::json!({"success": false, "error": e})),
    }
}

/// Record transport
async fn record_handler(State(state): State<ServerState>) -> impl IntoResponse {
    match state.shared.roam_client.record().await {
        Ok(_) => axum::Json(serde_json::json!({"success": true})),
        Err(e) => axum::Json(serde_json::json!({"success": false, "error": e})),
    }
}

/// WebSocket handler - streams transport state to browser
async fn ws_handler(
    ws: WebSocketUpgrade,
    State(state): State<ServerState>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_websocket(socket, state))
}

/// Handle a WebSocket connection
async fn handle_websocket(socket: WebSocket, state: ServerState) {
    let (mut sender, mut receiver) = socket.split();

    info!("WebSocket client connected");

    // Check if connected to REAPER
    let status = state.shared.roam_client.status().await;
    if !matches!(status, ConnectionStatus::Connected(_)) {
        warn!("WebSocket client connected but REAPER not connected");
        let msg = serde_json::json!({
            "type": "error",
            "message": "Not connected to REAPER"
        });
        let _ = sender.send(Message::Text(msg.to_string().into())).await;
        return;
    }

    // Subscribe to transport stream
    let rx_result = state.shared.roam_client.subscribe_transport().await;
    let mut rx = match rx_result {
        Ok(rx) => rx,
        Err(e) => {
            error!("Failed to subscribe to transport: {}", e);
            let msg = serde_json::json!({
                "type": "error",
                "message": format!("Failed to subscribe: {}", e)
            });
            let _ = sender.send(Message::Text(msg.to_string().into())).await;
            return;
        }
    };

    info!("WebSocket subscribed to transport stream");

    // Spawn task to handle incoming messages from browser
    let roam_client = state.shared.roam_client.clone();
    let recv_task = tokio::spawn(async move {
        while let Some(msg) = receiver.next().await {
            match msg {
                Ok(Message::Text(text)) => {
                    // Parse command from browser
                    if let Ok(cmd) = serde_json::from_str::<serde_json::Value>(&text) {
                        if let Some(action) = cmd.get("action").and_then(|a| a.as_str()) {
                            match action {
                                "play" => { let _ = roam_client.play().await; }
                                "stop" => { let _ = roam_client.stop().await; }
                                "record" => { let _ = roam_client.record().await; }
                                _ => debug!("Unknown action: {}", action),
                            }
                        }
                    }
                }
                Ok(Message::Close(_)) => {
                    info!("WebSocket client disconnected");
                    break;
                }
                Err(e) => {
                    error!("WebSocket receive error: {}", e);
                    break;
                }
                _ => {}
            }
        }
    });

    // Send transport updates to browser
    loop {
        match rx.recv().await {
            Ok(Some(state)) => {
                let msg = transport_to_json(&state);
                if sender.send(Message::Text(msg.into())).await.is_err() {
                    debug!("WebSocket send failed, client disconnected");
                    break;
                }
            }
            Ok(None) => {
                info!("Transport stream ended");
                break;
            }
            Err(e) => {
                error!("Transport stream error: {:?}", e);
                break;
            }
        }
    }

    // Clean up
    recv_task.abort();
    info!("WebSocket connection closed");
}

/// Convert PlaybackState to JSON for WebSocket
fn transport_to_json(state: &PlaybackState) -> String {
    serde_json::json!({
        "type": "transport",
        "is_playing": state.is_playing,
        "is_recording": state.is_recording,
        "position_seconds": state.position_seconds,
        "tempo_bpm": state.tempo_bpm
    }).to_string()
}
