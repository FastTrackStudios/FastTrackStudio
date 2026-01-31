//! Embedded HTTP server for serving control-web and bridging to ROAM
//!
//! Provides:
//! - Static file serving for control-web WASM app
//! - ROAM HTTP Bridge (POST /api/{service}/{method} for RPC)
//! - ROAM WebSocket (GET /ws for roam-bridge.v1 streaming)

use crate::roam_client::ConnectionStatus;
use crate::shared_state::get_shared_state;
use axum::Router;
use host_proto::transport_service_detail;
use roam_http_bridge::{BridgeRouter, GenericBridgeService};
use std::net::SocketAddr;
use tower_http::cors::{Any, CorsLayer};
use tower_http::services::{ServeDir, ServeFile};
use tracing::{info, warn};

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

/// Run the HTTP server
pub async fn run_server() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Get shared state
    let shared = get_shared_state();

    // Wait until connected to REAPER
    info!("Waiting for REAPER connection before starting HTTP server...");
    loop {
        let status = shared.roam_client.status().await;
        if matches!(status, ConnectionStatus::Connected(_)) {
            info!("Connected to REAPER, starting HTTP server");
            break;
        }
        tokio::time::sleep(std::time::Duration::from_millis(100)).await;
    }

    // Get the ROAM connection handle
    let handle = shared.roam_client.get_handle().await?;

    // Get the ServiceDetail for Transport
    let transport_detail: &'static _ = Box::leak(Box::new(transport_service_detail()));

    // Create a GenericBridgeService that auto-exposes all Transport methods
    let transport_service = GenericBridgeService::new(handle, transport_detail);

    info!("📦 Registered services:");
    info!(
        "   - Transport ({} methods)",
        transport_detail.methods.len()
    );
    for method in &transport_detail.methods {
        info!("     • {}", method.method_name);
    }

    // Build the bridge router
    // Provides:
    // - POST /api/Transport/{method} for RPC calls
    // - GET /api/@ws for WebSocket streaming (roam-bridge.v1)
    let bridge_router = BridgeRouter::new().service(transport_service).build();

    // CORS layer for cross-origin requests from phones/tablets
    let cors = CorsLayer::new()
        .allow_origin(Any)
        .allow_methods(Any)
        .allow_headers(Any);

    // Build router with API endpoints
    let mut app = Router::new()
        .nest("/api", bridge_router)
        .route(
            "/health",
            axum::routing::get(|| async { axum::Json(serde_json::json!({"status": "ok"})) }),
        )
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
    info!("  🌐 HTTP Server listening at: http://0.0.0.0:{}", port);
    info!("     POST /api/Transport/{{method}} - RPC calls");
    info!("     GET  /api/@ws - WebSocket (roam-bridge.v1)");
    info!("     GET  /* - Control web UI");
    info!("  Access from other devices on your network");
    info!("════════════════════════════════════════════════════════════");

    axum::serve(listener, app).await?;

    Ok(())
}
