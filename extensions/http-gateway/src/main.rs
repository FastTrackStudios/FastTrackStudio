//! HTTP Gateway Extension
//!
//! Exposes ROAM services over HTTP/JSON and WebSocket for web browsers and mobile apps.
//! Uses the `roam-http-bridge` crate for automatic JSON↔postcard transcoding.
//!
//! Architecture:
//! - HTTP Request → HTTP Gateway → BridgeRouter → roam SHM → Host → DAW Services
//! - WebSocket → Real-time streaming with `Tx<T>`/`Rx<T>` channels
//!
//! Endpoints:
//! - POST /api/{service}/{method} - RPC call with JSON args
//! - GET  /api/@ws - WebSocket streaming (roam-bridge.v1 subprotocol)
//! - GET  /* - Static files from FTS_CONTROL_DIST (SPA with index.html fallback)
//!
//! Environment:
//! - FTS_CONTROL_DIST: Path to built control app (default: disabled)
//! - FTS_GATEWAY_HOST: Host to bind to (default: 0.0.0.0 for network access)
//! - FTS_GATEWAY_PORT: Port to bind to (default: 3000)

use axum::Router;
use extension_runtime::{run_extension, ConnectionHandle, NoDispatcher};
use roam_http_bridge::{BridgeRouter, GenericBridgeService};
use std::net::SocketAddr;
use std::sync::{Arc, OnceLock};
use tower_http::cors::{Any, CorsLayer};
use tower_http::services::{ServeDir, ServeFile};
use tracing::{error, info, warn};

// Import the service detail function from host-proto
// The #[roam::service] macro generates `host_service_service_detail()`
use host_proto::host_service_service_detail;

/// Find the control app dist directory
/// Priority: 1) Sibling "control" directory (for deployed extensions)
///           2) FTS_CONTROL_DIST env var (for development)
fn find_control_dist() -> Option<std::path::PathBuf> {
    // Try sibling "control" directory first
    // The extension binary is at Extensions/FTS/http-gateway-extension
    // The control app is at Extensions/FTS/control/
    if let Ok(exe_path) = std::env::current_exe() {
        // Resolve symlinks to get the actual binary location
        if let Ok(real_path) = exe_path.canonicalize() {
            if let Some(parent) = real_path.parent() {
                let sibling_control = parent.join("control");
                if sibling_control.exists() && sibling_control.join("index.html").exists() {
                    return Some(sibling_control);
                }
            }
        }
        // Also try without canonicalize (symlinked location)
        if let Some(parent) = exe_path.parent() {
            let sibling_control = parent.join("control");
            if sibling_control.exists() && sibling_control.join("index.html").exists() {
                return Some(sibling_control);
            }
        }
    }

    // Fall back to FTS_CONTROL_DIST env var
    if let Ok(dist_path) = std::env::var("FTS_CONTROL_DIST") {
        let path = std::path::PathBuf::from(&dist_path);
        if path.exists() {
            return Some(path);
        }
        warn!(
            "⚠️  FTS_CONTROL_DIST path does not exist: {}",
            path.display()
        );
    }

    None
}

/// Start the HTTP server with the bridge router
async fn start_http_server(
    handle_cell: Arc<OnceLock<ConnectionHandle>>,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    // Wait for the handle to be initialized
    let handle = loop {
        if let Some(h) = handle_cell.get() {
            break h.clone();
        }
        tokio::time::sleep(std::time::Duration::from_millis(10)).await;
    };

    // Get the ServiceDetail for HostService
    // We need to leak it to get a 'static reference (required by GenericBridgeService)
    let host_service_detail: &'static _ = Box::leak(Box::new(host_service_service_detail()));

    // Create a GenericBridgeService that auto-exposes all HostService methods
    let host_service = GenericBridgeService::new(handle, host_service_detail);

    info!("📦 Registered services:");
    info!(
        "   - HostService ({} methods)",
        host_service_detail.methods.len()
    );
    for method in &host_service_detail.methods {
        info!("     • {}", method.method_name);
    }

    // Build the bridge router with our services
    // This automatically provides:
    // - POST /{service}/{method} for RPC calls
    // - GET /@ws for WebSocket streaming
    let bridge_router = BridgeRouter::new().service(host_service).build();

    // CORS layer for API requests
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

    // Check for static file serving (FTS Control app)
    // Priority: 1) Sibling "control" directory, 2) FTS_CONTROL_DIST env var
    let control_dist = find_control_dist();
    let has_control_dist = control_dist.is_some();
    if let Some(dist_path) = control_dist {
        let index_path = dist_path.join("index.html");
        if index_path.exists() {
            info!("📁 Serving static files from: {}", dist_path.display());
            // ServeDir with SPA fallback - unknown routes return index.html
            let static_service =
                ServeDir::new(&dist_path).fallback(ServeFile::new(&index_path));
            app = app.fallback_service(static_service);
        } else {
            warn!(
                "⚠️  Control dist found but index.html missing: {}",
                index_path.display()
            );
        }
    }

    // Parse host and port from environment (default: 0.0.0.0:3000 for network access)
    let host: [u8; 4] = std::env::var("FTS_GATEWAY_HOST")
        .ok()
        .and_then(|h| {
            let parts: Vec<u8> = h.split('.').filter_map(|p| p.parse().ok()).collect();
            if parts.len() == 4 {
                Some([parts[0], parts[1], parts[2], parts[3]])
            } else {
                None
            }
        })
        .unwrap_or([0, 0, 0, 0]); // Default to 0.0.0.0 for network access

    let port: u16 = std::env::var("FTS_GATEWAY_PORT")
        .ok()
        .and_then(|p| p.parse().ok())
        .unwrap_or(3000);

    let addr = SocketAddr::from((host, port));
    let listener = tokio::net::TcpListener::bind(addr).await?;

    info!("════════════════════════════════════════════════════════════");
    info!("  🌐 HTTP Gateway listening at: http://{}", addr);
    info!("     POST /api/HostService/{{method}} - RPC calls");
    info!("     GET  /api/@ws - WebSocket (subprotocol: roam-bridge.v1)");
    if has_control_dist {
        info!("     GET  /* - FTS Control static files (SPA)");
    }
    info!("════════════════════════════════════════════════════════════");
    info!("✅ HTTP Gateway ready!");

    axum::serve(listener, app).await?;

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_extension!("http-gateway", |handle| {
        // Clone the handle cell for the HTTP server task
        let handle_for_server = handle.clone();

        // Start HTTP server in background task
        extension_runtime::tokio::spawn(async move {
            if let Err(e) = start_http_server(handle_for_server).await {
                error!("HTTP server error: {:#}", e);
            }
        });

        // The extension needs a dispatcher for the root connection
        // We don't expose any services ourselves, just run the HTTP server
        NoDispatcher
    })
}
