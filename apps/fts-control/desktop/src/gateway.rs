//! In-Process WebSocket Gateway
//!
//! Runs the WebSocket gateway server in-process, allowing browsers to connect
//! to the desktop app and access session services via WebSocket.
//!
//! # Architecture
//!
//! ```text
//!     Browser ──WebSocket──► StandaloneGateway ──► LocalServices
//!                              (Axum server)        (in-process)
//! ```
//!
//! Unlike the cell-based gateway that forwards to a host via `ConnectionHandle`,
//! this gateway takes a `ServiceDispatcher` directly and handles requests in-process.

use gateway_ws::StandaloneGateway;
use roam::session::ServiceDispatcher;
use tracing::info;

/// Start the WebSocket gateway server.
///
/// # Arguments
///
/// * `dispatcher` - The service dispatcher to handle incoming RPC calls
/// * `bind_addr` - Address to bind the server to (e.g., "0.0.0.0:3030")
/// * `static_dir` - Optional directory to serve static files from (web app)
///
/// # Example
///
/// ```rust,ignore
/// let services = LocalServices::new();
/// let dispatcher = services.create_dispatcher();
///
/// // Start gateway on port 3030
/// start_gateway(dispatcher, "0.0.0.0:3030", Some("./dist")).await?;
/// ```
pub async fn start_gateway<D>(
    dispatcher: D,
    bind_addr: &str,
    static_dir: Option<&str>,
) -> eyre::Result<()>
where
    D: ServiceDispatcher + Clone + Send + Sync + 'static,
{
    info!("Starting WebSocket gateway on {}", bind_addr);

    let mut gateway = StandaloneGateway::new(dispatcher);

    if let Some(dir) = static_dir {
        gateway = gateway.with_static_dir(dir);
    }

    gateway.run(bind_addr).await
}

/// Gateway configuration for the desktop app.
#[derive(Clone, Debug)]
pub struct GatewayConfig {
    /// Address to bind the WebSocket server to.
    pub bind_addr: String,
    /// Optional directory for serving static files (web app).
    pub static_dir: Option<String>,
}

impl Default for GatewayConfig {
    fn default() -> Self {
        Self {
            bind_addr: std::env::var("GATEWAY_WS_ADDR")
                .unwrap_or_else(|_| "0.0.0.0:3030".to_string()),
            static_dir: std::env::var("GATEWAY_WS_STATIC_DIR").ok(),
        }
    }
}
