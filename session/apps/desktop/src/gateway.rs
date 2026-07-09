//! In-Process WebSocket Gateway
//!
//! Runs a WebSocket server in-process, allowing browsers to connect
//! to the desktop app and access session services via vox binary RPC.
//!
//! # Architecture
//!
//! ```text
//!     Browser ──WebSocket──► axum server ──► vox acceptor ──► Handler
//! ```

use std::io;
use std::net::SocketAddr;
use std::sync::Arc;

use axum::Router;
use axum::extract::ws::{Message, WebSocket};
use axum::extract::{State, WebSocketUpgrade};
use axum::response::IntoResponse;
use axum::routing::get;
use session::{SetlistEvent, WebClientServiceClient};
use std::sync::OnceLock;
use tokio::net::TcpListener;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use tower_http::services::ServeDir;
use tracing::{debug, info, warn};

// ============================================================================
pub struct GatewayInfo {
    /// Port the gateway is listening on.
    pub port: u16,
    /// LAN IP address (e.g. 192.168.1.x), if detected.
    pub lan_ip: Option<String>,
    /// Whether the web app is being served (static files found).
    pub serving_web_app: bool,
}

impl GatewayInfo {
    /// URL for local access.
    pub fn local_url(&self) -> String {
        format!("http://localhost:{}", self.port)
    }

    /// URL for LAN access, if available.
    pub fn network_url(&self) -> Option<String> {
        self.lan_ip
            .as_ref()
            .map(|ip| format!("http://{}:{}", ip, self.port))
    }
}

/// Detect the machine's LAN IP by connecting a UDP socket to a public address.
fn detect_lan_ip() -> Option<String> {
    let socket = std::net::UdpSocket::bind("0.0.0.0:0").ok()?;
    socket.connect("8.8.8.8:80").ok()?;
    let addr = socket.local_addr().ok()?;
    Some(addr.ip().to_string())
}

pub async fn start_gateway(
    router: architect::LayerRouter,
    bind_addr: &str,
    static_dir: Option<&str>,
    info_tx: tokio::sync::oneshot::Sender<GatewayInfo>,
) -> eyre::Result<()> {
    let addr: SocketAddr = bind_addr.parse()?;

    let mut app = Router::new()
        .route("/ws", get(ws_handler))
        .with_state(router);

    let serving_web_app = static_dir.is_some();
    if let Some(dir) = static_dir {
        info!("Serving static files from: {}", dir);
        app = app.fallback_service(ServeDir::new(dir));
    }

    let listener = TcpListener::bind(addr).await?;
    let local_addr = listener.local_addr()?;
    let port = local_addr.port();
    let lan_ip = detect_lan_ip();

    let gw_info = GatewayInfo {
        port,
        lan_ip: lan_ip.clone(),
        serving_web_app,
    };
    if serving_web_app {
        info!("Session web app available at:");
        info!("  Local:   {}", gw_info.local_url());
        if let Some(url) = gw_info.network_url() {
            info!("  Network: {url}");
        }
    }
    info!("WebSocket gateway listening on ws://{}", local_addr);

    // Send info to the UI before entering serve loop
    let _ = info_tx.send(gw_info);

    axum::serve(listener, app).await?;
    Ok(())
}

async fn ws_handler(
    ws: WebSocketUpgrade,
    State(router): State<architect::LayerRouter>,
) -> impl IntoResponse {
    // One vox connection per upgrade; the shared LayerRouter dispatches
    // every service (Setlist, SetlistStream, Song) by method id.
    ws.on_upgrade(move |socket| async move {
        let router = router.clone();
        let acceptor = architect::axum_ws::lane_acceptor_fn(move |_req, connection| {
            connection.handle_with(router.clone());
            Ok(())
        });
        architect::axum_ws::serve(socket, acceptor).await;
    })
}

