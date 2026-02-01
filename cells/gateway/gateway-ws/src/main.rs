//! Binary WebSocket Gateway Cell
//!
//! This cell provides a WebSocket server that bridges browser clients to roam services
//! using binary postcard encoding over WebSocket. This allows browsers to use the same
//! roam-websocket client as native apps, enabling shared UI code via daw-control API.
//!
//! Architecture:
//! - Browser connects via WebSocket and speaks binary roam protocol (postcard)
//! - Gateway uses ForwardingDispatcher to transparently forward all calls to host
//! - No schema knowledge needed at gateway - fully extensible
//!
//! The gateway can be suspended when a desktop app takes over, redirecting clients
//! to the desktop's gateway instead.

use std::io;
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use axum::extract::ws::{Message, WebSocket};
use axum::extract::{State, WebSocketUpgrade};
use axum::response::IntoResponse;
use axum::routing::get;
use axum::Router;
use cell_runtime::run_cell;
use futures_util::{SinkExt, StreamExt};
use gateway_proto::{
    GatewayControl, GatewayControlDispatcher, GatewayInfo, GatewayState, GatewayType,
};
use roam::session::ConnectionHandle;
use roam::Context;
use roam_session::{ForwardingDispatcher, MessageTransport};
use roam_stream::{accept_framed, HandshakeConfig};
use roam_wire::Message as RoamMessage;
use tokio::net::TcpListener;
use tokio::sync::RwLock;
use tower_http::services::ServeDir;
use tracing::{debug, info, warn};

// ============================================================================
// AxumWsTransport - Adapts axum WebSocket to MessageTransport
// ============================================================================

/// Adapter that implements MessageTransport for axum WebSocket.
///
/// This allows running a roam driver directly on an axum WebSocket connection,
/// enabling binary postcard-encoded RPC over WebSocket.
struct AxumWsTransport {
    sender: futures_util::stream::SplitSink<WebSocket, Message>,
    receiver: futures_util::stream::SplitStream<WebSocket>,
    last_decoded: Vec<u8>,
}

impl AxumWsTransport {
    fn new(socket: WebSocket) -> Self {
        let (sender, receiver) = socket.split();
        Self {
            sender,
            receiver,
            last_decoded: Vec::new(),
        }
    }
}

impl MessageTransport for AxumWsTransport {
    async fn send(&mut self, msg: &RoamMessage) -> io::Result<()> {
        let payload = facet_postcard::to_vec(msg)
            .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, e.to_string()))?;

        self.sender
            .send(Message::Binary(payload.into()))
            .await
            .map_err(|e| io::Error::other(format!("WebSocket send failed: {e}")))?;

        Ok(())
    }

    async fn recv_timeout(&mut self, timeout: Duration) -> io::Result<Option<RoamMessage>> {
        tokio::select! {
            result = self.recv() => result,
            _ = tokio::time::sleep(timeout) => Ok(None),
        }
    }

    async fn recv(&mut self) -> io::Result<Option<RoamMessage>> {
        loop {
            match self.receiver.next().await {
                Some(Ok(Message::Binary(data))) => {
                    self.last_decoded = data.to_vec();
                    let msg: RoamMessage =
                        facet_postcard::from_slice(&self.last_decoded).map_err(|e| {
                            io::Error::new(io::ErrorKind::InvalidData, format!("postcard: {e}"))
                        })?;
                    return Ok(Some(msg));
                }
                Some(Ok(Message::Text(text))) => {
                    // Treat text as binary (shouldn't happen for roam protocol)
                    self.last_decoded = text.as_bytes().to_vec();
                    let msg: RoamMessage =
                        facet_postcard::from_slice(&self.last_decoded).map_err(|e| {
                            io::Error::new(io::ErrorKind::InvalidData, format!("postcard: {e}"))
                        })?;
                    return Ok(Some(msg));
                }
                Some(Ok(Message::Close(_))) => {
                    return Ok(None);
                }
                Some(Ok(Message::Ping(_) | Message::Pong(_))) => {
                    // Ignore ping/pong, continue receiving
                    continue;
                }
                Some(Err(e)) => {
                    return Err(io::Error::other(format!("WebSocket error: {e}")));
                }
                None => {
                    // Stream ended
                    return Ok(None);
                }
            }
        }
    }

    fn last_decoded(&self) -> &[u8] {
        &self.last_decoded
    }
}

// ============================================================================
// WebSocket Gateway
// ============================================================================

/// WebSocket gateway state and configuration
struct WebSocketGateway {
    /// Current gateway state (Active or Suspended)
    state: RwLock<GatewayState>,
    /// Connection handle back to the host
    host_handle: Arc<std::sync::OnceLock<ConnectionHandle>>,
    /// Server bind address
    bind_addr: SocketAddr,
    /// Directory for static files (web app)
    static_dir: Option<String>,
}

impl WebSocketGateway {
    fn new(host_handle: Arc<std::sync::OnceLock<ConnectionHandle>>) -> Self {
        let bind_addr: SocketAddr = std::env::var("GATEWAY_WS_ADDR")
            .unwrap_or_else(|_| "0.0.0.0:3030".to_string())
            .parse()
            .expect("Invalid GATEWAY_WS_ADDR");

        let static_dir = std::env::var("GATEWAY_WS_STATIC_DIR").ok();

        Self {
            state: RwLock::new(GatewayState::Active),
            host_handle,
            bind_addr,
            static_dir,
        }
    }

    /// Run the HTTP/WebSocket server
    async fn run_server(self: Arc<Self>) -> eyre::Result<()> {
        // Wait for the host connection to be established
        let host_handle = loop {
            if let Some(h) = self.host_handle.get() {
                break h.clone();
            }
            tokio::time::sleep(Duration::from_millis(10)).await;
        };

        info!("Host connection established, starting binary WebSocket gateway");

        // Build the router with WebSocket endpoint
        let mut app = Router::new()
            .route("/ws", get(ws_handler))
            .with_state((self.clone(), host_handle));

        // Optionally serve static files for the web app
        if let Some(ref static_dir) = self.static_dir {
            info!("Serving static files from: {}", static_dir);
            app = app.fallback_service(ServeDir::new(static_dir));
        }

        // Start the server
        let listener = TcpListener::bind(self.bind_addr).await?;
        info!(
            "Binary WebSocket gateway listening on ws://{}",
            self.bind_addr
        );

        axum::serve(listener, app).await?;
        Ok(())
    }
}

/// WebSocket upgrade handler
async fn ws_handler(
    ws: WebSocketUpgrade,
    State((gateway, host_handle)): State<(Arc<WebSocketGateway>, ConnectionHandle)>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_socket(socket, gateway, host_handle))
}

/// Handle a WebSocket connection - runs roam RPC and forwards to host
async fn handle_socket(
    socket: WebSocket,
    gateway: Arc<WebSocketGateway>,
    host_handle: ConnectionHandle,
) {
    // Check if gateway is suspended
    if *gateway.state.read().await == GatewayState::Suspended {
        warn!("Connection rejected: gateway suspended");
        return;
    }

    debug!("WebSocket connection received, setting up binary RPC forwarding");

    // Wrap the axum WebSocket in our transport adapter
    let transport = AxumWsTransport::new(socket);

    // Create ForwardingDispatcher - forwards ALL calls to host transparently
    // No schema knowledge needed - just forwards raw bytes by method_id
    let dispatcher = ForwardingDispatcher::new(host_handle);

    // Accept the roam session with the forwarding dispatcher
    let config = HandshakeConfig::default();
    match accept_framed(transport, config, dispatcher).await {
        Ok((_browser_handle, _incoming, driver)) => {
            info!("Binary WebSocket client connected");
            // Run the driver until the connection closes
            if let Err(e) = driver.run().await {
                debug!("WebSocket session ended: {:?}", e);
            }
            debug!("WebSocket connection closed");
        }
        Err(e) => {
            warn!("WebSocket handshake failed: {:?}", e);
        }
    }
}

// ============================================================================
// GatewayControl implementation (host can suspend/resume this gateway)
// ============================================================================

/// Wrapper type to implement GatewayControl trait
#[derive(Clone)]
struct GatewayControlImpl(Arc<WebSocketGateway>);

impl GatewayControl for GatewayControlImpl {
    /// Suspend the gateway (e.g., when desktop app takes over)
    async fn suspend(&self, _cx: &Context, redirect_info: String) -> u32 {
        *self.0.state.write().await = GatewayState::Suspended;
        info!("Gateway suspended, redirect: {}", redirect_info);
        0
    }

    /// Resume the gateway (e.g., when desktop app disconnects)
    async fn resume(&self, _cx: &Context) {
        *self.0.state.write().await = GatewayState::Active;
        info!("Gateway resumed");
    }

    /// Get current gateway state
    async fn get_state(&self, _cx: &Context) -> GatewayInfo {
        GatewayInfo {
            gateway_type: GatewayType::WebSocket,
            state: *self.0.state.read().await,
            connection_count: 0,
        }
    }
}

// ============================================================================
// Entry point
// ============================================================================

fn main() -> Result<(), Box<dyn std::error::Error>> {
    run_cell!("gateway-ws", |handle| {
        let gateway = Arc::new(WebSocketGateway::new(handle));

        // Spawn the WebSocket server
        let gateway_for_server = gateway.clone();
        tokio::spawn(async move {
            if let Err(e) = gateway_for_server.run_server().await {
                warn!("Gateway server error: {}", e);
            }
        });

        // Return the dispatcher for GatewayControl (host can suspend/resume us)
        GatewayControlDispatcher::new(GatewayControlImpl(gateway))
    })
}
