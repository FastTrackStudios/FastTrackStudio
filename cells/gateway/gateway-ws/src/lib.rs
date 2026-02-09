//! WebSocket Gateway Library
//!
//! This crate provides a WebSocket server that bridges browser clients to roam services
//! using binary postcard encoding. Can be used:
//! - As a cell (via `main.rs` and `run_cell!` macro)
//! - In-process (by importing and running `WebSocketGateway` directly)
//!
//! # In-Process Usage
//!
//! ```rust,ignore
//! use gateway_ws::WebSocketGateway;
//! use roam::session::ServiceDispatcher;
//!
//! // Create gateway with a dispatcher
//! let gateway = WebSocketGateway::new_standalone();
//!
//! // Run the server
//! gateway.run_with_dispatcher(my_dispatcher, "0.0.0.0:3030").await?;
//! ```

use std::io;
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use axum::extract::ws::{Message, WebSocket};
use axum::extract::{State, WebSocketUpgrade};
use axum::response::IntoResponse;
use axum::routing::get;
use axum::Router;
use futures_util::{SinkExt, StreamExt};
use roam::session::ServiceDispatcher;
use roam_session::MessageTransport;
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
pub struct AxumWsTransport {
    sender: futures_util::stream::SplitSink<WebSocket, Message>,
    receiver: futures_util::stream::SplitStream<WebSocket>,
    last_decoded: Vec<u8>,
}

impl AxumWsTransport {
    pub fn new(socket: WebSocket) -> Self {
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
                    debug!(
                        "AxumWsTransport: received binary message, len={}",
                        data.len()
                    );
                    self.last_decoded = data.to_vec();
                    let msg: RoamMessage =
                        facet_postcard::from_slice(&self.last_decoded).map_err(|e| {
                            io::Error::new(io::ErrorKind::InvalidData, format!("postcard: {e}"))
                        })?;
                    debug!("AxumWsTransport: decoded RoamMessage: {:?}", msg);
                    return Ok(Some(msg));
                }
                Some(Ok(Message::Text(text))) => {
                    debug!(
                        "AxumWsTransport: received text message (unexpected), len={}",
                        text.len()
                    );
                    // Treat text as binary (shouldn't happen for roam protocol)
                    self.last_decoded = text.as_bytes().to_vec();
                    let msg: RoamMessage =
                        facet_postcard::from_slice(&self.last_decoded).map_err(|e| {
                            io::Error::new(io::ErrorKind::InvalidData, format!("postcard: {e}"))
                        })?;
                    return Ok(Some(msg));
                }
                Some(Ok(Message::Close(frame))) => {
                    debug!("AxumWsTransport: received close frame: {:?}", frame);
                    return Ok(None);
                }
                Some(Ok(Message::Ping(_) | Message::Pong(_))) => {
                    // Ignore ping/pong, continue receiving
                    continue;
                }
                Some(Err(e)) => {
                    debug!("AxumWsTransport: WebSocket error: {}", e);
                    return Err(io::Error::other(format!("WebSocket error: {e}")));
                }
                None => {
                    debug!("AxumWsTransport: stream ended");
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
// Standalone WebSocket Gateway (for in-process use)
// ============================================================================

/// Gateway state for standalone mode
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum GatewayState {
    Active,
    Suspended,
}

/// Standalone WebSocket gateway that forwards to a local dispatcher.
///
/// Unlike the cell-based gateway that forwards to a host via `ConnectionHandle`,
/// this version takes a `ServiceDispatcher` directly for in-process use.
pub struct StandaloneGateway<D: ServiceDispatcher + Clone + Send + Sync + 'static> {
    dispatcher: D,
    state: Arc<RwLock<GatewayState>>,
    static_dir: Option<String>,
}

impl<D: ServiceDispatcher + Clone + Send + Sync + 'static> StandaloneGateway<D> {
    /// Create a new standalone gateway with the given dispatcher.
    pub fn new(dispatcher: D) -> Self {
        let static_dir = std::env::var("GATEWAY_WS_STATIC_DIR").ok();
        Self {
            dispatcher,
            state: Arc::new(RwLock::new(GatewayState::Active)),
            static_dir,
        }
    }

    /// Create with a custom static file directory.
    pub fn with_static_dir(mut self, dir: impl Into<String>) -> Self {
        self.static_dir = Some(dir.into());
        self
    }

    /// Run the WebSocket server on the specified address.
    pub async fn run(self, bind_addr: &str) -> eyre::Result<()> {
        let addr: SocketAddr = bind_addr.parse()?;
        let gateway = Arc::new(self);

        // Build the router
        let mut app = Router::new()
            .route("/ws", get(standalone_ws_handler::<D>))
            .with_state(gateway.clone());

        // Optionally serve static files
        if let Some(ref static_dir) = gateway.static_dir {
            info!("Serving static files from: {}", static_dir);
            app = app.fallback_service(ServeDir::new(static_dir));
        }

        // Start the server
        let listener = TcpListener::bind(addr).await?;
        info!("Standalone WebSocket gateway listening on ws://{}", addr);

        axum::serve(listener, app).await?;
        Ok(())
    }

    /// Suspend the gateway (stops accepting new connections).
    pub async fn suspend(&self) {
        *self.state.write().await = GatewayState::Suspended;
        info!("Gateway suspended");
    }

    /// Resume the gateway.
    pub async fn resume(&self) {
        *self.state.write().await = GatewayState::Active;
        info!("Gateway resumed");
    }

    /// Check if gateway is active.
    pub async fn is_active(&self) -> bool {
        *self.state.read().await == GatewayState::Active
    }
}

/// WebSocket handler for standalone gateway.
async fn standalone_ws_handler<D: ServiceDispatcher + Clone + Send + Sync + 'static>(
    ws: WebSocketUpgrade,
    State(gateway): State<Arc<StandaloneGateway<D>>>,
) -> impl IntoResponse {
    // Don't require any subprotocol - Safari mobile is strict about protocol negotiation
    ws.on_upgrade(move |socket| standalone_handle_socket(socket, gateway))
}

/// Handle a WebSocket connection in standalone mode.
async fn standalone_handle_socket<D: ServiceDispatcher + Clone + Send + Sync + 'static>(
    socket: WebSocket,
    gateway: Arc<StandaloneGateway<D>>,
) {
    // Check if gateway is suspended
    if !gateway.is_active().await {
        warn!("Connection rejected: gateway suspended");
        return;
    }

    debug!("WebSocket connection received, setting up RPC");

    // Wrap the WebSocket in our transport adapter
    let transport = AxumWsTransport::new(socket);

    // Accept the roam session with the dispatcher
    // Use higher credit for 60Hz streaming updates
    let config = HandshakeConfig {
        max_payload_size: 1024 * 1024,            // 1 MiB
        initial_channel_credit: 16 * 1024 * 1024, // 16 MiB for high-frequency streaming
        max_concurrent_requests: 64,
    };
    match accept_framed(transport, config, gateway.dispatcher.clone()).await {
        Ok((_handle, _incoming, driver)) => {
            debug!("WebSocket client connected to standalone gateway");
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
// Re-exports for cell-based usage
// ============================================================================

pub use gateway_proto::{GatewayControl, GatewayInfo, GatewayType};
