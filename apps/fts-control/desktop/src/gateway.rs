//! In-Process WebSocket Gateway
//!
//! Runs a WebSocket server in-process, allowing browsers to connect
//! to the desktop app and access session services via roam binary RPC.
//!
//! # Architecture
//!
//! ```text
//!     Browser ──WebSocket──► axum server ──► roam accept_framed ──► LocalServices
//! ```

use std::io;
use std::net::SocketAddr;
use std::sync::Arc;
use std::time::Duration;

use axum::Router;
use axum::extract::ws::{Message, WebSocket};
use axum::extract::{State, WebSocketUpgrade};
use axum::response::IntoResponse;
use axum::routing::get;
use futures_util::{SinkExt, StreamExt};
use roam::session::ServiceDispatcher;
use roam_session::MessageTransport;
use roam_stream::{HandshakeConfig, accept_framed};
use roam_wire::Message as RoamMessage;
use tokio::net::TcpListener;
use tokio::sync::RwLock;
use tower_http::services::ServeDir;
use tracing::{debug, info, warn};

// ============================================================================
// AxumWsTransport
// ============================================================================

struct AxumWsTransport {
    sender: futures_util::stream::SplitSink<WebSocket, Message>,
    receiver: futures_util::stream::SplitStream<WebSocket>,
    last_decoded: Vec<u8>,
}

impl AxumWsTransport {
    fn new(socket: WebSocket) -> Self {
        let (sender, receiver) = socket.split();
        Self { sender, receiver, last_decoded: Vec::new() }
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
                    let msg: RoamMessage = facet_postcard::from_slice(&self.last_decoded)
                        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("postcard: {e}")))?;
                    return Ok(Some(msg));
                }
                Some(Ok(Message::Text(text))) => {
                    self.last_decoded = text.as_bytes().to_vec();
                    let msg: RoamMessage = facet_postcard::from_slice(&self.last_decoded)
                        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("postcard: {e}")))?;
                    return Ok(Some(msg));
                }
                Some(Ok(Message::Close(_))) => return Ok(None),
                Some(Ok(Message::Ping(_) | Message::Pong(_))) => continue,
                Some(Err(e)) => {
                    return Err(io::Error::other(format!("WebSocket error: {e}")));
                }
                None => return Ok(None),
            }
        }
    }

    fn last_decoded(&self) -> &[u8] {
        &self.last_decoded
    }
}

// ============================================================================
// StandaloneGateway
// ============================================================================

#[derive(Clone, Copy, PartialEq, Eq)]
enum GatewayState {
    Active,
    Suspended,
}

struct StandaloneGateway<D: ServiceDispatcher + Clone + Send + Sync + 'static> {
    dispatcher: D,
    state: Arc<RwLock<GatewayState>>,
    static_dir: Option<String>,
}

impl<D: ServiceDispatcher + Clone + Send + Sync + 'static> StandaloneGateway<D> {
    fn new(dispatcher: D) -> Self {
        let static_dir = std::env::var("GATEWAY_WS_STATIC_DIR").ok();
        Self { dispatcher, state: Arc::new(RwLock::new(GatewayState::Active)), static_dir }
    }

    fn with_static_dir(mut self, dir: impl Into<String>) -> Self {
        self.static_dir = Some(dir.into());
        self
    }

    async fn run(self, bind_addr: &str) -> eyre::Result<()> {
        let addr: SocketAddr = bind_addr.parse()?;
        let gateway = Arc::new(self);

        let mut app = Router::new()
            .route("/ws", get(ws_handler::<D>))
            .with_state(gateway.clone());

        if let Some(ref static_dir) = gateway.static_dir {
            info!("Serving static files from: {}", static_dir);
            app = app.fallback_service(ServeDir::new(static_dir));
        }

        let listener = TcpListener::bind(addr).await?;
        debug!("WebSocket gateway listening on ws://{}", addr);
        axum::serve(listener, app).await?;
        Ok(())
    }
}

async fn ws_handler<D: ServiceDispatcher + Clone + Send + Sync + 'static>(
    ws: WebSocketUpgrade,
    State(gateway): State<Arc<StandaloneGateway<D>>>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_socket(socket, gateway))
}

async fn handle_socket<D: ServiceDispatcher + Clone + Send + Sync + 'static>(
    socket: WebSocket,
    gateway: Arc<StandaloneGateway<D>>,
) {
    if *gateway.state.read().await == GatewayState::Suspended {
        warn!("Connection rejected: gateway suspended");
        return;
    }

    let transport = AxumWsTransport::new(socket);
    let config = HandshakeConfig {
        max_payload_size: 1024 * 1024,
        initial_channel_credit: 16 * 1024 * 1024,
        max_concurrent_requests: 64,
    };
    match accept_framed(transport, config, gateway.dispatcher.clone()).await {
        Ok((_handle, _incoming, driver)) => {
            debug!("WebSocket client connected");
            if let Err(e) = driver.run().await {
                debug!("WebSocket session ended: {:?}", e);
            }
        }
        Err(e) => warn!("WebSocket handshake failed: {:?}", e),
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Start the WebSocket gateway server.
pub async fn start_gateway<D>(
    dispatcher: D,
    bind_addr: &str,
    static_dir: Option<&str>,
) -> eyre::Result<()>
where
    D: ServiceDispatcher + Clone + Send + Sync + 'static,
{
    debug!("Starting WebSocket gateway on {}", bind_addr);
    let mut gateway = StandaloneGateway::new(dispatcher);
    if let Some(dir) = static_dir {
        gateway = gateway.with_static_dir(dir);
    }
    gateway.run(bind_addr).await
}

/// Gateway configuration for the desktop app.
#[derive(Clone, Debug)]
pub struct GatewayConfig {
    pub bind_addr: String,
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
