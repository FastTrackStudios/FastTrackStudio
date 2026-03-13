//! In-Process WebSocket Gateway
//!
//! Runs a WebSocket server in-process, allowing browsers to connect
//! to the desktop app and access session services via roam binary RPC.
//!
//! # Architecture
//!
//! ```text
//!     Browser ──WebSocket──► axum server ──► roam acceptor ──► Handler
//! ```

use std::io;
use std::net::SocketAddr;
use std::sync::Arc;

use axum::Router;
use axum::extract::ws::{Message, WebSocket};
use axum::extract::{State, WebSocketUpgrade};
use axum::response::IntoResponse;
use axum::routing::get;
use roam::{Backing, DriverCaller, DriverReplySink, Handler, Link, LinkRx, LinkTx, LinkTxPermit, WriteSlot};
use tokio::net::TcpListener;
use tokio::sync::{RwLock, mpsc};
use tokio::task::JoinHandle;
use tower_http::services::ServeDir;
use tracing::{debug, info, warn};

// ============================================================================
// AxumWsLink — implements roam's Link trait for axum WebSocket
// ============================================================================

/// A [`Link`] implementation over an axum [`WebSocket`] connection.
///
/// Each roam payload maps 1:1 to a binary WebSocket frame. The WebSocket
/// protocol preserves message boundaries natively, so no length-prefix
/// framing is needed.
struct AxumWsLink {
    socket: WebSocket,
}

impl AxumWsLink {
    fn new(socket: WebSocket) -> Self {
        Self { socket }
    }
}

impl Link for AxumWsLink {
    type Tx = AxumWsLinkTx;
    type Rx = AxumWsLinkRx;

    fn split(self) -> (Self::Tx, Self::Rx) {
        let (tx_out, rx_out) = mpsc::channel::<Vec<u8>>(1);
        let (tx_in, rx_in) = mpsc::channel::<Result<Message, io::Error>>(1);

        let io_task = tokio::spawn(axum_ws_io_loop(self.socket, rx_out, tx_in));

        (
            AxumWsLinkTx {
                tx: tx_out,
                io_task,
            },
            AxumWsLinkRx { rx: rx_in },
        )
    }
}

/// Background I/O task that owns the axum WebSocket.
///
/// Multiplexes outbound writes (from the mpsc channel) with inbound reads
/// (forwarded to the read channel).
async fn axum_ws_io_loop(
    mut ws: WebSocket,
    mut rx_out: mpsc::Receiver<Vec<u8>>,
    tx_in: mpsc::Sender<Result<Message, io::Error>>,
) {
    loop {
        tokio::select! {
            msg = rx_out.recv() => {
                match msg {
                    Some(bytes) => {
                        if let Err(e) = ws.send(Message::Binary(bytes.into())).await {
                            let _ = tx_in.send(Err(io::Error::other(e.to_string()))).await;
                            return;
                        }
                    }
                    None => {
                        // Write channel closed — drop the WebSocket.
                        return;
                    }
                }
            }
            frame = ws.recv() => {
                match frame {
                    Some(Ok(msg)) => {
                        if tx_in.send(Ok(msg)).await.is_err() {
                            // Reader dropped — shut down.
                            return;
                        }
                    }
                    Some(Err(e)) => {
                        let _ = tx_in.send(Err(io::Error::other(e.to_string()))).await;
                        return;
                    }
                    None => {
                        // WebSocket stream ended.
                        return;
                    }
                }
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tx
// ---------------------------------------------------------------------------

/// Sending half of an [`AxumWsLink`].
struct AxumWsLinkTx {
    tx: mpsc::Sender<Vec<u8>>,
    io_task: JoinHandle<()>,
}

/// Permit for sending one payload through an [`AxumWsLinkTx`].
struct AxumWsLinkTxPermit {
    permit: mpsc::OwnedPermit<Vec<u8>>,
}

/// Write slot for [`AxumWsLinkTx`].
struct AxumWsWriteSlot {
    buf: Vec<u8>,
    permit: mpsc::OwnedPermit<Vec<u8>>,
}

impl LinkTx for AxumWsLinkTx {
    type Permit = AxumWsLinkTxPermit;

    async fn reserve(&self) -> io::Result<Self::Permit> {
        let permit = self.tx.clone().reserve_owned().await.map_err(|_| {
            io::Error::new(
                io::ErrorKind::ConnectionReset,
                "websocket writer task stopped",
            )
        })?;
        Ok(AxumWsLinkTxPermit { permit })
    }

    async fn close(self) -> io::Result<()> {
        drop(self.tx);
        self.io_task.await.map_err(io::Error::other)
    }
}

impl LinkTxPermit for AxumWsLinkTxPermit {
    type Slot = AxumWsWriteSlot;

    fn alloc(self, len: usize) -> io::Result<Self::Slot> {
        Ok(AxumWsWriteSlot {
            buf: vec![0u8; len],
            permit: self.permit,
        })
    }
}

impl WriteSlot for AxumWsWriteSlot {
    fn as_mut_slice(&mut self) -> &mut [u8] {
        &mut self.buf
    }

    fn commit(self) {
        drop(self.permit.send(self.buf));
    }
}

// ---------------------------------------------------------------------------
// Rx
// ---------------------------------------------------------------------------

/// Receiving half of an [`AxumWsLink`].
struct AxumWsLinkRx {
    rx: mpsc::Receiver<Result<Message, io::Error>>,
}

/// Error type for [`AxumWsLinkRx`].
#[derive(Debug)]
struct AxumWsLinkRxError(io::Error);

impl std::fmt::Display for AxumWsLinkRxError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "axum websocket rx: {}", self.0)
    }
}

impl std::error::Error for AxumWsLinkRxError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.0)
    }
}

impl LinkRx for AxumWsLinkRx {
    type Error = AxumWsLinkRxError;

    async fn recv(&mut self) -> Result<Option<Backing>, Self::Error> {
        loop {
            match self.rx.recv().await {
                Some(Ok(Message::Binary(data))) => {
                    return Ok(Some(Backing::Boxed(Vec::from(data).into_boxed_slice())));
                }
                Some(Ok(Message::Close(_))) | None => {
                    return Ok(None);
                }
                Some(Ok(Message::Ping(_) | Message::Pong(_))) => {
                    continue;
                }
                Some(Ok(Message::Text(_))) => {
                    return Err(AxumWsLinkRxError(io::Error::new(
                        io::ErrorKind::InvalidData,
                        "text frames not allowed on roam websocket link",
                    )));
                }
                Some(Err(e)) => {
                    return Err(AxumWsLinkRxError(e));
                }
            }
        }
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

struct StandaloneGateway<H: Handler<DriverReplySink> + Clone + Send + Sync + 'static> {
    handler: H,
    state: Arc<RwLock<GatewayState>>,
    static_dir: Option<String>,
}

impl<H: Handler<DriverReplySink> + Clone + Send + Sync + 'static> StandaloneGateway<H> {
    fn new(handler: H) -> Self {
        let static_dir = std::env::var("GATEWAY_WS_STATIC_DIR").ok();
        Self { handler, state: Arc::new(RwLock::new(GatewayState::Active)), static_dir }
    }

    fn with_static_dir(mut self, dir: impl Into<String>) -> Self {
        self.static_dir = Some(dir.into());
        self
    }

    async fn run(self, bind_addr: &str) -> eyre::Result<()> {
        let addr: SocketAddr = bind_addr.parse()?;
        let gateway = Arc::new(self);

        let mut app = Router::new()
            .route("/ws", get(ws_handler::<H>))
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

async fn ws_handler<H: Handler<DriverReplySink> + Clone + Send + Sync + 'static>(
    ws: WebSocketUpgrade,
    State(gateway): State<Arc<StandaloneGateway<H>>>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_socket(socket, gateway))
}

async fn handle_socket<H: Handler<DriverReplySink> + Clone + Send + Sync + 'static>(
    socket: WebSocket,
    gateway: Arc<StandaloneGateway<H>>,
) {
    if *gateway.state.read().await == GatewayState::Suspended {
        warn!("Connection rejected: gateway suspended");
        return;
    }

    let link = AxumWsLink::new(socket);
    match roam::acceptor(link)
        .max_concurrent_requests(64)
        .establish::<DriverCaller>(gateway.handler.clone())
        .await
    {
        Ok((_caller, _session_handle)) => {
            debug!("WebSocket client connected, session established");
            // Session runs in background (spawned by establish).
            // Keep the handler alive until the connection drops.
        }
        Err(e) => warn!("WebSocket handshake failed: {:?}", e),
    }
}

// ============================================================================
// Public API
// ============================================================================

/// Start the WebSocket gateway server.
pub async fn start_gateway<H>(
    handler: H,
    bind_addr: &str,
    static_dir: Option<&str>,
) -> eyre::Result<()>
where
    H: Handler<DriverReplySink> + Clone + Send + Sync + 'static,
{
    debug!("Starting WebSocket gateway on {}", bind_addr);
    let mut gateway = StandaloneGateway::new(handler);
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
