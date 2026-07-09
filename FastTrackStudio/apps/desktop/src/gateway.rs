//! In-Process WebSocket Gateway
//!
//! Runs a WebSocket server in-process, allowing browsers to connect
//! to the desktop app and access session services via vox binary RPC.

use std::io;
use std::net::SocketAddr;
use std::sync::Arc;

use axum::extract::ws::{Message, WebSocket};
use axum::extract::{State, WebSocketUpgrade};
use axum::response::IntoResponse;
use axum::routing::get;
use axum::Router;
use session::{SetlistEvent, WebClientServiceClient};
use std::sync::OnceLock;
use tokio::net::TcpListener;
use tokio::sync::mpsc;
use tokio::task::JoinHandle;
use tower_http::services::ServeDir;
use tracing::{debug, info, warn};
use vox::{Backing, Caller, DriverReplySink, Handler, Link, LinkRx, LinkTx, ReplySink};

static WEB_CLIENT_REGISTRY: OnceLock<WebClientRegistry> = OnceLock::new();

pub fn web_client_registry() -> &'static WebClientRegistry {
    WEB_CLIENT_REGISTRY.get_or_init(WebClientRegistry::new)
}

// ============================================================================
// Remote connection (to the REAPER-hosted fts-extensions)
// ============================================================================

/// Late-bound handle to the vox `Caller` for the REAPER connection.
///
/// The gateway starts before REAPER is reachable, so browser RPC arrives
/// before there is anything to forward to. The connection task installs the
/// caller here once `session_cli::connection::connect` succeeds (and replaces
/// it on reconnect); the forwarding handler reads it per call.
pub type RemoteConn = Arc<std::sync::Mutex<Option<Caller>>>;

static REMOTE_CONN: OnceLock<RemoteConn> = OnceLock::new();

/// Shared handle to the REAPER connection. Cloning is cheap (`Arc`).
pub fn remote_conn() -> RemoteConn {
    REMOTE_CONN
        .get_or_init(|| Arc::new(std::sync::Mutex::new(None)))
        .clone()
}

/// A `Handler` that forwards every browser RPC over the shared vox connection
/// to the REAPER-hosted `fts-extensions` and relays the response back.
///
/// This is a generic request/response proxy: it re-borrows the incoming
/// call's payload and re-issues it on the remote caller, then relays the
/// remote response bytes verbatim. Push streams to browsers do not go through
/// here — those are delivered out-of-band via [`WebClientRegistry`].
#[derive(Clone)]
pub struct ForwardingHandler {
    remote: RemoteConn,
}

impl ForwardingHandler {
    pub fn new(remote: RemoteConn) -> Self {
        Self { remote }
    }
}

impl Handler<DriverReplySink> for ForwardingHandler {
    async fn handle(
        &self,
        call: vox::SelfRef<vox::RequestCall<'static>>,
        reply: DriverReplySink,
        _schemas: Arc<vox::SchemaRecvTracker>,
    ) {
        // Clone the caller out from under the std mutex so we never hold the
        // lock across an await.
        let remote = self.remote.lock().expect("remote conn poisoned").clone();
        let Some(remote) = remote else {
            // REAPER not connected yet — surface a retryable error to the browser.
            reply
                .send_error(vox::VoxError::<core::convert::Infallible>::ConnectionClosed)
                .await;
            return;
        };

        let incoming = call.get();
        let forwarded = vox::RequestCall {
            method_id: incoming.method_id,
            channels: incoming.channels.clone(),
            metadata: incoming.metadata.clone(),
            args: incoming.args.reborrow(),
            schemas: incoming.schemas.clone(),
        };

        match remote.call(forwarded).await {
            Ok(response) => {
                let response = response.value;
                let r = response.get();
                reply
                    .send_reply(vox::RequestResponse {
                        metadata: r.metadata.clone(),
                        ret: r.ret.reborrow(),
                        schemas: r.schemas.clone(),
                    })
                    .await;
            }
            Err(e) => reply.send_error(e).await,
        }
    }
}

// ============================================================================
// AxumWsLink
// ============================================================================

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
                    None => return,
                }
            }
            frame = ws.recv() => {
                match frame {
                    Some(Ok(msg)) => {
                        if tx_in.send(Ok(msg)).await.is_err() {
                            return;
                        }
                    }
                    Some(Err(e)) => {
                        let _ = tx_in.send(Err(io::Error::other(e.to_string()))).await;
                        return;
                    }
                    None => return,
                }
            }
        }
    }
}

// Tx

struct AxumWsLinkTx {
    tx: mpsc::Sender<Vec<u8>>,
    io_task: JoinHandle<()>,
}

impl LinkTx for AxumWsLinkTx {
    async fn send(&self, bytes: Vec<u8>) -> io::Result<()> {
        self.tx.send(bytes).await.map_err(|_| {
            io::Error::new(
                io::ErrorKind::ConnectionReset,
                "websocket writer task stopped",
            )
        })
    }

    async fn close(self) -> io::Result<()> {
        drop(self.tx);
        self.io_task.await.map_err(io::Error::other)
    }
}

// Rx

struct AxumWsLinkRx {
    rx: mpsc::Receiver<Result<Message, io::Error>>,
}

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
                        "text frames not allowed on vox websocket link",
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
// Web Client Registry
// ============================================================================

#[derive(Clone, Default)]
pub struct WebClientRegistry {
    clients: Arc<tokio::sync::Mutex<Vec<WebClientServiceClient>>>,
}

impl WebClientRegistry {
    pub fn new() -> Self {
        Self::default()
    }

    pub async fn register(&self, client: WebClientServiceClient) {
        self.clients.lock().await.push(client);
        debug!(
            "Web client registered ({} total)",
            self.clients.lock().await.len()
        );
    }

    pub async fn broadcast(&self, event: &SetlistEvent) {
        let clients = {
            let guard = self.clients.lock().await;
            if guard.is_empty() {
                return;
            }
            guard.clone()
        };

        let mut failed_indices = Vec::new();
        for (i, client) in clients.iter().enumerate() {
            if client.push_event(event.clone()).await.is_err() {
                failed_indices.push(i);
            }
        }

        if !failed_indices.is_empty() {
            let mut guard = self.clients.lock().await;
            for &i in failed_indices.iter().rev() {
                if i < guard.len() {
                    let _ = guard.swap_remove(i);
                }
            }
            debug!(
                "Pruned {} dead web clients ({} remaining)",
                failed_indices.len(),
                guard.len()
            );
        }
    }
}

// ============================================================================
// RoutedHandler
// ============================================================================

trait DynHandler: Send + Sync + 'static {
    fn handle(
        &self,
        call: vox::SelfRef<vox::RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: std::sync::Arc<vox::SchemaRecvTracker>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send + '_>>;
}

impl<H: Handler<DriverReplySink>> DynHandler for H {
    fn handle(
        &self,
        call: vox::SelfRef<vox::RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: std::sync::Arc<vox::SchemaRecvTracker>,
    ) -> std::pin::Pin<Box<dyn std::future::Future<Output = ()> + Send + '_>> {
        Box::pin(Handler::handle(self, call, reply, schemas))
    }
}

#[derive(Clone, Default)]
pub struct RoutedHandler {
    method_map: std::collections::HashMap<vox::MethodId, usize>,
    handlers: Vec<Arc<dyn DynHandler>>,
}

impl RoutedHandler {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn with<H: Handler<DriverReplySink>>(
        mut self,
        descriptor: &vox::ServiceDescriptor,
        handler: H,
    ) -> Self {
        let idx = self.handlers.len();
        self.handlers.push(Arc::new(handler));
        for method in descriptor.methods {
            self.method_map.insert(method.id, idx);
        }
        self
    }
}

impl Handler<DriverReplySink> for RoutedHandler {
    async fn handle(
        &self,
        call: vox::SelfRef<vox::RequestCall<'static>>,
        reply: DriverReplySink,
        schemas: std::sync::Arc<vox::SchemaRecvTracker>,
    ) {
        let method_id = call.get().method_id;
        if let Some(&idx) = self.method_map.get(&method_id) {
            self.handlers[idx].handle(call, reply, schemas).await;
        } else {
            reply
                .send_error(vox::VoxError::<core::convert::Infallible>::UnknownMethod)
                .await;
        }
    }
}

// ============================================================================
// Gateway Server
// ============================================================================

struct Gateway {
    handler: RoutedHandler,
}

#[derive(Clone, Default)]
pub struct GatewayInfo {
    pub port: u16,
    pub lan_ip: Option<String>,
    pub serving_web_app: bool,
}

impl GatewayInfo {
    pub fn local_url(&self) -> String {
        format!("http://localhost:{}", self.port)
    }

    pub fn network_url(&self) -> Option<String> {
        self.lan_ip
            .as_ref()
            .map(|ip| format!("http://{}:{}", ip, self.port))
    }
}

fn detect_lan_ip() -> Option<String> {
    let socket = std::net::UdpSocket::bind("0.0.0.0:0").ok()?;
    socket.connect("8.8.8.8:80").ok()?;
    let addr = socket.local_addr().ok()?;
    Some(addr.ip().to_string())
}

pub async fn start_gateway(
    handler: RoutedHandler,
    bind_addr: &str,
    static_dir: Option<&str>,
    info_tx: tokio::sync::oneshot::Sender<GatewayInfo>,
) -> eyre::Result<()> {
    let addr: SocketAddr = bind_addr.parse()?;
    let gateway = Arc::new(Gateway { handler });

    let mut app = Router::new()
        .route("/ws", get(ws_handler))
        .with_state(gateway.clone());

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
        lan_ip,
        serving_web_app,
    };
    if serving_web_app {
        info!("Web app available at:");
        info!("  Local:   {}", gw_info.local_url());
        if let Some(url) = gw_info.network_url() {
            info!("  Network: {url}");
        }
    }
    info!("WebSocket gateway listening on ws://{}", local_addr);

    let _ = info_tx.send(gw_info);

    axum::serve(listener, app).await?;
    Ok(())
}

async fn ws_handler(
    ws: WebSocketUpgrade,
    State(gateway): State<Arc<Gateway>>,
) -> impl IntoResponse {
    ws.on_upgrade(move |socket| handle_socket(socket, gateway))
}

/// Minimal `FromVoxLane` client that captures an accepted lane's `Caller` so
/// the desktop can push `SetlistEvent`s back to the web client (vox 0.10
/// lane model — replaces the removed root-session `NoopClient` caller).
#[derive(Clone)]
struct GatewayLaneClient {
    caller: vox::Caller,
}

impl vox::FromVoxLane for GatewayLaneClient {
    const SERVICE_NAME: &'static str = "fasttrackstudio-gateway";

    fn from_vox_lane(caller: vox::Caller, _connection: Option<vox::ConnectionHandle>) -> Self {
        Self { caller }
    }
}

async fn handle_socket(socket: WebSocket, gateway: Arc<Gateway>) {
    let link = AxumWsLink::new(socket);

    // vox 0.10 lane model: accept every inbound lane with the shared
    // RoutedHandler (it dispatches by method id) and capture the lane's
    // caller so setlist events can be pushed back to this web client.
    let handler = gateway.handler.clone();
    let lane_acceptor = vox::lane_acceptor_fn(move |_req, lane: vox::PendingLane| {
        let lane_client: GatewayLaneClient = lane.handle_with_client(handler.clone());
        let client = WebClientServiceClient::new(lane_client.caller);
        tokio::spawn(async move {
            web_client_registry().register(client).await;
        });
        Ok(())
    });

    match vox::acceptor_on(link)
        .on_lane(lane_acceptor)
        .establish_connection()
        .await
    {
        Ok(_connection) => {
            debug!("WebSocket client connected");
            // Keep the connection handle alive until the socket drops.
            std::future::pending::<()>().await;
        }
        Err(e) => warn!("WebSocket handshake failed: {:?}", e),
    }
}
