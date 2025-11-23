//! REAPER Extension RPC Server
//!
//! Implements the irpc server for the REAPER extension.
//! Streams state updates to clients and handles incoming commands.

use anyhow::{bail, Result};
use irpc::{
    channel::mpsc,
    Client, Request, WithChannels,
};
use iroh::protocol::Router;
use irpc_iroh::IrohProtocol;
use n0_future::task::{self, AbortOnDropHandle};
use peer_2_peer::{
    iroh_connection::{create_reaper_server, store_endpoint_id, REAPER_ALPN},
    reaper_api::{
        ClientRequest, Connect, ReaperMessage, ReaperProtocol, ReaperStateUpdate,
    },
};
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::time;
use tracing::{error, info, warn};

/// REAPER RPC Actor that handles client connections
pub struct ReaperActor {
    recv: mpsc::Receiver<ReaperMessage>,
    /// Channel to send state updates to all connected clients
    state_update_tx: Arc<Mutex<Option<mpsc::Sender<ReaperStateUpdate>>>>,
}

impl ReaperActor {
    /// Create a new REAPER actor and return the API
    pub fn local() -> ReaperApi {
        let (tx, rx) = mpsc::channel(128);
        let state_update_tx = Arc::new(Mutex::new(None));
        
        let actor = Self {
            recv: rx,
            state_update_tx: state_update_tx.clone(),
        };
        
        // Spawn the actor to handle messages
        task::spawn(actor.run());
        
        ReaperApi {
            inner: Client::local(tx),
            state_update_tx,
        }
    }

    async fn run(mut self) {
        let state_update_tx = self.state_update_tx.clone();
        while let Ok(Some(msg)) = self.recv.recv().await {
            let state_update_tx = state_update_tx.clone();
            task::spawn(async move {
                if let Err(e) = Self::handle(msg, state_update_tx).await {
                    error!("[REAPER RPC] Error handling message: {}", e);
                }
            });
        }
    }

    async fn handle(
        msg: ReaperMessage,
        state_update_tx: Arc<Mutex<Option<mpsc::Sender<ReaperStateUpdate>>>>,
    ) -> Result<()> {
        match msg {
            ReaperMessage::Connect(connect) => {
                info!("[REAPER RPC] Client connected, setting up bidirectional stream");
                let WithChannels {
                    rx: client_rx,
                    tx: state_tx,
                    span,
                    ..
                } = connect;
                let _entered = span.enter();
                
                // Store the state update sender so we can send updates
                {
                    let mut tx_guard = state_update_tx.lock().await;
                    *tx_guard = Some(state_tx.clone());
                }
                
                // Spawn task to handle client requests
                let state_update_tx_for_requests = state_tx.clone();
                let state_update_tx_for_clear = state_update_tx.clone();
                task::spawn(async move {
                    let mut receiver = client_rx;
                    while let Ok(Some(request)) = receiver.recv().await {
                        match request {
                            ClientRequest::Heartbeat => {
                                // Only log heartbeats once
                                info!("[REAPER RPC] 💓 Received heartbeat from client");
                                // Send heartbeat back (don't log sends)
                                if let Err(e) = state_update_tx_for_requests.send(ReaperStateUpdate::Heartbeat).await {
                                    warn!("[REAPER RPC] Failed to send heartbeat response: {}", e);
                                }
                            }
                            ClientRequest::GetSetlistState => {
                                // TODO: Get current setlist state and send it
                                info!("[REAPER RPC] Client requested setlist state");
                            }
                            ClientRequest::GetTransportState => {
                                // TODO: Get current transport state and send it
                                info!("[REAPER RPC] Client requested transport state");
                            }
                            ClientRequest::TransportCommand(cmd) => {
                                // Handle transport command
                                info!("[REAPER RPC] Client sent transport command: {:?}", cmd);
                                // TODO: Execute command via REAPER API
                            }
                        }
                    }
                    info!("[REAPER RPC] Client disconnected");
                    // Clear the state update sender so heartbeat task knows to stop
                    {
                        let mut tx_guard = state_update_tx_for_clear.lock().await;
                        *tx_guard = None;
                    }
                });
                
                // Spawn task to send periodic heartbeats from server to client at 30Hz
                let state_tx_for_heartbeat = state_tx.clone();
                let state_update_tx_for_heartbeat_check = state_update_tx.clone();
                task::spawn(async move {
                    let mut interval = time::interval(time::Duration::from_millis(33)); // ~30Hz
                    let mut tick_count = 0u64;
                    loop {
                        interval.tick().await;
                        
                        // Check if connection is still valid
                        let tx_guard = state_update_tx_for_heartbeat_check.lock().await;
                        if tx_guard.is_none() {
                            info!("[REAPER RPC] Client disconnected, stopping heartbeat");
                            break;
                        }
                        drop(tx_guard);
                        
                        tick_count += 1;
                        info!("[REAPER RPC] 💓 Sending heartbeat tick #{}", tick_count);
                        if let Err(e) = state_tx_for_heartbeat.send(ReaperStateUpdate::Heartbeat).await {
                            warn!("[REAPER RPC] Failed to send heartbeat: {}", e);
                            break; // Connection closed
                        }
                    }
                });
                
                // The state_tx will be used by the polling handler to send updates
                info!("[REAPER RPC] Bidirectional stream established");
            }
        }
        Ok(())
    }
}

/// REAPER RPC API
#[derive(Clone)]
pub struct ReaperApi {
    inner: Client<ReaperProtocol>,
    /// Channel to send state updates to connected clients
    state_update_tx: Arc<Mutex<Option<mpsc::Sender<ReaperStateUpdate>>>>,
}

impl ReaperApi {
    /// Get the local sender for this API (for building custom routers)
    pub fn get_local_sender(&self) -> Result<irpc::LocalSender<ReaperProtocol>> {
        let Some(local) = self.inner.as_local() else {
            bail!("cannot get local sender from a remote service");
        };
        Ok(local)
    }
    
    /// Create an IROH router for this API
    /// 
    /// This sets up the IROH endpoint with the REAPER protocol handler.
    /// Returns the router (which must be kept alive) and the endpoint ID.
    pub async fn create_iroh_router(&self) -> Result<(Router, iroh::EndpointId)> {
        let local = self.get_local_sender()?;
        create_reaper_server(local).await
    }

    /// Send a state update to all connected clients
    pub async fn send_state_update(&self, update: ReaperStateUpdate) -> Result<()> {
        let tx_guard = self.state_update_tx.lock().await;
        if let Some(ref tx) = *tx_guard {
            tx.send(update).await?;
        }
        Ok(())
    }

    /// Open a bidirectional connection (client-side)
    pub async fn connect(
        &self,
    ) -> Result<(mpsc::Sender<ClientRequest>, mpsc::Receiver<ReaperStateUpdate>)> {
        let msg = Connect;
        match self.inner.request().await? {
            Request::Local(request) => {
                let (client_tx, client_rx) = mpsc::channel(128);
                let (state_tx, state_rx) = mpsc::channel(128);
                request.send((msg, state_tx, client_rx)).await?;
                Ok((client_tx, state_rx))
            }
            Request::Remote(request) => {
                let (tx, rx) = request.write(msg).await?;
                Ok((tx.into(), rx.into()))
            }
        }
    }
}

/// Global REAPER API instance for sending state updates
/// This will be set when the server starts
static REAPER_API: std::sync::OnceLock<Arc<tokio::sync::Mutex<Option<ReaperApi>>>> = std::sync::OnceLock::new();

/// Set the global REAPER API instance
pub fn set_global_api(api: ReaperApi) {
    REAPER_API.get_or_init(|| Arc::new(tokio::sync::Mutex::new(None)));
    // Note: This is a simplified approach - in practice you might want to use a different pattern
}

/// Send a state update via the RPC server
pub async fn send_state_update(update: ReaperStateUpdate) -> Result<()> {
    if let Some(api_guard) = REAPER_API.get() {
        let api = api_guard.lock().await;
        if let Some(ref api) = *api {
            api.send_state_update(update).await?;
        }
    }
    Ok(())
}

/// Start the REAPER RPC server using IROH
/// 
/// Returns the API handle and router (which must be kept alive).
/// The endpoint ID is stored to a file for client discovery.
pub async fn start_reaper_rpc_server() -> Result<(ReaperApi, Router)> {
    let api = ReaperActor::local();
    
    // Store the API globally
    set_global_api(api.clone());
    
    // Create IROH router
    let (router, endpoint_id) = api.create_iroh_router().await?;
    
    // Store endpoint ID for client discovery
    if let Err(e) = store_endpoint_id(endpoint_id) {
        warn!("[REAPER RPC] Failed to store endpoint ID: {}", e);
    }
    
    info!("[REAPER RPC] Server started with IROH, endpoint ID: {}", endpoint_id);
    
    Ok((api, router))
}

