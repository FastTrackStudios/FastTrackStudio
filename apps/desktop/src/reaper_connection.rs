//! REAPER extension connection via irpc over IROH
//!
//! Handles connection to the REAPER extension using IROH with MDNS discovery
//! and processes incoming state updates

use anyhow::Result;
use irpc::{
    channel::mpsc,
    Client, Request,
};
use irpc_iroh::client;
use iroh::EndpointId;
use peer_2_peer::{
    iroh_connection::{create_reaper_client, discover_reaper_endpoint, REAPER_ALPN},
    reaper_api::{
        ClientRequest, Connect, ReaperProtocol, ReaperStateUpdate, TransportCommand,
    },
};
use std::sync::Arc;
use tokio::sync::Mutex;
use tracing::{error, info, warn};

/// Callback type for handling REAPER state updates
pub type StateUpdateHandler = Arc<dyn Fn(ReaperStateUpdate) + Send + Sync>;

/// REAPER connection manager
#[derive(Clone)]
pub struct ReaperConnection {
    /// Channel for sending commands to REAPER
    command_sender: Arc<Mutex<Option<mpsc::Sender<ClientRequest>>>>,
    /// Handler for incoming state updates
    state_update_handler: Arc<Mutex<Option<StateUpdateHandler>>>,
    /// Connection status sender
    connected_tx: Arc<tokio::sync::watch::Sender<bool>>,
    /// Connection status receiver (for internal use)
    connected_rx: Arc<tokio::sync::watch::Receiver<bool>>,
}

/// Command sender for REAPER commands
pub struct ReaperCommandSender {
    sender: Arc<Mutex<Option<mpsc::Sender<ClientRequest>>>>,
}

impl ReaperCommandSender {
    /// Send a command to REAPER
    pub async fn send(&self, request: ClientRequest) -> Result<(), String> {
        let guard = self.sender.lock().await;
        if let Some(ref sender) = *guard {
            sender.send(request).await.map_err(|e| format!("Failed to send command: {}", e))?;
            Ok(())
        } else {
            Err("Not connected to REAPER".to_string())
        }
    }

    /// Send a transport command
    pub async fn send_transport_command(&self, cmd: TransportCommand) -> Result<(), String> {
        self.send(ClientRequest::TransportCommand(cmd)).await
    }
}

impl ReaperConnection {
    /// Create a new REAPER connection manager
    pub fn new() -> (Self, tokio::sync::watch::Receiver<bool>, ReaperCommandSender) {
        let (connected_tx, connected_rx) = tokio::sync::watch::channel(false);
        let command_sender = Arc::new(Mutex::new(None));
        
        let connection = Self {
            command_sender: command_sender.clone(),
            state_update_handler: Arc::new(Mutex::new(None)),
            connected_tx: Arc::new(connected_tx),
            connected_rx: Arc::new(connected_rx.clone()),
        };
        
        let command_sender_wrapper = ReaperCommandSender {
            sender: command_sender,
        };
        
        (connection, connected_rx, command_sender_wrapper)
    }

    /// Set the handler for state updates
    pub async fn set_state_update_handler(&self, handler: StateUpdateHandler) {
        let mut guard = self.state_update_handler.lock().await;
        *guard = Some(handler);
    }

    /// Start the connection to REAPER using IROH with automatic reconnection
    /// 
    /// Uses MDNS discovery to find the REAPER extension endpoint ID.
    /// Automatically retries connection with exponential backoff on failure.
    pub async fn start(&self) -> Result<()> {
        let self_clone = self.clone();
        tokio::spawn(async move {
            self_clone.connect_with_retry().await;
        });
        Ok(())
    }
    
    /// Connect with automatic retry logic
    async fn connect_with_retry(&self) {
        let mut retry_count = 0u32;
        
            loop {
            retry_count += 1;
            
            match self.try_connect().await {
                Ok(()) => {
                    info!("[DESKTOP] Successfully connected to REAPER");
                    retry_count = 0;
                    
                    // Wait for connection to close before retrying
                    // The connection tasks will update the status when they detect closure
                    let mut connected_rx = (*self.connected_rx).clone();
                    while connected_rx.changed().await.is_ok() {
                        if !*connected_rx.borrow() {
                            info!("[DESKTOP] Connection lost, will retry...");
                            break;
                        }
                    }
                }
                Err(e) => {
                    if retry_count % 10 == 0 {
                    warn!("[DESKTOP] Connection attempt {} failed: {}", retry_count, e);
                    }
                    
                    // Update connection status
                    self.connected_tx.send(false).ok();
                    
                    // Retry every 1 second
                    tokio::time::sleep(tokio::time::Duration::from_secs(1)).await;
                }
            }
        }
    }
    
    /// Attempt a single connection to REAPER
    async fn try_connect(&self) -> Result<()> {
        info!("[DESKTOP] Discovering REAPER extension via IROH...");
        
        // Discover REAPER endpoint ID
        let endpoint_id = discover_reaper_endpoint().await?
            .ok_or_else(|| anyhow::anyhow!("Could not discover REAPER extension. Make sure REAPER extension is running."))?;
        
        info!("[DESKTOP] Found REAPER endpoint ID: {}", endpoint_id);
        
        // Create IROH client endpoint
        let client_endpoint = create_reaper_client().await?;
        
        // Create IROH client
        let client: Client<ReaperProtocol> = client(client_endpoint, endpoint_id, REAPER_ALPN);
        
        // Open bidirectional connection with timeout
        let msg = Connect;
        let connection_timeout = tokio::time::Duration::from_secs(5); // 5 second timeout for initial connection
        let (command_tx, mut state_rx): (mpsc::Sender<ClientRequest>, mpsc::Receiver<ReaperStateUpdate>) = match tokio::time::timeout(connection_timeout, client.request()).await {
            Ok(Ok(Request::Remote(request))) => {
                // Also timeout the write operation
                match tokio::time::timeout(connection_timeout, request.write(msg)).await {
                    Ok(Ok((tx, rx))) => (tx.into(), rx.into()),
                    Ok(Err(e)) => return Err(anyhow::anyhow!("Failed to write connection message: {}", e)),
                    Err(_) => return Err(anyhow::anyhow!("Timeout writing connection message after {:?}", connection_timeout)),
                }
            }
            Ok(Ok(Request::Local(_))) => {
                return Err(anyhow::anyhow!("Expected remote connection, got local"));
            }
            Ok(Err(e)) => {
                return Err(anyhow::anyhow!("Connection request failed: {}", e));
            }
            Err(_) => {
                return Err(anyhow::anyhow!("Connection timeout after {:?} - REAPER extension may not be running or network issue", connection_timeout));
            }
        };
        
        // Store command sender
        {
            let mut guard = self.command_sender.lock().await;
            *guard = Some(command_tx);
        }
        
        // Update connection status
        self.connected_tx.send(true).ok();
        info!("[DESKTOP] Connected to REAPER RPC server");
        
        // Spawn task to receive state updates with fast connection health checking
        let handler = self.state_update_handler.clone();
        let connected_tx_for_updates = self.connected_tx.clone();
        tokio::spawn(async move {
            info!("[DESKTOP] Starting state update receive loop");
            let mut update_count = 0u64;
            let health_check_interval = tokio::time::Duration::from_millis(33); // ~30Hz for checking
            // Connection timeout: 1 second - should receive data at 30Hz so this is reasonable
            let connection_timeout = tokio::time::Duration::from_secs(1);
            
            // Shared last received time for health checking
            let last_received = Arc::new(Mutex::new(tokio::time::Instant::now()));
            
            // Spawn a health check task that runs at 30Hz to check connection status
            // This doesn't require sending data - it just checks if we've received data recently
            let connected_tx_for_health = connected_tx_for_updates.clone();
            let last_received_for_health = last_received.clone();
            tokio::spawn(async move {
                let mut interval = tokio::time::interval(health_check_interval);
                loop {
                    interval.tick().await;
                    // Check if we haven't received anything recently
                    let guard = last_received_for_health.lock().await;
                    if guard.elapsed() > connection_timeout {
                        warn!("[DESKTOP] Connection health check failed - no data received for {:?}", guard.elapsed());
                        connected_tx_for_health.send(false).ok();
                        break;
                    }
                }
            });
            
            loop {
                // Use timeout to detect if connection is dead
                // This will timeout after 35s if no data is received, which is longer than heartbeat interval
                match tokio::time::timeout(connection_timeout, state_rx.recv()).await {
                    Ok(Ok(Some(update))) => {
                        // Update last received time
                        {
                            let mut guard = last_received.lock().await;
                            *guard = tokio::time::Instant::now();
                                        }
                        update_count += 1;
                        
                        match &update {
                            ReaperStateUpdate::Heartbeat => {
                                info!("[DESKTOP] 💓 Received heartbeat #{} from REAPER", update_count);
                                        }
                                        _ => {
                                info!("[DESKTOP] Received state update #{}: {:?}", update_count, std::mem::discriminant(&update));
                                        }
                                    }
                                    
                                    // Call handler if set
                        let handler_guard = handler.lock().await;
                        if let Some(ref handler_fn) = *handler_guard {
                            handler_fn(update);
                                    } else {
                            warn!("[DESKTOP] No handler set for state update");
                        }
                    }
                    Ok(Ok(None)) => {
                        // Channel closed
                        info!("[DESKTOP] State update stream closed (received {} updates)", update_count);
                        connected_tx_for_updates.send(false).ok();
                        break;
                    }
                    Ok(Err(_)) => {
                        // Receive error
                        warn!("[DESKTOP] Error receiving state update");
                        connected_tx_for_updates.send(false).ok();
                        break;
                    }
                    Err(_) => {
                        // Timeout - connection appears dead
                        warn!("[DESKTOP] Connection timeout - no data received for {:?}", connection_timeout);
                        connected_tx_for_updates.send(false).ok();
                        break;
                    }
                }
            }
        });
        
        // Spawn task to send periodic heartbeats from client to server at 30Hz
        let command_sender_for_heartbeat = self.command_sender.clone();
        let connected_for_heartbeat = self.connected_tx.clone();
        let connected_rx_for_heartbeat = self.connected_rx.clone();
        tokio::spawn(async move {
            let mut interval = tokio::time::interval(tokio::time::Duration::from_millis(33)); // ~30Hz
            let mut tick_count = 0u64;
            let mut connected_rx = (*connected_rx_for_heartbeat).clone();
            
            loop {
                tokio::select! {
                    _ = interval.tick() => {
                        // Check connection status before sending
                        if !*connected_rx.borrow() {
                            info!("[DESKTOP] Connection lost, stopping heartbeat");
                                    break;
                                }
                        
                        tick_count += 1;
                        let guard = command_sender_for_heartbeat.lock().await;
                        if let Some(ref sender) = *guard {
                            info!("[DESKTOP] 💓 Sending heartbeat tick #{}", tick_count);
                            if let Err(e) = sender.send(ClientRequest::Heartbeat).await {
                                warn!("[DESKTOP] Failed to send heartbeat: {}", e);
                                connected_for_heartbeat.send(false).ok();
                                break; // Connection closed
                            }
                        } else {
                            warn!("[DESKTOP] Not connected, stopping heartbeat");
                            connected_for_heartbeat.send(false).ok();
                            break;
                        }
                    }
                    result = connected_rx.changed() => {
                        // Connection status changed
                        if result.is_err() || !*connected_rx.borrow() {
                            info!("[DESKTOP] Connection status changed to disconnected, stopping heartbeat");
                            break;
                        }
                    }
                }
            }
        });
        
        Ok(())
    }
    }
    
/// Default handler for state updates (logs them)
fn handle_state_update_default(update: ReaperStateUpdate) {
    match update {
        ReaperStateUpdate::Heartbeat => {
            info!("[DESKTOP] 💓 Heartbeat received");
        }
        // SetlistState is no longer used - setlist is now streamed via SETLIST global signal
        ReaperStateUpdate::SetlistState(_) => {
            // Ignored - setlist is now handled via SETLIST global signal
        }
        ReaperStateUpdate::TransportState(state) => {
            info!("[DESKTOP] TransportState update - playing: {}, position: {:.2}s", 
                state.is_playing, state.position_seconds);
        }
    }
}
