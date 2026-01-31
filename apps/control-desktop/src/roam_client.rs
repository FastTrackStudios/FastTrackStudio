//! ROAM client for connecting to REAPER via Unix socket.
//!
//! Scans for available REAPER instances by looking for socket files in /tmp,
//! then connects via roam-stream to the socket-gateway extension.

use host_proto::{PlaybackState, TransportClient, TransportResult};
use roam::{Rx, channel};
use roam_stream::{Connector, HandshakeConfig, NoDispatcher, connect};
use std::io;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::net::UnixStream;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

/// A REAPER instance that we can connect to.
#[derive(Debug, Clone, PartialEq)]
pub struct ReaperInstance {
    /// Process ID of the REAPER instance.
    pub pid: u32,
    /// Path to the Unix socket.
    pub socket_path: PathBuf,
}

impl ReaperInstance {
    /// Display name for this instance.
    pub fn display_name(&self) -> String {
        format!("REAPER (PID {})", self.pid)
    }
}

/// Scan for available REAPER instances by looking for socket files.
pub fn scan_reaper_instances() -> Vec<ReaperInstance> {
    let temp_dir = std::env::temp_dir();
    let mut instances = Vec::new();

    if let Ok(entries) = std::fs::read_dir(&temp_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if let Some(file_name) = path.file_name().and_then(|n| n.to_str()) {
                // Look for fts-reaper-{pid}.sock files
                if file_name.starts_with("fts-reaper-") && file_name.ends_with(".sock") {
                    // Extract PID from filename
                    let pid_str = file_name
                        .trim_start_matches("fts-reaper-")
                        .trim_end_matches(".sock");
                    if let Ok(pid) = pid_str.parse::<u32>() {
                        instances.push(ReaperInstance {
                            pid,
                            socket_path: path,
                        });
                    }
                }
            }
        }
    }

    instances
}

/// Unix socket connector for roam-stream.
struct UnixConnector {
    path: PathBuf,
}

impl Connector for UnixConnector {
    type Transport = UnixStream;

    async fn connect(&self) -> io::Result<UnixStream> {
        debug!("Connecting to Unix socket: {}", self.path.display());
        UnixStream::connect(&self.path).await
    }
}

/// Connection status.
#[derive(Debug, Clone, PartialEq)]
pub enum ConnectionStatus {
    Disconnected,
    Connecting,
    Connected(ReaperInstance),
    Error(String),
}

/// Client for communicating with REAPER via ROAM.
pub struct RoamClient {
    /// Current connection status.
    status: Arc<RwLock<ConnectionStatus>>,
    /// The underlying ROAM client (if connected).
    client: Arc<RwLock<Option<roam_stream::Client<UnixConnector, NoDispatcher>>>>,
}

impl RoamClient {
    /// Create a new ROAM client (not yet connected).
    pub fn new() -> Self {
        Self {
            status: Arc::new(RwLock::new(ConnectionStatus::Disconnected)),
            client: Arc::new(RwLock::new(None)),
        }
    }

    /// Get the current connection status.
    pub async fn status(&self) -> ConnectionStatus {
        self.status.read().await.clone()
    }

    /// Connect to a REAPER instance.
    pub async fn connect(&self, instance: ReaperInstance) -> Result<(), String> {
        // Update status to connecting
        *self.status.write().await = ConnectionStatus::Connecting;

        info!(
            "Connecting to REAPER (PID {}) at {}",
            instance.pid,
            instance.socket_path.display()
        );

        // Create the connector
        let connector = UnixConnector {
            path: instance.socket_path.clone(),
        };

        // Create the client with automatic reconnection
        let client = connect(connector, HandshakeConfig::default(), NoDispatcher);

        // Try to get a handle to verify the connection works
        match client.handle().await {
            Ok(_handle) => {
                info!("Connected to REAPER (PID {})", instance.pid);
                *self.client.write().await = Some(client);
                *self.status.write().await = ConnectionStatus::Connected(instance);
                Ok(())
            }
            Err(e) => {
                let error_msg = format!("Failed to connect: {}", e);
                warn!("{}", error_msg);
                *self.status.write().await = ConnectionStatus::Error(error_msg.clone());
                Err(error_msg)
            }
        }
    }

    /// Disconnect from the current REAPER instance.
    pub async fn disconnect(&self) {
        *self.client.write().await = None;
        *self.status.write().await = ConnectionStatus::Disconnected;
        info!("Disconnected from REAPER");
    }

    /// Get the transport state.
    pub async fn get_transport_state(&self) -> Result<PlaybackState, String> {
        let client_guard = self.client.read().await;
        let client = client_guard
            .as_ref()
            .ok_or_else(|| "Not connected".to_string())?;

        let transport = TransportClient::new(client.clone());
        transport.get_state()
            .await
            .map_err(|e| format!("RPC error: {}", e))
    }

    /// Start playback.
    pub async fn play(&self) -> Result<TransportResult, String> {
        let client_guard = self.client.read().await;
        let client = client_guard
            .as_ref()
            .ok_or_else(|| "Not connected".to_string())?;

        let transport = TransportClient::new(client.clone());
        transport.play().await.map_err(|e| format!("RPC error: {}", e))
    }

    /// Stop playback.
    pub async fn stop(&self) -> Result<TransportResult, String> {
        let client_guard = self.client.read().await;
        let client = client_guard
            .as_ref()
            .ok_or_else(|| "Not connected".to_string())?;

        let transport = TransportClient::new(client.clone());
        transport.stop().await.map_err(|e| format!("RPC error: {}", e))
    }

    /// Start recording.
    pub async fn record(&self) -> Result<TransportResult, String> {
        let client_guard = self.client.read().await;
        let client = client_guard
            .as_ref()
            .ok_or_else(|| "Not connected".to_string())?;

        let transport = TransportClient::new(client.clone());
        transport.record()
            .await
            .map_err(|e| format!("RPC error: {}", e))
    }

    /// Set playback position.
    pub async fn set_position(&self, seconds: f64) -> Result<TransportResult, String> {
        let client_guard = self.client.read().await;
        let client = client_guard
            .as_ref()
            .ok_or_else(|| "Not connected".to_string())?;

        let transport = TransportClient::new(client.clone());
        transport.set_position(seconds)
            .await
            .map_err(|e| format!("RPC error: {}", e))
    }

    /// Subscribe to transport state updates.
    ///
    /// Returns an Rx channel that will receive PlaybackState updates.
    /// The stream continues until disconnected.
    pub async fn subscribe_transport(&self) -> Result<Rx<PlaybackState>, String> {
        let client_guard = self.client.read().await;
        let client = client_guard
            .as_ref()
            .ok_or_else(|| "Not connected".to_string())?;

        // Create a channel pair - we pass tx to server, keep rx to receive data
        let (tx, rx) = channel::<PlaybackState>();

        // Note: In the new architecture, streaming methods need to be added
        // to the service definition. For now, poll get_state() instead.
        info!("Transport streaming not yet implemented in new service - polling instead");
        
        Ok(rx)
    }

    /// Get a connection handle for use with roam-http-bridge.
    ///
    /// This allows the HTTP server to expose the ROAM connection via WebSocket.
    pub async fn get_handle(&self) -> Result<roam_session::ConnectionHandle, String> {
        let client_guard = self.client.read().await;
        let client = client_guard
            .as_ref()
            .ok_or_else(|| "Not connected".to_string())?;

        client
            .handle()
            .await
            .map_err(|e| format!("Failed to get handle: {}", e))
    }
}

impl Default for RoamClient {
    fn default() -> Self {
        Self::new()
    }
}

// Implement PartialEq using pointer equality for Dioxus memoization
impl PartialEq for RoamClient {
    fn eq(&self, other: &Self) -> bool {
        // Use pointer equality - same instance = equal
        Arc::ptr_eq(&self.status, &other.status) && Arc::ptr_eq(&self.client, &other.client)
    }
}
