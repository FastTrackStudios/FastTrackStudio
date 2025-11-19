//! WebSocket client for broadcasting setlist updates to the desktop web server
//!
//! This module connects the REAPER extension to the desktop web server's WebSocket endpoint
//! and broadcasts setlist and transport updates in real-time.

use anyhow::Result;
use setlist::core::Setlist;
use tokio_tungstenite::{connect_async, tungstenite::Message as WsMessage};
use tracing::{debug, error, info, warn};
use futures_util::{SinkExt, StreamExt};

/// WebSocket client configuration
#[derive(Debug, Clone)]
pub struct WebSocketClientConfig {
    /// Web server host (default: localhost)
    pub host: String,
    /// Web server port (default: 8080)
    pub port: u16,
}

impl Default for WebSocketClientConfig {
    fn default() -> Self {
        Self {
            host: "localhost".to_string(),
            port: 8080,
        }
    }
}

/// WebSocket client for broadcasting updates
pub struct WebSocketClient {
    config: WebSocketClientConfig,
    tx: Option<tokio::sync::broadcast::Sender<SetlistMessage>>,
    handle: Option<tokio::task::JoinHandle<()>>,
}

/// Messages that can be sent to the WebSocket server
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = "type")]
pub enum SetlistMessage {
    /// Full setlist update
    SetlistUpdate {
        setlist: Setlist,
    },
    /// Transport state update (playing, position, etc.)
    TransportUpdate {
        playing: bool,
        position: f64,
        tempo: f64,
    },
    /// Ping message for connection health
    Ping,
    /// Pong response to ping
    Pong,
}

impl WebSocketClient {
    /// Create a new WebSocket client
    pub fn new(config: WebSocketClientConfig) -> Self {
        Self {
            config,
            tx: None,
            handle: None,
        }
    }

    /// Start the WebSocket client connection
    /// Returns a sender channel for broadcasting messages
    pub async fn start(&mut self) -> Result<tokio::sync::broadcast::Sender<SetlistMessage>> {
        let url = format!("ws://{}:{}/ws", self.config.host, self.config.port);
        info!(url = %url, "Connecting to WebSocket server");
        
        let (tx, _rx) = tokio::sync::broadcast::channel::<SetlistMessage>(100);
        let tx_for_client = tx.clone();
        
        let url_clone = url.clone();
        let handle = tokio::spawn(async move {
            loop {
                // Create a new receiver for each connection attempt
                let mut rx = tx.subscribe();
                
                match connect_async(&url_clone).await {
                    Ok((ws_stream, _)) => {
                        info!("WebSocket connected successfully");
                        let (mut write, mut read) = ws_stream.split();
                        
                        // Spawn task to read messages from server
                        let mut read_task = tokio::spawn(async move {
                            while let Some(msg) = read.next().await {
                                match msg {
                                    Ok(WsMessage::Text(text)) => {
                                        debug!(message = %text, "Received message from server");
                                    }
                                    Ok(WsMessage::Close(_)) => {
                                        warn!("Server closed connection");
                                        break;
                                    }
                                    Ok(WsMessage::Ping(data)) => {
                                        // Echo pong back - need to use write handle
                                        // Note: This won't work with split, so we'll handle ping differently
                                    }
                                    Err(e) => {
                                        error!(error = %e, "WebSocket error");
                                        break;
                                    }
                                    _ => {}
                                }
                            }
                        });
                        
                        // Spawn task to send messages to server
                        let mut write_task = tokio::spawn(async move {
                            while let Ok(msg) = rx.recv().await {
                                let json = match serde_json::to_string(&msg) {
                                    Ok(json) => json,
                                    Err(e) => {
                                        error!(error = %e, "Failed to serialize message");
                                        continue;
                                    }
                                };
                                
                                if write.send(WsMessage::Text(json)).await.is_err() {
                                    debug!("Failed to send message, connection may be closed");
                                    break;
                                }
                            }
                        });
                        
                        // Wait for either task to complete
                        tokio::select! {
                            _ = &mut read_task => {
                                write_task.abort();
                            }
                            _ = &mut write_task => {
                                read_task.abort();
                            }
                        }
                        
                        warn!("WebSocket connection closed, will attempt to reconnect in 5 seconds");
                    }
                    Err(e) => {
                        warn!(error = %e, url = %url_clone, "Failed to connect to WebSocket server, retrying in 5 seconds");
                    }
                }
                
                // Wait before reconnecting
                tokio::time::sleep(tokio::time::Duration::from_secs(5)).await;
            }
        });
        
        self.handle = Some(handle);
        self.tx = Some(tx_for_client.clone());
        
        Ok(tx_for_client)
    }

    /// Broadcast a setlist update
    pub fn broadcast_setlist(&self, setlist: Setlist) {
        if let Some(ref tx) = self.tx {
            let msg = SetlistMessage::SetlistUpdate { setlist };
            match tx.send(msg) {
                Ok(count) => {
                    debug!(receiver_count = count, "Setlist update sent to {} receiver(s)", count);
                }
                Err(e) => {
                    // This is normal if no receivers are subscribed yet (connection not established)
                    debug!(error = %e, "No active WebSocket connection to send setlist update");
                }
            }
        } else {
            warn!("WebSocket client not started, cannot broadcast setlist");
        }
    }

    /// Broadcast a transport update
    pub fn broadcast_transport(&self, playing: bool, position: f64, tempo: f64) {
        if let Some(ref tx) = self.tx {
            let msg = SetlistMessage::TransportUpdate {
                playing,
                position,
                tempo,
            };
            match tx.send(msg) {
                Ok(count) => {
                    debug!(receiver_count = count, "Transport update sent to {} receiver(s)", count);
                }
                Err(e) => {
                    // This is normal if no receivers are subscribed yet (connection not established)
                    debug!(error = %e, "No active WebSocket connection to send transport update");
                }
            }
        } else {
            warn!("WebSocket client not started, cannot broadcast transport");
        }
    }
}

