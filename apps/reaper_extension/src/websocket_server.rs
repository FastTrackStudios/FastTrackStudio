//! WebSocket server for broadcasting setlist updates to web clients
//!
//! This module runs an Axum WebSocket server that broadcasts setlist information
//! from REAPER to connected web clients.

use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        State,
    },
    response::Response,
    routing::get,
    Router,
};
use futures_util::{sink::SinkExt, stream::StreamExt};
use std::sync::Arc;
use tokio::net::TcpListener;
use tokio::sync::broadcast;
use tower::ServiceBuilder;
use tower_http::{
    cors::{Any, CorsLayer},
    trace::TraceLayer,
};
use tracing::{debug, error, info, warn};

use setlist::core::Setlist;

/// Shared state for WebSocket connections
#[derive(Clone)]
pub struct WebSocketState {
    /// Broadcast channel for sending setlist updates to all connected clients
    pub tx: broadcast::Sender<SetlistMessage>,
    /// Cached setlist (updated when actions trigger rebuilds)
    pub cached_setlist: Arc<tokio::sync::RwLock<Option<Setlist>>>,
    /// Track number of connected clients
    pub client_count: Arc<tokio::sync::RwLock<usize>>,
}

impl WebSocketState {
    pub fn new() -> Self {
        let (tx, _rx) = broadcast::channel(100);
        Self {
            tx,
            cached_setlist: Arc::new(tokio::sync::RwLock::new(None)),
            client_count: Arc::new(tokio::sync::RwLock::new(0)),
        }
    }
    
    pub async fn increment_client_count(&self) {
        let mut count = self.client_count.write().await;
        *count += 1;
        info!(client_count = *count, "Client connected (total: {})", *count);
    }
    
    pub async fn decrement_client_count(&self) {
        let mut count = self.client_count.write().await;
        *count = count.saturating_sub(1);
        info!(client_count = *count, "Client disconnected (total: {})", *count);
    }
    
    pub async fn get_client_count(&self) -> usize {
        *self.client_count.read().await
    }
    
    /// Update the cached setlist
    pub async fn update_cached_setlist(&self, setlist: Setlist) {
        let mut cached = self.cached_setlist.write().await;
        *cached = Some(setlist);
    }
    
    /// Get the cached setlist
    pub async fn get_cached_setlist(&self) -> Option<Setlist> {
        let cached = self.cached_setlist.read().await;
        cached.clone()
    }
}

/// Messages that can be sent over WebSocket
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
#[serde(tag = "type")]
pub enum SetlistMessage {
    /// Full setlist update
    SetlistUpdate {
        setlist: Setlist,
        /// Index of the active song in the setlist (None if no active song)
        active_song_index: Option<usize>,
    },
    /// Transport state update for a specific project (playing, position, etc.)
    /// Includes progress values (0-1) for active song and section, similar to AbleSet
    TransportUpdate {
        project_name: String,
        is_active: bool,
        playing: bool,
        position: f64,
        tempo: f64,
        /// Progress for each song in this project (0-1), keyed by song name
        song_progress: std::collections::HashMap<String, f64>,
        /// Progress for each section in each song (0-1), keyed by "song_name:section_name"
        section_progress: std::collections::HashMap<String, f64>,
    },
    /// Client request for setlist update
    RequestSetlist,
    /// Client request to switch to a project by name
    SwitchToProject {
        project_name: String,
    },
    /// Client request to seek to a section's start position
    SeekToSection {
        project_name: String,
        song_name: String,
        section_name: String,
    },
    /// Ping message for connection health
    Ping,
    /// Pong response to ping
    Pong,
}

/// WebSocket handler endpoint
pub async fn websocket_handler(
    ws: WebSocketUpgrade,
    State(state): State<Arc<WebSocketState>>,
) -> Response {
    ws.on_upgrade(|socket| handle_socket(socket, state))
}

/// Handle a WebSocket connection
async fn handle_socket(socket: WebSocket, state: Arc<WebSocketState>) {
    let (mut sender, mut receiver) = socket.split();
    
    // Increment client count
    state.increment_client_count().await;
    
    // Subscribe to broadcast channel
    let mut rx = state.tx.subscribe();
    
    info!("New WebSocket client connected");
    
    // Send cached setlist immediately on connection (if available)
    // The setlist is built on the main thread when actions are triggered
    if let Some(setlist) = state.get_cached_setlist().await {
        // For cached setlist, we don't have active_song_index, so use None
        // The frontend will determine it from transport states
        let msg = SetlistMessage::SetlistUpdate { 
            setlist: setlist.clone(),
            active_song_index: None,
        };
        match serde_json::to_string(&msg) {
            Ok(json) => {
                if sender.send(Message::Text(json.into())).await.is_err() {
                    return;
                }
            }
            Err(e) => {
                warn!(error = %e, "Failed to serialize initial setlist");
            }
        }
    }
    
    // Spawn task to send messages from broadcast channel to client
    let mut send_task = tokio::spawn(async move {
        while let Ok(msg) = rx.recv().await {
            let json = match serde_json::to_string(&msg) {
                Ok(json) => json,
                Err(e) => {
                    error!(error = %e, "Failed to serialize message");
                    continue;
                }
            };
            
            if sender.send(Message::Text(json.into())).await.is_err() {
                break;
            }
        }
    });
    
    // Handle incoming messages from client
    let state_for_recv = state.clone();
    let mut recv_task = tokio::spawn(async move {
        while let Some(Ok(msg)) = receiver.next().await {
            match msg {
                Message::Text(text) => {
                    // Try to parse as SetlistMessage
                    match serde_json::from_str::<SetlistMessage>(&text) {
                        Ok(SetlistMessage::RequestSetlist) => {
                            
                            // Always request a rebuild to get fresh data
                            // The rebuild will happen on main thread and broadcast when complete
                            crate::websocket_state::request_setlist_rebuild();
                            
                            // Send cached setlist immediately (if available) so client gets something right away
                            // The rebuild will broadcast a fresh update when complete
                            if let Some(cached_setlist) = state_for_recv.get_cached_setlist().await {
                                let msg = SetlistMessage::SetlistUpdate { 
                                    setlist: cached_setlist,
                                    active_song_index: None, // Cached setlist doesn't have active song index
                                };
                                let _ = state_for_recv.tx.send(msg);
                            } else {
                                // Send empty setlist so client knows the request was received
                                // The rebuild will broadcast an update when complete
                                let empty_setlist = Setlist::new("REAPER Setlist".to_string())
                                    .unwrap_or_else(|_| Setlist::default());
                                let msg = SetlistMessage::SetlistUpdate { 
                                    setlist: empty_setlist,
                                    active_song_index: None,
                                };
                                let _ = state_for_recv.tx.send(msg);
                            }
                        }
                        Ok(SetlistMessage::SwitchToProject { project_name }) => {
                            // Request project switch on main thread
                            crate::websocket_state::request_project_switch(project_name);
                        }
                        Ok(SetlistMessage::SeekToSection { project_name, song_name, section_name }) => {
                            // Request section seek on main thread
                            crate::websocket_state::request_section_seek(project_name, song_name, section_name);
                        }
                        Ok(SetlistMessage::Ping) => {
                            // Send pong back via broadcast channel
                            let _ = state_for_recv.tx.send(SetlistMessage::Pong);
                        }
                        Ok(_) => {
                            // Other message types
                        }
                        Err(_) => {
                            // Not a SetlistMessage, might be plain text
                            if text == "ping" {
                                let _ = state_for_recv.tx.send(SetlistMessage::Pong);
                            }
                        }
                    }
                }
                Message::Close(_) => {
                    debug!("Client disconnected (close)");
                    break;
                }
                _ => {}
            }
        }
    });
    
    // Wait for either task to complete
    tokio::select! {
        _ = &mut send_task => {
            recv_task.abort();
        }
        _ = &mut recv_task => {
            send_task.abort();
        }
    }
    
    // Decrement client count
    state.decrement_client_count().await;
    
    info!("WebSocket client disconnected");
}

/// Start the WebSocket server
pub async fn start_websocket_server(port: u16) -> anyhow::Result<(tokio::task::JoinHandle<anyhow::Result<()>>, Arc<WebSocketState>)> {
    // Create WebSocket state
    let ws_state = Arc::new(WebSocketState::new());
    
    // Create router with WebSocket endpoint
    let router = Router::new()
        .route("/ws", get(websocket_handler))
        .layer(
            ServiceBuilder::new()
                .layer(TraceLayer::new_for_http())
                .layer(
                    CorsLayer::new()
                        .allow_origin(Any)
                        .allow_methods(Any)
                        .allow_headers(Any),
                ),
        )
        .with_state(ws_state.clone());
    
    let addr = std::net::SocketAddr::from(([0, 0, 0, 0], port));
    let listener = TcpListener::bind(&addr).await?;
    
    info!(
        port = port,
        "Starting WebSocket server for setlist updates"
    );
    
    let handle = tokio::spawn(async move {
        info!("WebSocket server listening on ws://0.0.0.0:{}/ws", port);
        info!("Web clients can connect at ws://localhost:{}/ws", port);
        
        if let Err(e) = axum::serve(listener, router).await {
            anyhow::bail!("WebSocket server error: {}", e);
        }
        
        Ok(())
    });
    
    Ok((handle, ws_state))
}

/// Broadcast a setlist update to all connected clients
/// Also updates the cached setlist for new connections
/// 
/// NOTE: This must be called from a context with access to a tokio runtime handle.
/// Use `broadcast_setlist_with_runtime` if calling from a non-tokio context.
pub fn broadcast_setlist(state: &Arc<WebSocketState>, setlist: Setlist, active_song_index: Option<usize>) {
    // Update cache (spawn async task to update cache)
    let state_clone = state.clone();
    let setlist_clone = setlist.clone();
    tokio::spawn(async move {
        state_clone.update_cached_setlist(setlist_clone).await;
    });
    
    // Broadcast to all connected clients
    let msg = SetlistMessage::SetlistUpdate { setlist, active_song_index };
    match state.tx.send(msg) {
        Ok(_count) => {
            // Setlist update sent successfully - no logging needed
        }
        Err(e) => {
            // Check client count asynchronously - if no clients, this is expected
            let state_clone = state.clone();
            tokio::spawn(async move {
                let client_count = state_clone.get_client_count().await;
                if client_count > 0 {
                    warn!(
                        error = %e,
                        client_count = client_count,
                        "Failed to broadcast setlist update (clients may have disconnected)"
                    );
                }
            });
        }
    }
}

/// Broadcast a setlist update using a provided tokio runtime handle
/// This is for calling from non-tokio contexts (like REAPER's main thread)
pub fn broadcast_setlist_with_runtime(
    state: &Arc<WebSocketState>,
    setlist: Setlist,
    active_song_index: Option<usize>,
    rt: &tokio::runtime::Handle,
) {
    // Update cache using the provided runtime handle
    let state_clone = state.clone();
    let setlist_clone = setlist.clone();
    rt.spawn(async move {
        state_clone.update_cached_setlist(setlist_clone).await;
    });
    
    // Broadcast to all connected clients
    let msg = SetlistMessage::SetlistUpdate { 
        setlist: setlist.clone(),
        active_song_index,
    };
    match state.tx.send(msg) {
        Ok(_count) => {
            // Setlist update sent successfully - no logging needed
        }
        Err(e) => {
            // Check client count asynchronously - if no clients, this is expected
            let state_clone = state.clone();
            rt.spawn(async move {
                let client_count = state_clone.get_client_count().await;
                if client_count > 0 {
                    warn!(
                        error = %e,
                        client_count = client_count,
                        "Failed to broadcast setlist update (clients may have disconnected)"
                    );
                }
            });
        }
    }
}

/// Broadcast a transport update for a specific project to all connected clients
pub fn broadcast_transport(
    state: &Arc<WebSocketState>,
    project_name: &str,
    is_active: bool,
    playing: bool,
    position: f64,
    tempo: f64,
    song_progress: std::collections::HashMap<String, f64>,
    section_progress: std::collections::HashMap<String, f64>,
) {
    let msg = SetlistMessage::TransportUpdate {
        project_name: project_name.to_string(),
        is_active,
        playing,
        position,
        tempo,
        song_progress,
        section_progress,
    };
    match state.tx.send(msg) {
        Ok(_count) => {
            // Transport updates sent successfully - no logging needed at 30Hz
        }
        Err(e) => {
            // Only warn if we actually have clients connected (channel closed means no receivers)
            // If there are no clients, this is expected and not an error
            // Spawn async task to check client count (this function is synchronous)
            let state_clone = state.clone();
            tokio::spawn(async move {
                let client_count = state_clone.get_client_count().await;
                if client_count > 0 {
                    warn!(
                        error = %e,
                        client_count = client_count,
                        "Failed to broadcast transport update (clients may have disconnected)"
                    );
                }
            });
        }
    }
}

