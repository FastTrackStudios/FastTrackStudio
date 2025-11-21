//! Server state manager - connects to WebSocket server using Dioxus WebSocket API
//!
//! This module provides:
//! 1. A Dioxus server function that bridges to external WebSocket servers (like reaper_extension)
//! 2. Client-side hooks for managing WebSocket connections
//! 3. Message handling to update state signals

use dioxus::prelude::*;
use dioxus_fullstack::{use_websocket, WebSocketOptions, Websocket};
use setlist::Setlist;
use std::collections::HashMap;
use crate::state::messages::{ClientEvent, ServerEvent};

/// Setup WebSocket connection on the client side
/// 
/// This uses Dioxus's `use_websocket` hook which connects to a server function.
/// The server function acts as a bridge to the external WebSocket server.
pub fn create_websocket_connection(
    server_url: String,
) -> impl Fn() -> Websocket<ClientEvent, ServerEvent> {
    move || {
        use_websocket(|| setlist_ws(server_url.clone(), WebSocketOptions::new()))
    }
}

/// Handle incoming server messages and update state signals
pub fn handle_server_message(
    message: ServerEvent,
    setlist: Signal<Setlist>,
    transport_positions: Signal<HashMap<String, f64>>,
    song_positions: Signal<HashMap<String, f64>>,
    current_song_index: Signal<Option<usize>>,
    current_section_index: Signal<Option<usize>>,
    is_playing: Signal<bool>,
) {
    match message {
        ServerEvent::SetlistUpdate { setlist: new_setlist, active_song_index } => {
            setlist.set(new_setlist);
            if let Some(idx) = active_song_index {
                current_song_index.set(Some(idx));
            }
        }
        ServerEvent::TransportUpdate {
            project_name,
            playing,
            position,
            song_progress,
            section_progress,
            ..
        } => {
            // Update transport position for this project
            transport_positions.with_mut(|positions| {
                positions.insert(project_name.clone(), position);
            });
            
            // Update playing state
            is_playing.set(playing);
            
            // Note: song_progress and section_progress are 0-1 values
            // We'd need the setlist to convert these to absolute positions
            // For now, we rely on the position field for transport positions
        }
        ServerEvent::Pong => {
            // Connection is alive
        }
    }
}

/// Server function for WebSocket connection
/// 
/// This server function handles WebSocket connections from clients (web browsers or other instances).
/// 
/// When `external_server_url` is provided, it bridges to an external WebSocket server (like reaper_extension).
/// When `external_server_url` is None, the desktop app manages state itself.
/// 
/// Architecture:
/// - Desktop app runs this server function (always available when desktop feature is enabled)
/// - Web clients connect via WebSocket to desktop app
/// - Desktop app can bridge to external servers (reaper_extension) or manage state locally
/// - Each client can toggle between local/server mode independently
#[dioxus::fullstack::get("/api/setlist_ws?external_server_url")]
pub async fn setlist_ws(
    external_server_url: Option<String>,
    options: WebSocketOptions,
) -> Result<Websocket<ClientEvent, ServerEvent, dioxus::fullstack::JsonEncoding>, dioxus::fullstack::prelude::ServerFnError> {
    use tokio::sync::broadcast;
    use std::sync::Arc;
    
    // If external server URL is provided, bridge to it
    // Otherwise, desktop app manages state itself (local mode on server side)
    if let Some(server_url) = external_server_url {
        // Bridge mode: connect to external server (e.g., reaper_extension)
        use tokio_tungstenite::{connect_async, tungstenite::Message as WsMessage};
        use futures_util::{SinkExt, StreamExt};
        
        let external_ws_url = format!("ws://{}/ws", server_url);
        let (external_stream, _) = connect_async(&external_ws_url)
            .await
            .map_err(|e| dioxus::fullstack::prelude::ServerFnError::new(format!("Failed to connect to external server: {}", e)))?;
        
        let (mut external_write, mut external_read) = external_stream.split();
        
        // Return a WebSocket that bridges client <-> external server
        Ok(options.on_upgrade(move |mut client_socket| async move {
            // Spawn task to forward messages from external server to client
            let mut client_socket_for_external = client_socket.clone();
            tokio::spawn(async move {
                while let Some(msg) = external_read.next().await {
                    match msg {
                        Ok(WsMessage::Text(text)) => {
                            // Parse external server message and convert to ServerEvent
                            // The external server uses SetlistMessage format, we convert to ServerEvent
                            if let Ok(server_event) = parse_external_message(&text) {
                                let _ = client_socket_for_external.send(server_event).await;
                            }
                        }
                        Ok(WsMessage::Close(_)) => break,
                        Err(e) => {
                            eprintln!("External WebSocket error: {}", e);
                            break;
                        }
                        _ => {}
                    }
                }
            });
            
            // Forward messages from client to external server
            while let Ok(client_event) = client_socket.recv().await {
                // Convert ClientEvent to external server message format
                let external_msg = convert_client_event_to_external(&client_event);
                
                if let Some(text) = external_msg {
                    if let Ok(msg) = serde_json::to_string(&text) {
                        let _ = external_write.send(WsMessage::Text(msg.into())).await;
                    }
                }
            }
        }))
    } else {
        // Desktop app manages state itself (local mode on server)
        // In this case, we'd need shared state management on the server
        // For now, return an error - this would need server-side state management
        Err(dioxus::fullstack::prelude::ServerFnError::new(
            "Server-managed state not yet implemented. Use external_server_url to bridge to external server."
        ))
    }
}

/// Parse external server message (SetlistMessage format) into ServerEvent
fn parse_external_message(text: &str) -> Result<ServerEvent, serde_json::Error> {
    // External server uses SetlistMessage format with "type" tag
    // We need to parse it and convert to ServerEvent
    use serde_json::Value;
    
    let value: Value = serde_json::from_str(text)?;
    let msg_type = value.get("type").and_then(|v| v.as_str());
    
    match msg_type {
        Some("SetlistUpdate") => {
            // Parse SetlistUpdate from external format
            serde_json::from_value(value)
        }
        Some("TransportUpdate") => {
            // Parse TransportUpdate from external format
            serde_json::from_value(value)
        }
        Some("Pong") => Ok(ServerEvent::Pong),
        _ => Err(serde::de::Error::custom("Unknown message type"))
    }
}

/// Convert ClientEvent to external server message format
fn convert_client_event_to_external(event: &ClientEvent) -> Option<serde_json::Value> {
    match event {
        ClientEvent::RequestSetlist => Some(serde_json::json!({"type": "RequestSetlist"})),
        ClientEvent::SwitchToProject { project_name } => {
            Some(serde_json::json!({"type": "SwitchToProject", "project_name": project_name}))
        }
        ClientEvent::SeekToSection { project_name, song_name, section_name } => {
            Some(serde_json::json!({
                "type": "SeekToSection",
                "project_name": project_name,
                "song_name": song_name,
                "section_name": section_name
            }))
        }
        ClientEvent::Ping => Some(serde_json::json!({"type": "Ping"})),
        _ => None, // Navigation events handled locally on client
    }
}

// Note: The server function above handles both server and client cases.
// When called from client via use_websocket, Dioxus automatically routes
// the call to the server function via HTTP/WebSocket upgrade.
