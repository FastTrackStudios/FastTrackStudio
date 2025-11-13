//! WebSocket Infrastructure for Transport
//!
//! This module provides WebSocket real-time communication for transport operations.
//! It adapts the core transport domain to WebSocket protocol for real-time control
//! and status updates.

use axum::{
    extract::{
        ws::{Message, WebSocket, WebSocketUpgrade},
        ConnectInfo, State,
    },
    response::IntoResponse,
};
use futures_util::{sink::SinkExt, stream::StreamExt};
use serde::{Deserialize, Serialize};
use std::{net::SocketAddr, sync::Arc};
use tokio::sync::{broadcast, mpsc, Mutex};

use crate::core::{TransportActions, TransportError, Tempo, RecordMode};
use primitives::TimeSignature;

/// WebSocket message types for transport communication
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum TransportMessage {
    // Commands (Client -> Server)
    Command(TransportCommand),

    // Responses (Server -> Client)
    Response(TransportResponse),

    // Status updates (Server -> All Clients)
    StatusUpdate(TransportStatus),

    // Error responses
    Error { message: String },

    // Connection management
    Ping,
    Pong,
}

/// Transport commands that clients can send
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "action")]
pub enum TransportCommand {
    // Playback control
    Play,
    Pause,
    Stop,
    PlayPause,
    PlayStop,

    // Recording control
    StartRecording,
    StopRecording,
    ToggleRecording,

    // Configuration
    SetTempo { bpm: f64 },
    SetTimeSignature { numerator: i32, denominator: i32 },
    SetRecordMode { mode: String },
    SetPosition { seconds: f64 },

    // Queries
    GetStatus,
    GetTempo,
    GetPosition,
    IsPlaying,
    IsRecording,
}

/// Transport responses sent back to clients
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransportResponse {
    pub success: bool,
    pub message: String,
    pub data: Option<serde_json::Value>,
}

/// Transport status for real-time updates
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransportStatus {
    pub is_playing: bool,
    pub is_recording: bool,
    pub tempo_bpm: f64,
    pub position_seconds: f64,
    pub time_signature: TimeSignatureData,
    pub record_mode: String,
    pub timestamp: u64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TimeSignatureData {
    pub numerator: i32,
    pub denominator: i32,
}

/// WebSocket handler state
pub struct WebSocketHandler<T> {
    transport: Arc<Mutex<T>>,
    status_broadcast: broadcast::Sender<TransportStatus>,
}

impl<T> Clone for WebSocketHandler<T> {
    fn clone(&self) -> Self {
        Self {
            transport: self.transport.clone(),
            status_broadcast: self.status_broadcast.clone(),
        }
    }
}

impl<T> WebSocketHandler<T>
where
    T: TransportActions + Send + Sync + 'static,
{
    /// Create a new WebSocket handler
    pub fn new(transport: Arc<Mutex<T>>) -> Self {
        let (status_broadcast, _) = broadcast::channel(100);

        Self {
            transport,
            status_broadcast,
        }
    }

    /// Get a status broadcast receiver
    pub fn subscribe_status(&self) -> broadcast::Receiver<TransportStatus> {
        self.status_broadcast.subscribe()
    }

    /// Broadcast status update to all connected clients
    async fn broadcast_status(&self) -> Result<(), TransportError> {
        let transport = self.transport.lock().await;
        let tempo = transport.get_tempo()?;
        let position = transport.get_position()?;
        let time_sig = transport.get_time_signature()?;
        let record_mode = transport.get_record_mode()?;
        let is_playing = transport.is_playing()?;
        let is_recording = transport.is_recording()?;

        let record_mode_str = match record_mode {
            RecordMode::Normal => "normal",
            RecordMode::TimeSelection => "time_selection",
            RecordMode::Item => "item",
        };

        let status = TransportStatus {
            is_playing,
            is_recording,
            tempo_bpm: tempo.bpm,
            position_seconds: position,
            time_signature: TimeSignatureData {
                numerator: time_sig.numerator,
                denominator: time_sig.denominator,
            },
            record_mode: record_mode_str.to_string(),
            timestamp: std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap_or_default()
                .as_millis() as u64,
        };

        // Send to all subscribers (ignore if no receivers)
        let _ = self.status_broadcast.send(status);
        Ok(())
    }

    /// Handle a transport command
    async fn handle_command(&self, command: TransportCommand) -> TransportResponse {
        let mut transport = self.transport.lock().await;

        let result = match &command {
            TransportCommand::Play => transport.play(),
            TransportCommand::Pause => transport.pause(),
            TransportCommand::Stop => transport.stop(),
            TransportCommand::PlayPause => transport.play_pause(),
            TransportCommand::PlayStop => transport.play_stop(),
            TransportCommand::StartRecording => transport.start_recording(),
            TransportCommand::StopRecording => transport.stop_recording(),
            TransportCommand::ToggleRecording => transport.toggle_recording(),

            TransportCommand::SetTempo { bpm } => {
                transport.set_tempo(Tempo::new(*bpm))
            },
            TransportCommand::SetTimeSignature { numerator, denominator } => {
                transport.set_time_signature(TimeSignature::new(*numerator, *denominator))
            },
            TransportCommand::SetRecordMode { mode } => {
                let record_mode = match mode.to_lowercase().as_str() {
                    "normal" => RecordMode::Normal,
                    "time_selection" => RecordMode::TimeSelection,
                    "item" => RecordMode::Item,
                    _ => return TransportResponse {
                        success: false,
                        message: "Invalid record mode".to_string(),
                        data: None,
                    },
                };
                transport.set_record_mode(record_mode)
            },
            TransportCommand::SetPosition { seconds } => {
                transport.set_position(*seconds)
            },

            TransportCommand::GetStatus => {
                // Handle this separately below
                Ok("Status request".to_string())
            },
            TransportCommand::GetTempo => {
                match transport.get_tempo() {
                    Ok(tempo) => Ok(format!("Current tempo: {} BPM", tempo.bpm)),
                    Err(e) => Err(e),
                }
            },
            TransportCommand::GetPosition => {
                match transport.get_position() {
                    Ok(pos) => Ok(format!("Current position: {} seconds", pos)),
                    Err(e) => Err(e),
                }
            },
            TransportCommand::IsPlaying => {
                match transport.is_playing() {
                    Ok(playing) => Ok(format!("Is playing: {}", playing)),
                    Err(e) => Err(e),
                }
            },
            TransportCommand::IsRecording => {
                match transport.is_recording() {
                    Ok(recording) => Ok(format!("Is recording: {}", recording)),
                    Err(e) => Err(e),
                }
            },
        };

        // Drop the lock before broadcasting
        drop(transport);

        // Broadcast status update after any state-changing command
        match &command {
            TransportCommand::Play | TransportCommand::Pause | TransportCommand::Stop |
            TransportCommand::PlayPause | TransportCommand::PlayStop |
            TransportCommand::StartRecording | TransportCommand::StopRecording |
            TransportCommand::ToggleRecording | TransportCommand::SetTempo { .. } |
            TransportCommand::SetTimeSignature { .. } | TransportCommand::SetRecordMode { .. } |
            TransportCommand::SetPosition { .. } => {
                let _ = self.broadcast_status().await;
            },
            _ => {}, // No broadcast for queries
        }

        match result {
            Ok(message) => TransportResponse {
                success: true,
                message,
                data: None,
            },
            Err(e) => TransportResponse {
                success: false,
                message: e.to_string(),
                data: None,
            },
        }
    }
}

/// WebSocket upgrade handler
pub async fn ws_transport_handler<T>(
    ws: WebSocketUpgrade,
    ConnectInfo(addr): ConnectInfo<SocketAddr>,
    State(handler): State<WebSocketHandler<T>>,
) -> impl IntoResponse
where
    T: TransportActions + Send + Sync + 'static,
{
    println!("WebSocket transport connection from {addr}");
    ws.on_upgrade(move |socket| handle_transport_socket(socket, addr, handler))
}

/// Handle individual WebSocket connection
async fn handle_transport_socket<T>(
    socket: WebSocket,
    who: SocketAddr,
    handler: WebSocketHandler<T>,
) where
    T: TransportActions + Send + Sync + 'static,
{
    println!("Transport WebSocket connected: {who}");

    let (sender, mut receiver) = socket.split();
    let (tx, mut rx) = mpsc::unbounded_channel::<Message>();
    let mut status_rx = handler.subscribe_status();

    // Send initial status
    if let Ok(()) = handler.broadcast_status().await {
        // Status will be sent via the broadcast mechanism
    }

    // Spawn task to handle outgoing messages
    let mut send_task = tokio::spawn(async move {
        let mut sender = sender;
        while let Some(msg) = rx.recv().await {
            if let Err(e) = sender.send(msg).await {
                println!("Failed to send message to {who}: {e}");
                break;
            }
        }
    });

    // Spawn task to handle incoming messages from client
    let handler_clone = handler.clone();
    let tx_clone = tx.clone();
    let mut recv_task = tokio::spawn(async move {
        while let Some(Ok(msg)) = receiver.next().await {
            match msg {
                Message::Text(text) => {
                    match serde_json::from_str::<TransportMessage>(&text) {
                        Ok(TransportMessage::Command(command)) => {
                            let response = handler_clone.handle_command(command).await;
                            let response_msg = TransportMessage::Response(response);

                            if let Ok(json) = serde_json::to_string(&response_msg) {
                                let _ = tx_clone.send(Message::Text(json.into()));
                            }
                        },
                        Ok(TransportMessage::Ping) => {
                            let pong = TransportMessage::Pong;
                            if let Ok(json) = serde_json::to_string(&pong) {
                                let _ = tx_clone.send(Message::Text(json.into()));
                            }
                        },
                        Ok(_) => {
                            // Ignore other message types from client
                        },
                        Err(e) => {
                            println!("Failed to parse message from {who}: {e}");
                            let error_msg = TransportMessage::Error {
                                message: "Invalid message format".to_string(),
                            };
                            if let Ok(json) = serde_json::to_string(&error_msg) {
                                let _ = tx_clone.send(Message::Text(json.into()));
                            }
                        }
                    }
                },
                Message::Close(_) => {
                    println!("WebSocket {who} disconnected");
                    break;
                },
                Message::Ping(data) => {
                    let _ = tx_clone.send(Message::Pong(data));
                },
                _ => {
                    // Handle other message types as needed
                }
            }
        }
    });

    // Spawn task to handle status broadcasts
    let tx_broadcast = tx.clone();
    let mut broadcast_task = tokio::spawn(async move {
        while let Ok(status) = status_rx.recv().await {
            let status_msg = TransportMessage::StatusUpdate(status);
            if let Ok(json) = serde_json::to_string(&status_msg) {
                if let Err(_) = tx_broadcast.send(Message::Text(json.into())) {
                    break;
                }
            }
        }
    });

    // Wait for any task to complete
    tokio::select! {
        _ = (&mut recv_task) => {
            send_task.abort();
            broadcast_task.abort();
        },
        _ = (&mut send_task) => {
            recv_task.abort();
            broadcast_task.abort();
        }
        _ = (&mut broadcast_task) => {
            recv_task.abort();
            send_task.abort();
        }
    }

    println!("Transport WebSocket {who} connection closed");
}

/// Create WebSocket router for transport
pub fn create_transport_ws_router<T>() -> axum::Router<WebSocketHandler<T>>
where
    T: TransportActions + Send + Sync + 'static,
{
    axum::Router::new()
        .route("/ws", axum::routing::get(ws_transport_handler::<T>))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::Transport;

    #[tokio::test]
    async fn test_websocket_handler_creation() {
        let transport = Arc::new(Mutex::new(Transport::new()));
        let handler = WebSocketHandler::new(transport);

        // Test that we can create a status subscription
        let _rx = handler.subscribe_status();
        assert!(true); // If we get here without panic, it works
    }

    #[tokio::test]
    async fn test_command_handling() {
        let transport = Arc::new(Mutex::new(Transport::new()));
        let handler = WebSocketHandler::new(transport);

        // Test play command
        let command = TransportCommand::Play;
        let response = handler.handle_command(command).await;
        assert!(response.success);
        assert!(response.message.contains("Playback started"));

        // Test tempo setting
        let command = TransportCommand::SetTempo { bpm: 140.0 };
        let response = handler.handle_command(command).await;
        assert!(response.success);
        assert!(response.message.contains("140"));
    }

    #[test]
    fn test_message_serialization() {
        let command = TransportCommand::Play;
        let msg = TransportMessage::Command(command);

        let json = serde_json::to_string(&msg).unwrap();
        let parsed: TransportMessage = serde_json::from_str(&json).unwrap();

        match parsed {
            TransportMessage::Command(TransportCommand::Play) => {},
            _ => panic!("Failed to serialize/deserialize correctly"),
        }
    }
}
