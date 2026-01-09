//! REAPER Extension RPC API
//!
//! Defines the RPC protocol for communication between the desktop app and REAPER extension.
//! Uses irpc for type-safe, streaming RPC over QUIC.

use fts::setlist::Setlist;
use irpc::{channel::mpsc, rpc_requests};
use serde::{Deserialize, Serialize};

/// Transport state information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransportState {
    pub is_playing: bool,
    pub is_recording: bool,
    pub is_paused: bool,
    pub position_seconds: f64,
    pub tempo_bpm: f64,
    pub time_sig_num: i32,
    pub time_sig_den: i32,
    pub play_rate: f64,
    pub active_project_name: Option<String>,
}

/// Setlist state with active song/section info
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SetlistState {
    /// The full setlist struct (serialized as JSON for transmission)
    pub setlist_json: String,
    /// Active song index
    pub active_song_index: usize,
    /// Active section index within the active song
    pub active_section_index: usize,
    /// Active song progress (0.0-1.0) if available
    pub active_song_progress: Option<f64>,
    /// Active section progress (0.0-1.0) if available
    pub active_section_progress: Option<f64>,
}

/// Transport command from client to REAPER
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TransportCommand {
    Play,
    Pause,
    Stop,
    Record,
    SetPosition { seconds: f64 },
    SetTempo { bpm: f64 },
}

/// Client request/command
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ClientRequest {
    /// Request current setlist state (one-time)
    GetSetlistState,
    /// Request current transport state (one-time)
    GetTransportState,
    /// Send a transport command
    TransportCommand(TransportCommand),
    /// Heartbeat from client to server
    Heartbeat,
}

/// REAPER Extension RPC Protocol
///
/// This protocol uses bidirectional streaming:
/// - REAPER streams state updates (SetlistState and TransportState) to clients
/// - Clients stream requests/commands (ClientRequest) to REAPER
/// - Both streams are active simultaneously
#[rpc_requests(message = ReaperMessage)]
#[derive(Serialize, Deserialize, Debug)]
pub enum ReaperProtocol {
    /// Bidirectional stream: REAPER sends state updates, client sends requests
    ///
    /// - rx: Client sends ClientRequest messages
    /// - tx: REAPER sends SetlistState and TransportState messages
    ///
    /// The client can send requests at any time, and REAPER will stream
    /// state updates continuously.
    #[rpc(rx = mpsc::Receiver<ClientRequest>, tx = mpsc::Sender<ReaperStateUpdate>)]
    Connect(Connect),
}

/// Initial connection message (no data needed, just opens the bidirectional stream)
#[derive(Debug, Serialize, Deserialize)]
pub struct Connect;

/// State updates from REAPER to client (sent over the bidirectional stream)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ReaperStateUpdate {
    /// Setlist state update
    SetlistState(SetlistState),
    /// Transport state update
    TransportState(TransportState),
    /// Heartbeat from server to client
    Heartbeat,
}

/// Helper to deserialize Setlist from SetlistState
impl SetlistState {
    pub fn deserialize_setlist(&self) -> Result<Setlist, serde_json::Error> {
        serde_json::from_str(&self.setlist_json)
    }
}

/// Helper to serialize Setlist into SetlistState
impl From<(Setlist, usize, usize, Option<f64>, Option<f64>)> for SetlistState {
    fn from(
        (setlist, song_idx, section_idx, song_progress, section_progress): (
            Setlist,
            usize,
            usize,
            Option<f64>,
            Option<f64>,
        ),
    ) -> Self {
        let setlist_json = serde_json::to_string(&setlist).unwrap_or_default();
        Self {
            setlist_json,
            active_song_index: song_idx,
            active_section_index: section_idx,
            active_song_progress: song_progress,
            active_section_progress: section_progress,
        }
    }
}
