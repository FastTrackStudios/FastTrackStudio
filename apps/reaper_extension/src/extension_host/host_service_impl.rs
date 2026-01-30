//! Unified Host Service Implementation - Direct REAPER API Access
//!
//! Implements HostService by directly calling REAPER APIs.
//! This runs in the REAPER extension context, so it has full API access.
//!
//! IMPORTANT: REAPER APIs must be called from the main thread.
//! RPC handlers run on tokio worker threads, so we queue commands
//! for execution by the timer callback on the main thread.
//!
//! Note: Extensions are separate processes and CANNOT call REAPER APIs directly.
//! All DAW functionality must be implemented here in the host.

use host_proto::{
    HostService, ReadyMsg, ReadyAck,
    TransportResult, PlaybackState,
    LoggerResult,
    MarkerResult, Marker,
    TrackResult, Track,
};
use roam::Tx;
use roam_session::{ConnectionHandle, Context};
use std::collections::HashMap;
use std::sync::mpsc;
use std::sync::Arc;
use parking_lot::RwLock;
use tracing::{debug, info, trace, warn};

// ============================================================================
// Command Queue Types (for main-thread execution)
// ============================================================================

/// Commands that need to execute on the main thread
#[derive(Debug)]
enum HostCommand {
    // Transport
    Play,
    Stop,
    Record,
    GetState,
    SetPosition(f64),
    // Logger
    Log(String),
    // Markers
    AddMarker { position: f64, name: String },
    GetMarkers,
    DeleteMarker(usize),
    // Tracks
    GetTrackCount,
    GetTrack(usize),
    GetAllTracks,
    SetTrackName { index: usize, name: String },
}

/// Results from main-thread command execution
#[derive(Debug)]
enum HostCommandResult {
    Transport(TransportResult),
    PlaybackState(PlaybackState),
    Logger(LoggerResult),
    Marker(MarkerResult),
    TrackCount(usize),
    Track(TrackResult),
}

/// Global command queue for extension host
/// This is accessed by both RPC handlers (to queue) and timer (to process)
static COMMAND_QUEUE: std::sync::OnceLock<CommandQueue> = std::sync::OnceLock::new();

/// Command queue with channel for cross-thread communication
pub struct CommandQueue {
    tx: mpsc::Sender<(HostCommand, mpsc::Sender<HostCommandResult>)>,
    rx: parking_lot::Mutex<mpsc::Receiver<(HostCommand, mpsc::Sender<HostCommandResult>)>>,
}

impl CommandQueue {
    fn new() -> Self {
        let (tx, rx) = mpsc::channel();
        Self {
            tx,
            rx: parking_lot::Mutex::new(rx),
        }
    }

    /// Send a command and wait for result (called from async context)
    fn execute(&self, cmd: HostCommand) -> HostCommandResult {
        let (result_tx, result_rx) = mpsc::channel();

        // Send command to queue
        if self.tx.send((cmd, result_tx)).is_err() {
            // Channel closed - return error
            return HostCommandResult::Transport(TransportResult::Error {
                message: "Command queue closed".to_string(),
            });
        }

        // Wait for result from main thread
        match result_rx.recv() {
            Ok(result) => result,
            Err(_) => HostCommandResult::Transport(TransportResult::Error {
                message: "Failed to receive command result".to_string(),
            }),
        }
    }
}

/// Initialize the global command queue
pub fn init_command_queue() {
    let _ = COMMAND_QUEUE.set(CommandQueue::new());
}

/// Process pending commands on the main thread (called from timer callback)
pub fn process_host_commands() {
    let Some(queue) = COMMAND_QUEUE.get() else {
        return;
    };

    let rx = queue.rx.lock();

    // Process all pending commands
    while let Ok((cmd, result_tx)) = rx.try_recv() {
        let result = execute_on_main_thread(cmd);
        let _ = result_tx.send(result);
    }
}

/// Execute a command on the main thread (has REAPER API access)
fn execute_on_main_thread(cmd: HostCommand) -> HostCommandResult {
    use reaper_high::Reaper;
    use reaper_medium::{CommandId, PositionInSeconds, ProjectContext, SetEditCurPosOptions, MarkerOrRegionPosition};

    let reaper = Reaper::get();
    let medium = reaper.medium_reaper();
    let project = reaper.current_project();

    match cmd {
        // =====================================================================
        // Transport Commands
        // =====================================================================
        HostCommand::Play => {
            debug!("HostService::play() - executing REAPER command");
            medium.main_on_command_ex(CommandId::new(1007), 0, ProjectContext::CurrentProject);
            info!("▶️  Transport: Play");
            HostCommandResult::Transport(TransportResult::Success)
        }

        HostCommand::Stop => {
            debug!("HostService::stop() - executing REAPER command");
            medium.main_on_command_ex(CommandId::new(1016), 0, ProjectContext::CurrentProject);
            info!("⏹️  Transport: Stop");
            HostCommandResult::Transport(TransportResult::Success)
        }

        HostCommand::Record => {
            debug!("HostService::record() - executing REAPER command");
            medium.main_on_command_ex(CommandId::new(1013), 0, ProjectContext::CurrentProject);
            info!("⏺️  Transport: Record");
            HostCommandResult::Transport(TransportResult::Success)
        }

        HostCommand::GetState => {
            let play_state = medium.get_play_state_ex(ProjectContext::CurrentProject);
            let position = project.play_or_edit_cursor_position().unwrap_or_default();
            let tempo = project.tempo().bpm().get();

            HostCommandResult::PlaybackState(PlaybackState {
                is_playing: play_state.is_playing,
                is_recording: play_state.is_recording,
                position_seconds: position.get(),
                tempo_bpm: tempo,
            })
        }

        HostCommand::SetPosition(seconds) => {
            debug!("HostService::set_position({}) - executing REAPER command", seconds);
            match PositionInSeconds::new(seconds) {
                Ok(pos) => {
                    project.set_edit_cursor_position(
                        pos,
                        SetEditCurPosOptions {
                            move_view: false,
                            seek_play: true,
                        },
                    );
                    info!("⏩ Transport: Set position to {}s", seconds);
                    HostCommandResult::Transport(TransportResult::Success)
                }
                Err(_) => {
                    warn!("Invalid position: {}", seconds);
                    HostCommandResult::Transport(TransportResult::Error {
                        message: format!("Invalid position: {}", seconds),
                    })
                }
            }
        }

        // =====================================================================
        // Logger Commands
        // =====================================================================
        HostCommand::Log(message) => {
            info!("[Extension Log] {}", message);
            reaper.show_console_msg(format!("[Extension] {}\n", message));
            HostCommandResult::Logger(LoggerResult::Success)
        }

        // =====================================================================
        // Marker Commands
        // =====================================================================
        HostCommand::AddMarker { position, name } => {
            debug!("HostService::add_marker() at {}s, name={}", position, name);
            match PositionInSeconds::new(position) {
                Ok(pos) => {
                    match medium.add_project_marker_2(
                        ProjectContext::CurrentProject,
                        MarkerOrRegionPosition::Marker(pos),
                        &*name,
                        None,
                        None,
                    ) {
                        Ok(marker_index) => {
                            info!("📍 Added marker '{}' at {}s (index: {})", name, position, marker_index);
                            HostCommandResult::Marker(MarkerResult::Success {
                                marker: Marker {
                                    index: marker_index as usize,
                                    position,
                                    name,
                                },
                            })
                        }
                        Err(e) => {
                            warn!("Failed to add marker: {:?}", e);
                            HostCommandResult::Marker(MarkerResult::Error {
                                message: format!("Failed to add marker: {:?}", e),
                            })
                        }
                    }
                }
                Err(_) => {
                    warn!("Invalid marker position: {}", position);
                    HostCommandResult::Marker(MarkerResult::Error {
                        message: format!("Invalid position: {}", position),
                    })
                }
            }
        }

        HostCommand::GetMarkers => {
            debug!("HostService::get_markers()");
            let mut markers = Vec::new();
            let mut idx = 0u32;

            loop {
                let mut found_marker = false;
                medium.enum_project_markers_3(ProjectContext::CurrentProject, idx, |result| {
                    if let Some(info) = result {
                        if info.region_end_position.is_none() {
                            markers.push(Marker {
                                index: idx as usize,
                                position: info.position.get(),
                                name: info.name.to_string(),
                            });
                        }
                        found_marker = true;
                    }
                });

                if !found_marker {
                    break;
                }
                idx += 1;
            }

            HostCommandResult::Marker(MarkerResult::SuccessList { markers })
        }

        HostCommand::DeleteMarker(index) => {
            debug!("HostService::delete_marker({})", index);
            let result = unsafe {
                medium.low().DeleteProjectMarker(
                    ProjectContext::CurrentProject.to_raw(),
                    index as i32,
                    false,
                )
            };

            if result {
                info!("🗑️  Deleted marker at index {}", index);
                HostCommandResult::Marker(MarkerResult::Success {
                    marker: Marker {
                        index,
                        position: 0.0,
                        name: String::new(),
                    },
                })
            } else {
                warn!("Failed to delete marker at index {}", index);
                HostCommandResult::Marker(MarkerResult::Error {
                    message: format!("Marker at index {} not found", index),
                })
            }
        }

        // =====================================================================
        // Track Commands
        // =====================================================================
        HostCommand::GetTrackCount => {
            HostCommandResult::TrackCount(project.track_count() as usize)
        }

        HostCommand::GetTrack(index) => {
            match project.track_by_index(index as u32) {
                Some(track) => {
                    let name = track.name().map(|n| n.to_string()).unwrap_or_default();
                    let is_selected = track.is_selected();
                    HostCommandResult::Track(TrackResult::Success {
                        track: Track { index, name, is_selected },
                    })
                }
                None => HostCommandResult::Track(TrackResult::Error {
                    message: format!("Track {} not found", index),
                }),
            }
        }

        HostCommand::GetAllTracks => {
            let track_count = project.track_count();
            let mut tracks = Vec::with_capacity(track_count as usize);

            for i in 0..track_count {
                if let Some(track) = project.track_by_index(i) {
                    let name = track.name().map(|n| n.to_string()).unwrap_or_default();
                    let is_selected = track.is_selected();
                    tracks.push(Track {
                        index: i as usize,
                        name,
                        is_selected,
                    });
                }
            }

            HostCommandResult::Track(TrackResult::SuccessList { tracks })
        }

        HostCommand::SetTrackName { index, name } => {
            match project.track_by_index(index as u32) {
                Some(track) => {
                    track.set_name(&*name);
                    let is_selected = track.is_selected();
                    info!("🏷️  Set track {} name to '{}'", index, name);
                    HostCommandResult::Track(TrackResult::Success {
                        track: Track { index, name, is_selected },
                    })
                }
                None => HostCommandResult::Track(TrackResult::Error {
                    message: format!("Track {} not found", index),
                }),
            }
        }
    }
}

// ============================================================================
// Host Service Implementation
// ============================================================================

/// Unified host service implementation
///
/// Implements DAW services by queuing commands for main-thread execution.
/// Extensions call these methods via RPC, commands are queued, and
/// the timer callback processes them on the main thread.
#[derive(Clone)]
pub struct HostServiceImpl {
    /// Connection handles per peer ID
    connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,

    /// Extension name registry (name → peer_id)
    extension_names: Arc<RwLock<HashMap<String, u8>>>,
}

impl HostServiceImpl {
    pub fn new(
        connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,
        extension_names: Arc<RwLock<HashMap<String, u8>>>,
    ) -> Self {
        Self {
            connections,
            extension_names,
        }
    }

    /// Get connection handle for a named extension
    #[allow(dead_code)]
    fn get_extension_handle(&self, name: &str) -> Option<ConnectionHandle> {
        let peer_id = self.extension_names.read().get(name).copied()?;
        self.connections.read().get(&peer_id).cloned()
    }

    /// Execute a command via the global queue
    fn execute_command(&self, cmd: HostCommand) -> HostCommandResult {
        match COMMAND_QUEUE.get() {
            Some(queue) => queue.execute(cmd),
            None => HostCommandResult::Transport(TransportResult::Error {
                message: "Command queue not initialized".to_string(),
            }),
        }
    }
}

impl HostService for HostServiceImpl {
    // =========================================================================
    // Extension Lifecycle (no REAPER API calls - safe on any thread)
    // =========================================================================

    async fn ready(&self, _cx: &Context, msg: ReadyMsg) -> ReadyAck {
        info!("✅ Extension '{}' (peer_id={}) is ready", msg.extension_name, msg.peer_id);
        self.extension_names.write().insert(msg.extension_name.clone(), msg.peer_id as u8);
        ReadyAck { ok: true }
    }

    // =========================================================================
    // DAW Transport Service (queued for main-thread execution)
    // =========================================================================

    async fn play(&self, _cx: &Context) -> TransportResult {
        match self.execute_command(HostCommand::Play) {
            HostCommandResult::Transport(r) => r,
            _ => TransportResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    async fn stop(&self, _cx: &Context) -> TransportResult {
        match self.execute_command(HostCommand::Stop) {
            HostCommandResult::Transport(r) => r,
            _ => TransportResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    async fn record(&self, _cx: &Context) -> TransportResult {
        match self.execute_command(HostCommand::Record) {
            HostCommandResult::Transport(r) => r,
            _ => TransportResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    async fn get_state(&self, _cx: &Context) -> PlaybackState {
        match self.execute_command(HostCommand::GetState) {
            HostCommandResult::PlaybackState(s) => s,
            _ => PlaybackState {
                is_playing: false,
                is_recording: false,
                position_seconds: 0.0,
                tempo_bpm: 120.0,
            },
        }
    }

    async fn set_position(&self, _cx: &Context, seconds: f64) -> TransportResult {
        match self.execute_command(HostCommand::SetPosition(seconds)) {
            HostCommandResult::Transport(r) => r,
            _ => TransportResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    async fn subscribe_transport(&self, _cx: &Context, output: Tx<PlaybackState>) {
        info!("📡 Transport stream subscription started");

        // Spawn a task to push transport state at ~60fps
        let queue_ref = COMMAND_QUEUE.get();

        tokio::spawn(async move {
            let mut interval = tokio::time::interval(std::time::Duration::from_millis(16));

            loop {
                interval.tick().await;

                // Get current transport state
                let state = if let Some(queue) = queue_ref {
                    match queue.execute(HostCommand::GetState) {
                        HostCommandResult::PlaybackState(s) => s,
                        _ => continue,
                    }
                } else {
                    break;
                };

                // Send to client - if this fails, the client disconnected
                if output.send(&state).await.is_err() {
                    debug!("Transport stream closed by client");
                    break;
                }
            }

            info!("📡 Transport stream subscription ended");
        });
    }

    // =========================================================================
    // DAW Logger Service (queued for main-thread execution)
    // =========================================================================

    async fn log(&self, _cx: &Context, message: String) -> LoggerResult {
        match self.execute_command(HostCommand::Log(message)) {
            HostCommandResult::Logger(r) => r,
            _ => LoggerResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    // =========================================================================
    // DAW Marker/Region Service (queued for main-thread execution)
    // =========================================================================

    async fn add_marker(&self, _cx: &Context, position: f64, name: String) -> MarkerResult {
        match self.execute_command(HostCommand::AddMarker { position, name }) {
            HostCommandResult::Marker(r) => r,
            _ => MarkerResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    async fn get_markers(&self, _cx: &Context) -> MarkerResult {
        match self.execute_command(HostCommand::GetMarkers) {
            HostCommandResult::Marker(r) => r,
            _ => MarkerResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    async fn delete_marker(&self, _cx: &Context, index: usize) -> MarkerResult {
        match self.execute_command(HostCommand::DeleteMarker(index)) {
            HostCommandResult::Marker(r) => r,
            _ => MarkerResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    // =========================================================================
    // DAW Track Service (queued for main-thread execution)
    // =========================================================================

    async fn get_track_count(&self, _cx: &Context) -> usize {
        match self.execute_command(HostCommand::GetTrackCount) {
            HostCommandResult::TrackCount(n) => n,
            _ => 0,
        }
    }

    async fn get_track(&self, _cx: &Context, index: usize) -> TrackResult {
        match self.execute_command(HostCommand::GetTrack(index)) {
            HostCommandResult::Track(r) => r,
            _ => TrackResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    async fn get_all_tracks(&self, _cx: &Context) -> TrackResult {
        match self.execute_command(HostCommand::GetAllTracks) {
            HostCommandResult::Track(r) => r,
            _ => TrackResult::Error { message: "Unexpected result type".to_string() },
        }
    }

    async fn set_track_name(&self, _cx: &Context, index: usize, name: String) -> TrackResult {
        match self.execute_command(HostCommand::SetTrackName { index, name }) {
            HostCommandResult::Track(r) => r,
            _ => TrackResult::Error { message: "Unexpected result type".to_string() },
        }
    }
}

/// Create the unified host service dispatcher
pub fn create_host_dispatcher(
    connections: Arc<RwLock<HashMap<u8, ConnectionHandle>>>,
    extension_names: Arc<RwLock<HashMap<String, u8>>>,
) -> host_proto::HostServiceDispatcher<HostServiceImpl> {
    host_proto::HostServiceDispatcher::new(HostServiceImpl::new(connections, extension_names))
}
