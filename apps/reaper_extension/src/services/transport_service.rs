//! Transport Service Implementation
//!
//! Provides transport control (play, stop, pause, etc.) via the centralized
//! command executor. Uses cached state for efficient reads.
//!
//! Architecture:
//! - Commands are queued via CommandExecutor (main thread execution)
//! - State is cached and updated periodically from REAPER
//! - Async methods return immediately (fire-and-forget for commands)

use reaper_high::Reaper;
use reaper_medium::ProjectContext;
use std::sync::{Arc, RwLock};
use tokio::sync::broadcast;
use tracing::{debug, error, trace};

use crate::command_executor::transport_commands as cmd;
use crate::command_executor::{CommandError, CommandExecutor};

/// Cached transport state (updated periodically from REAPER)
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TransportState {
    pub is_playing: bool,
    pub is_paused: bool,
    pub is_recording: bool,
    pub position_seconds: f64,
    pub tempo_bpm: f64,
}

impl Default for TransportState {
    fn default() -> Self {
        Self {
            is_playing: false,
            is_paused: false,
            is_recording: false,
            position_seconds: 0.0,
            tempo_bpm: 120.0,
        }
    }
}

/// Transport service with cached state and command-based control
pub struct TransportService {
    /// Command executor for REAPER API calls
    executor: CommandExecutor,

    /// Cached state (updated by timer callback)
    state: Arc<RwLock<TransportState>>,

    /// Broadcast channel for state changes
    state_tx: broadcast::Sender<TransportState>,
}

impl TransportService {
    /// Create a new transport service
    pub fn new(executor: CommandExecutor) -> Self {
        let (state_tx, _) = broadcast::channel(16);

        Self {
            executor,
            state: Arc::new(RwLock::new(TransportState::default())),
            state_tx,
        }
    }

    // ============================================================
    // Command Methods (queued for main thread execution)
    // ============================================================

    /// Start playback
    pub fn play(&self) -> Result<(), CommandError> {
        trace!("Queueing play command");
        self.executor.submit(cmd::Play)
    }

    /// Stop playback
    pub fn stop(&self) -> Result<(), CommandError> {
        trace!("Queueing stop command");
        self.executor.submit(cmd::Stop)
    }

    /// Pause playback
    pub fn pause(&self) -> Result<(), CommandError> {
        trace!("Queueing pause command");
        self.executor.submit(cmd::Pause)
    }

    /// Toggle play/pause
    pub fn toggle_play_pause(&self) -> Result<(), CommandError> {
        trace!("Queueing toggle play/pause command");
        self.executor.submit(cmd::TogglePlayPause)
    }

    /// Toggle play/stop
    pub fn toggle_play_stop(&self) -> Result<(), CommandError> {
        trace!("Queueing toggle play/stop command");
        self.executor.submit(cmd::TogglePlayStop)
    }

    /// Start recording
    pub fn record(&self) -> Result<(), CommandError> {
        trace!("Queueing record command");
        self.executor.submit(cmd::Record)
    }

    /// Stop recording (only if currently recording)
    pub fn stop_recording(&self) -> Result<(), CommandError> {
        trace!("Queueing stop recording command");
        self.executor.submit(cmd::StopRecording)
    }

    /// Set play/edit cursor position
    pub fn set_position(&self, position_seconds: f64) -> Result<(), CommandError> {
        trace!("Queueing set position command: {}s", position_seconds);
        self.executor.submit(cmd::SetPosition {
            position_seconds,
            move_view: false,
            seek_play: true,
        })
    }

    /// Set position with options
    pub fn set_position_with_options(
        &self,
        position_seconds: f64,
        move_view: bool,
        seek_play: bool,
    ) -> Result<(), CommandError> {
        self.executor.submit(cmd::SetPosition {
            position_seconds,
            move_view,
            seek_play,
        })
    }

    /// Go to start of project
    pub fn go_to_start(&self) -> Result<(), CommandError> {
        self.executor.submit(cmd::GoToStart)
    }

    /// Go to end of project
    pub fn go_to_end(&self) -> Result<(), CommandError> {
        self.executor.submit(cmd::GoToEnd)
    }

    /// Toggle loop mode
    pub fn toggle_loop(&self) -> Result<(), CommandError> {
        self.executor.submit(cmd::ToggleLoop)
    }

    // ============================================================
    // Query Methods (read from cached state)
    // ============================================================

    /// Check if currently playing
    pub fn is_playing(&self) -> bool {
        self.state.read().map(|s| s.is_playing).unwrap_or(false)
    }

    /// Check if currently paused
    pub fn is_paused(&self) -> bool {
        self.state.read().map(|s| s.is_paused).unwrap_or(false)
    }

    /// Check if currently recording
    pub fn is_recording(&self) -> bool {
        self.state.read().map(|s| s.is_recording).unwrap_or(false)
    }

    /// Get current position in seconds
    pub fn get_position(&self) -> f64 {
        self.state.read().map(|s| s.position_seconds).unwrap_or(0.0)
    }

    /// Get current tempo in BPM
    pub fn get_tempo(&self) -> f64 {
        self.state.read().map(|s| s.tempo_bpm).unwrap_or(120.0)
    }

    /// Get full transport state
    pub fn get_state(&self) -> TransportState {
        self.state.read().map(|s| *s).unwrap_or_default()
    }

    /// Subscribe to state changes
    pub fn subscribe(&self) -> broadcast::Receiver<TransportState> {
        self.state_tx.subscribe()
    }

    // ============================================================
    // State Update (called from timer callback)
    // ============================================================

    /// Update cached state from REAPER (called on main thread)
    pub fn update_from_reaper(&self) {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let project = reaper.current_project();

        // Read current state from REAPER
        let play_state = medium.get_play_state_ex(ProjectContext::CurrentProject);
        let position = project.play_or_edit_cursor_position().unwrap_or_default();
        let tempo = project.tempo().bpm().get();

        let new_state = TransportState {
            is_playing: play_state.is_playing,
            is_paused: play_state.is_paused,
            is_recording: play_state.is_recording,
            position_seconds: position.get(),
            tempo_bpm: tempo.get(),
        };

        // Check if state changed
        let should_broadcast = {
            let mut state_guard = match self.state.write() {
                Ok(guard) => guard,
                Err(e) => {
                    error!("Failed to lock state for writing: {}", e);
                    return;
                }
            };

            let changed = *state_guard != new_state;
            if changed {
                *state_guard = new_state;
            }
            changed
        };

        // Broadcast state change if needed
        if should_broadcast {
            let _ = self.state_tx.send(new_state);
            trace!("Broadcasted transport state change");
        }
    }
}

impl std::fmt::Debug for TransportService {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("TransportService")
            .field("state", &self.get_state())
            .finish()
    }
}

impl Clone for TransportService {
    fn clone(&self) -> Self {
        Self {
            executor: self.executor.clone(),
            state: Arc::clone(&self.state),
            state_tx: self.state_tx.clone(),
        }
    }
}
