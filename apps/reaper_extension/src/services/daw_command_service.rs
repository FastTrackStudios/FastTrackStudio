//! DAW Command Service
//!
//! Provides transport and DAW commands that execute on the main REAPER thread.
//! Extensions call these from async contexts, commands are queued and executed
//! by the timer callback on the main thread.

use daw_proto::{PlaybackState, TransportResult};
use std::sync::mpsc;
use std::sync::{Arc, Mutex};
use tracing::{error, info, warn};

/// DAW command request
#[derive(Debug, Clone)]
pub enum DawCommand {
    Play,
    Stop,
    Pause,
    Record,
    GetState,
    SetPosition(f64),
}

/// Response from DAW command execution
#[derive(Debug, Clone)]
pub enum DawResponse {
    Success,
    Error(String),
    State(PlaybackState),
}

/// Service for executing DAW commands on the main thread
#[derive(Debug, Clone)]
pub struct DawCommandService {
    /// Channel to send commands to main thread
    command_tx: Arc<Mutex<Option<mpsc::Sender<(DawCommand, mpsc::Sender<DawResponse>)>>>>,
}

impl DawCommandService {
    /// Create a new DAW command service
    pub fn new() -> Self {
        let (tx, _rx) = mpsc::channel();
        Self {
            command_tx: Arc::new(Mutex::new(Some(tx))),
        }
    }

    /// Initialize with the actual command receiver
    pub fn initialize(&self, tx: mpsc::Sender<(DawCommand, mpsc::Sender<DawResponse>)>) {
        if let Ok(mut guard) = self.command_tx.lock() {
            *guard = Some(tx);
        }
    }

    /// Execute a transport command (called from async context, e.g., extension)
    pub fn execute(&self, command: DawCommand) -> Result<DawResponse, String> {
        let (result_tx, result_rx) = mpsc::channel();

        if let Ok(guard) = self.command_tx.lock() {
            if let Some(sender) = guard.as_ref() {
                sender
                    .send((command, result_tx))
                    .map_err(|e| format!("Failed to queue command: {}", e))?;

                // Wait for execution on main thread
                result_rx
                    .recv()
                    .map_err(|e| format!("Failed to receive result: {}", e))
            } else {
                Err("Command service not initialized".to_string())
            }
        } else {
            Err("Failed to lock command channel".to_string())
        }
    }

    /// Convenience methods
    pub fn play(&self) -> TransportResult {
        match self.execute(DawCommand::Play) {
            Ok(DawResponse::Success) => TransportResult::Success,
            Ok(DawResponse::Error(msg)) => TransportResult::Error { message: msg },
            Err(e) => TransportResult::Error { message: e },
            _ => TransportResult::Error {
                message: "Unexpected response".to_string(),
            },
        }
    }

    pub fn stop(&self) -> TransportResult {
        match self.execute(DawCommand::Stop) {
            Ok(DawResponse::Success) => TransportResult::Success,
            Ok(DawResponse::Error(msg)) => TransportResult::Error { message: msg },
            Err(e) => TransportResult::Error { message: e },
            _ => TransportResult::Error {
                message: "Unexpected response".to_string(),
            },
        }
    }

    pub fn pause(&self) -> TransportResult {
        match self.execute(DawCommand::Pause) {
            Ok(DawResponse::Success) => TransportResult::Success,
            Ok(DawResponse::Error(msg)) => TransportResult::Error { message: msg },
            Err(e) => TransportResult::Error { message: e },
            _ => TransportResult::Error {
                message: "Unexpected response".to_string(),
            },
        }
    }

    pub fn record(&self) -> TransportResult {
        match self.execute(DawCommand::Record) {
            Ok(DawResponse::Success) => TransportResult::Success,
            Ok(DawResponse::Error(msg)) => TransportResult::Error { message: msg },
            Err(e) => TransportResult::Error { message: e },
            _ => TransportResult::Error {
                message: "Unexpected response".to_string(),
            },
        }
    }

    pub fn get_state(&self) -> PlaybackState {
        match self.execute(DawCommand::GetState) {
            Ok(DawResponse::State(state)) => state,
            Ok(DawResponse::Error(msg)) => {
                warn!("Failed to get state: {}", msg);
                PlaybackState {
                    is_playing: false,
                    is_paused: false,
                    is_recording: false,
                    position_seconds: 0.0,
                    tempo_bpm: 120.0,
                }
            }
            Err(e) => {
                warn!("Failed to get state: {}", e);
                PlaybackState {
                    is_playing: false,
                    is_paused: false,
                    is_recording: false,
                    position_seconds: 0.0,
                    tempo_bpm: 120.0,
                }
            }
            _ => PlaybackState {
                is_playing: false,
                is_paused: false,
                is_recording: false,
                position_seconds: 0.0,
                tempo_bpm: 120.0,
            },
        }
    }

    pub fn set_position(&self, seconds: f64) -> TransportResult {
        match self.execute(DawCommand::SetPosition(seconds)) {
            Ok(DawResponse::Success) => TransportResult::Success,
            Ok(DawResponse::Error(msg)) => TransportResult::Error { message: msg },
            Err(e) => TransportResult::Error { message: e },
            _ => TransportResult::Error {
                message: "Unexpected response".to_string(),
            },
        }
    }
}

impl Default for DawCommandService {
    fn default() -> Self {
        Self::new()
    }
}

/// Command processor - runs on main thread
pub struct DawCommandProcessor {
    command_rx: mpsc::Receiver<(DawCommand, mpsc::Sender<DawResponse>)>,
}

impl DawCommandProcessor {
    pub fn new() -> (Self, mpsc::Sender<(DawCommand, mpsc::Sender<DawResponse>)>) {
        let (tx, rx) = mpsc::channel();
        (Self { command_rx: rx }, tx)
    }

    /// Process pending commands - called from timer callback on main thread
    pub fn process_pending(&self) {
        while let Ok((command, response_tx)) = self.command_rx.try_recv() {
            let response = self.execute_command(command);
            let _ = response_tx.send(response);
        }
    }

    /// Execute command on main thread (where REAPER API is available)
    fn execute_command(&self, command: DawCommand) -> DawResponse {
        use reaper_medium::{ProjectContext, Reaper};

        // REAPER is now available on main thread
        let reaper = Reaper::get();
        let project = ProjectContext::CurrentProject;

        match command {
            DawCommand::Play => {
                info!("Executing play command on main thread");
                reaper.csurf_on_play();
                DawResponse::Success
            }
            DawCommand::Stop => {
                info!("Executing stop command on main thread");
                reaper.csurf_on_stop();
                DawResponse::Success
            }
            DawCommand::Pause => {
                info!("Executing pause command on main thread");
                reaper.csurf_on_pause();
                DawResponse::Success
            }
            DawCommand::Record => {
                info!("Executing record command on main thread");
                reaper.csurf_on_record();
                DawResponse::Success
            }
            DawCommand::GetState => {
                let state = reaper.get_play_state_ex(project);
                let position = reaper.get_play_position_ex(project);
                let tempo = reaper.master_get_tempo();

                DawResponse::State(PlaybackState {
                    is_playing: state.is_playing,
                    is_paused: state.is_paused,
                    is_recording: state.is_recording,
                    position_seconds: position.get(),
                    tempo_bpm: tempo.get(),
                })
            }
            DawCommand::SetPosition(seconds) => {
                use reaper_medium::{PositionInSeconds, SetEditCurPosOptions};
                info!("Executing set_position({}) on main thread", seconds);

                let options = SetEditCurPosOptions {
                    move_view: true,
                    seek_play: true,
                };
                reaper.set_edit_curs_pos_2(project, PositionInSeconds::new_panic(seconds), options);
                DawResponse::Success
            }
        }
    }
}
