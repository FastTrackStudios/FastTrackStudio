//! ReaperTransport - Transport service implementation for REAPER
//!
//! This service implements `TransportService` by calling REAPER APIs.
//! Commands are dispatched to the main thread for execution.

use std::sync::{Arc, RwLock};

use daw::transport::{
    PlayState, RecordMode, Tempo, Transport, TransportCommand, TransportEvent, TransportService,
};
use reaper_high::Reaper;
use reaper_medium::{CommandId, PositionInSeconds, ProjectContext, SetEditCurPosOptions};
use roam::{Context, Tx};
use tokio::sync::broadcast;
use tracing::{debug, info};

// region:    --- PlayState Conversion

/// Convert from reaper_medium::PlayState (struct with booleans) to daw::PlayState (enum)
///
/// Priority: Recording > Playing > Paused > Stopped
fn convert_play_state(reaper_state: reaper_medium::PlayState) -> PlayState {
    if reaper_state.is_recording {
        PlayState::Recording
    } else if reaper_state.is_playing {
        PlayState::Playing
    } else if reaper_state.is_paused {
        PlayState::Paused
    } else {
        PlayState::Stopped
    }
}

// endregion: --- PlayState Conversion

// region:    --- ReaperTransport

/// REAPER implementation of TransportService.
///
/// Reads transport state directly from REAPER and executes commands
/// via REAPER's action system or medium API.
///
/// Note: This struct contains runtime sync primitives (Arc, RwLock, broadcast channels)
/// that are implementation details and shouldn't be serialized. The data types it manages
/// (Transport, TransportEvent, etc.) derive Facet for RPC serialization.
pub struct ReaperTransport {
    /// Cached transport state (updated on timer callback)
    state: Arc<RwLock<Transport>>,
    /// Event broadcast channel
    event_tx: broadcast::Sender<TransportEvent>,
    /// Position broadcast channel (high frequency)
    position_tx: broadcast::Sender<f64>,
}

impl std::fmt::Debug for ReaperTransport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("ReaperTransport")
            .field("state", &"<Arc<RwLock<Transport>>>")
            .field("event_tx", &"<broadcast::Sender>")
            .field("position_tx", &"<broadcast::Sender>")
            .finish()
    }
}

impl ReaperTransport {
    /// Create a new ReaperTransport
    pub fn new() -> Self {
        let (event_tx, _) = broadcast::channel(64);
        let (position_tx, _) = broadcast::channel(256); // Higher capacity for position updates

        Self {
            state: Arc::new(RwLock::new(Transport::new())),
            event_tx,
            position_tx,
        }
    }

    /// Update state from REAPER (called from timer callback on main thread)
    ///
    /// This method reads current transport state from REAPER and broadcasts
    /// any changes to subscribers.
    pub fn update_from_reaper(&self) {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();
        let project = reaper.current_project();

        // Read current state from REAPER
        // reaper_medium::PlayState is a struct with booleans, not an enum
        let reaper_play_state = medium.get_play_state_ex(ProjectContext::CurrentProject);
        let play_state = convert_play_state(reaper_play_state);

        let position = project.play_or_edit_cursor_position().unwrap_or_default();
        let tempo_value = project.tempo().bpm().get();

        // Build new state
        let new_state = Transport {
            play_state,
            tempo: Tempo::new(tempo_value),
            playhead_position: daw::Position::from_seconds(position.get()),
            // TODO: Read other fields from REAPER
            ..Transport::new()
        };

        // Check if position changed (high frequency)
        let position_changed = {
            let current = self.state.read().unwrap();
            (current.playhead_position.time.to_seconds() - new_state.playhead_position.time.to_seconds()).abs() > 0.001
        };

        if position_changed {
            let _ = self.position_tx.send(new_state.playhead_position.time.to_seconds());
        }

        // Check if state changed
        let state_changed = {
            let current = self.state.read().unwrap();
            *current != new_state
        };

        if state_changed {
            // Determine what changed and send specific events
            let old_state = {
                let mut state = self.state.write().unwrap();
                let old = state.clone();
                *state = new_state.clone();
                old
            };

            if old_state.play_state != new_state.play_state {
                let _ = self.event_tx.send(TransportEvent::PlayStateChanged(new_state.play_state));
            }
            if old_state.tempo != new_state.tempo {
                let _ = self.event_tx.send(TransportEvent::TempoChanged(new_state.tempo));
            }
            if old_state.looping != new_state.looping {
                let _ = self.event_tx.send(TransportEvent::LoopChanged(new_state.looping));
            }

            // Always send full state change
            let _ = self.event_tx.send(TransportEvent::StateChanged(new_state));
        }
    }

    /// Execute a command on REAPER's main thread
    ///
    /// IMPORTANT: This must be called from the main thread (e.g., timer callback).
    /// For async execution, queue the command and process it in the timer.
    fn execute_command_main_thread(&self, cmd: TransportCommand) {
        let reaper = Reaper::get();
        let medium = reaper.medium_reaper();

        match cmd {
            TransportCommand::Play => {
                // REAPER command 1007: Play
                medium.main_on_command_ex(CommandId::new(1007), 0, ProjectContext::CurrentProject);
                debug!("ReaperTransport: Play");
            }
            TransportCommand::Pause => {
                // REAPER command 1008: Pause
                medium.main_on_command_ex(CommandId::new(1008), 0, ProjectContext::CurrentProject);
                debug!("ReaperTransport: Pause");
            }
            TransportCommand::Stop => {
                // REAPER command 1016: Stop
                medium.main_on_command_ex(CommandId::new(1016), 0, ProjectContext::CurrentProject);
                debug!("ReaperTransport: Stop");
            }
            TransportCommand::PlayPause => {
                // REAPER command 40073: Transport: Play/pause
                medium.main_on_command_ex(CommandId::new(40073), 0, ProjectContext::CurrentProject);
                debug!("ReaperTransport: PlayPause");
            }
            TransportCommand::PlayStop => {
                // REAPER command 40044: Transport: Play/stop
                medium.main_on_command_ex(CommandId::new(40044), 0, ProjectContext::CurrentProject);
                debug!("ReaperTransport: PlayStop");
            }
            TransportCommand::Record => {
                // REAPER command 1013: Record
                medium.main_on_command_ex(CommandId::new(1013), 0, ProjectContext::CurrentProject);
                debug!("ReaperTransport: Record");
            }
            TransportCommand::StopRecording => {
                // Stop if recording
                let play_state = medium.get_play_state_ex(ProjectContext::CurrentProject);
                if play_state.is_recording {
                    medium.main_on_command_ex(CommandId::new(1016), 0, ProjectContext::CurrentProject);
                    debug!("ReaperTransport: StopRecording");
                }
            }
            TransportCommand::ToggleRecording => {
                // REAPER command 1013: Toggle record
                medium.main_on_command_ex(CommandId::new(1013), 0, ProjectContext::CurrentProject);
                debug!("ReaperTransport: ToggleRecording");
            }
            TransportCommand::SetTempo(bpm) => {
                let project = reaper.current_project();
                let position = project.edit_cursor_position();
                // Note: Setting tempo requires more complex tempo map manipulation
                // For now, log a warning
                info!("ReaperTransport: SetTempo {} - not yet implemented", bpm);
            }
            TransportCommand::SetPosition(seconds) => {
                let project = reaper.current_project();
                if let Ok(pos) = PositionInSeconds::new(seconds) {
                    project.set_edit_cursor_position(
                        pos,
                        SetEditCurPosOptions {
                            move_view: false,
                            seek_play: true,
                        },
                    );
                    debug!("ReaperTransport: SetPosition {}", seconds);
                }
            }
            TransportCommand::SetRecordMode(_mode) => {
                // Record mode is set via REAPER's options
                info!("ReaperTransport: SetRecordMode - not yet implemented");
            }
            TransportCommand::ToggleLoop => {
                // REAPER command 1068: Toggle repeat
                medium.main_on_command_ex(CommandId::new(1068), 0, ProjectContext::CurrentProject);
                debug!("ReaperTransport: ToggleLoop");
            }
        }

        // Update state after command execution
        self.update_from_reaper();
    }

    /// Get current state (for internal use)
    pub fn current_state(&self) -> Transport {
        self.state.read().unwrap().clone()
    }
}

impl Default for ReaperTransport {
    fn default() -> Self {
        Self::new()
    }
}

// endregion: --- ReaperTransport

// region:    --- TransportService Implementation

impl TransportService for ReaperTransport {
    async fn get_state(&self, _cx: &Context) -> Transport {
        self.state.read().unwrap().clone()
    }

    async fn execute(&self, _cx: &Context, cmd: TransportCommand) {
        // IMPORTANT: REAPER APIs must be called from main thread.
        // For now, we execute directly. In production, this should queue
        // the command for execution by the timer callback.
        //
        // TODO: Implement command queuing for thread safety
        self.execute_command_main_thread(cmd);
    }

    async fn subscribe(&self, _cx: &Context, events: Tx<TransportEvent>) {
        let mut rx = self.event_tx.subscribe();

        tokio::spawn(async move {
            while let Ok(event) = rx.recv().await {
                if events.send(&event).await.is_err() {
                    break;
                }
            }
        });
    }

    async fn subscribe_position(&self, _cx: &Context, positions: Tx<f64>) {
        let mut rx = self.position_tx.subscribe();

        tokio::spawn(async move {
            while let Ok(pos) = rx.recv().await {
                if positions.send(&pos).await.is_err() {
                    break;
                }
            }
        });
    }

    async fn get_play_state(&self, _cx: &Context) -> PlayState {
        self.state.read().unwrap().play_state
    }

    async fn get_tempo(&self, _cx: &Context) -> Tempo {
        self.state.read().unwrap().tempo
    }

    async fn get_position(&self, _cx: &Context) -> f64 {
        self.state.read().unwrap().playhead_position.time.to_seconds()
    }

    async fn is_playing(&self, _cx: &Context) -> bool {
        matches!(
            self.state.read().unwrap().play_state,
            PlayState::Playing | PlayState::Recording
        )
    }

    async fn is_recording(&self, _cx: &Context) -> bool {
        self.state.read().unwrap().play_state == PlayState::Recording
    }
}

// endregion: --- TransportService Implementation
