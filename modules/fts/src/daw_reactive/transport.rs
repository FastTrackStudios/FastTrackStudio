//! REAPER Implementation of TransportReactiveService
//!
//! Reads transport state from REAPER and updates reactive streams.
//! This module is only available when the `reaper` feature is enabled.

use std::sync::{Arc, Mutex};

use reaper_high::{Project as ReaperProject, Reaper};
use rxrust::prelude::Observer;
use tracing::{debug, info};

use daw::project::Project;
use daw::transport::reactive::{
    TransportReactiveService, TransportReactiveState, TransportStreams,
};
use daw::transport::{PlayState, Tempo, Transport};

use super::logging::format_transport_change;
use super::SetlistProvider;
use crate::setlist::infra::reaper::ReaperTransport;

/// REAPER implementation of TransportReactiveService
pub struct ReaperTransportReactiveService<S: SetlistProvider> {
    state: Arc<Mutex<TransportReactiveState>>,
    streams: TransportStreams,
    emitting: Mutex<bool>,
    setlist_provider: Arc<S>,
}

impl<S: SetlistProvider> ReaperTransportReactiveService<S> {
    pub fn new(streams: TransportStreams, setlist_provider: Arc<S>) -> Self {
        Self {
            state: Arc::new(Mutex::new(TransportReactiveState::default())),
            streams,
            emitting: Mutex::new(false),
            setlist_provider,
        }
    }

    /// Update transport from current REAPER project
    /// Called from timer callback
    /// Only updates the active project (current REAPER project)
    pub fn update_from_reaper(&self) {
        let reaper = Reaper::get();
        let reaper_project = reaper.current_project();

        // Get the Project<Transport> from the setlist for the current REAPER project
        if let Some(project) = self
            .setlist_provider
            .get_project_from_reaper(reaper_project)
        {
            // Read transport from REAPER
            let transport_adapter = ReaperTransport::new(reaper_project);
            if let Ok(transport) = transport_adapter.read_transport() {
                // Update using the trait method (this will only emit if changed)
                self.update_transport(&project, transport);
            }
        }
    }
}

impl<S: SetlistProvider> TransportReactiveService for ReaperTransportReactiveService<S> {
    fn streams(&self) -> &TransportStreams {
        &self.streams
    }

    fn get_state(&self) -> Arc<Mutex<TransportReactiveState>> {
        self.state.clone()
    }

    fn update_transport(&self, project: &Project<Transport>, transport: Transport) {
        let project_id = project.id().to_string();
        let (changed, changes) = {
            let mut state = self.state.lock().unwrap();
            // Check if transport actually changed by comparing values
            // Track what changed for logging
            let (changed, changes) = if let Some(old_transport) = state.transport.get(&project_id) {
                // Compare actual transport values and track what changed
                let mut changes = Vec::new();
                let mut any_changed = false;

                if old_transport.play_state != transport.play_state {
                    changes.push(format!(
                        "play_state: {:?} -> {:?}",
                        old_transport.play_state, transport.play_state
                    ));
                    any_changed = true;
                }
                if old_transport.tempo != transport.tempo {
                    changes.push(format!(
                        "tempo: {} -> {}",
                        old_transport.tempo, transport.tempo
                    ));
                    any_changed = true;
                }
                let playhead_diff = (old_transport.playhead_position.time.to_seconds()
                    - transport.playhead_position.time.to_seconds())
                .abs();
                if playhead_diff > 0.001 {
                    let old_musical = old_transport.playhead_position.musical_position_string();
                    let new_musical = transport.playhead_position.musical_position_string();
                    changes.push(format!(
                        "playhead: {:.3}s ({}) -> {:.3}s ({})",
                        old_transport.playhead_position.time.to_seconds(),
                        old_musical,
                        transport.playhead_position.time.to_seconds(),
                        new_musical
                    ));
                    any_changed = true;
                }
                let edit_diff = (old_transport.edit_position.time.to_seconds()
                    - transport.edit_position.time.to_seconds())
                .abs();
                if edit_diff > 0.001 {
                    let old_musical = old_transport.edit_position.musical_position_string();
                    let new_musical = transport.edit_position.musical_position_string();
                    changes.push(format!(
                        "edit_cursor: {:.3}s ({}) -> {:.3}s ({})",
                        old_transport.edit_position.time.to_seconds(),
                        old_musical,
                        transport.edit_position.time.to_seconds(),
                        new_musical
                    ));
                    any_changed = true;
                }
                if old_transport.record_mode != transport.record_mode {
                    changes.push(format!(
                        "record_mode: {:?} -> {:?}",
                        old_transport.record_mode, transport.record_mode
                    ));
                    any_changed = true;
                }
                if old_transport.looping != transport.looping {
                    changes.push(format!(
                        "looping: {} -> {}",
                        old_transport.looping, transport.looping
                    ));
                    any_changed = true;
                }
                if (old_transport.playrate - transport.playrate).abs() > 0.001 {
                    changes.push(format!(
                        "playrate: {:.2}x -> {:.2}x",
                        old_transport.playrate, transport.playrate
                    ));
                    any_changed = true;
                }
                if old_transport.time_signature != transport.time_signature {
                    changes.push(format!(
                        "time_sig: {} -> {}",
                        old_transport.time_signature, transport.time_signature
                    ));
                    any_changed = true;
                }

                (any_changed, changes)
            } else {
                // New project - check if any other project has the same transport values
                let has_same_transport = state.transport.values().any(|existing| {
                    existing.play_state == transport.play_state
                        && existing.tempo == transport.tempo
                        && (existing.playhead_position.time.to_seconds()
                            - transport.playhead_position.time.to_seconds())
                        .abs()
                            <= 0.001
                        && (existing.edit_position.time.to_seconds()
                            - transport.edit_position.time.to_seconds())
                        .abs()
                            <= 0.001
                        && existing.record_mode == transport.record_mode
                        && existing.looping == transport.looping
                        && existing.playrate == transport.playrate
                        && existing.time_signature == transport.time_signature
                });

                if has_same_transport {
                    (false, Vec::new()) // Don't log duplicate transports
                } else {
                    // New project with unique transport - log initial state
                    (true, vec!["initial state".to_string()])
                }
            };

            if changed {
                state
                    .transport
                    .insert(project_id.clone(), transport.clone());
            }
            (changed, changes)
        };

        if changed {
            let message = format_transport_change(&changes, &project_id);
            info!("{}", message);
            self.emit_transport(project_id, transport);
        }
    }

    fn update_play_state(&self, project: &Project<Transport>, play_state: PlayState) {
        let project_id = project.id().to_string();
        let changed = {
            let state = self.state.lock().unwrap();
            state
                .transport
                .get(&project_id)
                .map(|transport| transport.play_state != play_state)
                .unwrap_or(true)
        };

        if changed {
            debug!(
                project_id = %project_id,
                play_state = ?play_state,
                "Play state changed - emitting to reactive stream"
            );
            self.emit_play_state(project_id, play_state);
        }
    }

    fn update_tempo(&self, project: &Project<Transport>, tempo: Tempo) {
        let project_id = project.id().to_string();
        let changed = {
            let state = self.state.lock().unwrap();
            state
                .transport
                .get(&project_id)
                .map(|transport| transport.tempo != tempo)
                .unwrap_or(true)
        };

        if changed {
            debug!(
                project_id = %project_id,
                tempo = %tempo,
                "Tempo changed - emitting to reactive stream"
            );
            self.emit_tempo(project_id, tempo);
        }
    }

    fn update_position(&self, project: &Project<Transport>, position: f64) {
        let project_id = project.id().to_string();
        const THRESHOLD: f64 = 0.001; // 1ms threshold

        let changed = {
            let state = self.state.lock().unwrap();
            state
                .transport
                .get(&project_id)
                .map(|transport| {
                    let current_pos = transport.playhead_position.time.to_seconds();
                    (current_pos - position).abs() > THRESHOLD
                })
                .unwrap_or(true)
        };

        if changed {
            debug!(
                project_id = %project_id,
                position = position,
                "Position changed - emitting to reactive stream"
            );
            self.emit_position(project_id, position);
        }
    }
}

impl<S: SetlistProvider> ReaperTransportReactiveService<S> {
    // Private emit methods (with guard to prevent nested calls)

    fn emit_transport(&self, project_id: String, transport: Transport) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams
            .transport_changed
            .borrow()
            .clone()
            .next((project_id, transport));
        *self.emitting.lock().unwrap() = false;
    }

    fn emit_play_state(&self, project_id: String, play_state: PlayState) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams
            .play_state_changed
            .borrow()
            .clone()
            .next((project_id, play_state));
        *self.emitting.lock().unwrap() = false;
    }

    fn emit_tempo(&self, project_id: String, tempo: Tempo) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams
            .tempo_changed
            .borrow()
            .clone()
            .next((project_id, tempo));
        *self.emitting.lock().unwrap() = false;
    }

    fn emit_position(&self, project_id: String, position: f64) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams
            .position_changed
            .borrow()
            .clone()
            .next((project_id, position));
        *self.emitting.lock().unwrap() = false;
    }
}
