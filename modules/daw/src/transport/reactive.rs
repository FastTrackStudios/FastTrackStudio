//! Reactive streams and services for transport state
//!
//! This module provides reactive streams and services for transport-related state changes.
//! All transport operations are scoped to a specific project.

use crate::project::Project;
use crate::transport::{PlayState, Tempo, Transport};
use rxrust::prelude::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Re-export EventStreamSubject for convenience
pub type EventStreamSubject<T> = RefCell<LocalSubject<'static, T, ()>>;

/// Reactive streams for transport state
#[derive(Clone, Default, Debug)]
pub struct TransportStreams {
    /// Transport state changed for a specific project
    pub transport_changed: EventStreamSubject<(String, Transport)>,

    /// Play state changed
    pub play_state_changed: EventStreamSubject<(String, PlayState)>,

    /// Tempo changed
    pub tempo_changed: EventStreamSubject<(String, Tempo)>,

    /// Position changed (playhead)
    pub position_changed: EventStreamSubject<(String, f64)>,
}

impl TransportStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            transport_changed: default(),
            play_state_changed: default(),
            tempo_changed: default(),
            position_changed: default(),
        }
    }
}

/// State managed by the transport reactive service
#[derive(Debug, Clone, Default)]
pub struct TransportReactiveState {
    /// Transport state for each project (project_name -> transport)
    pub transport: HashMap<String, Transport>,
}

/// Trait for transport reactive service implementations
///
/// Different DAW backends (REAPER, etc.) can implement this trait
/// to provide reactive streams using their own event systems.
///
/// Note: This trait is not `Send` or `Sync` because reactive streams
/// use `Rc<RefCell<...>>` internally and are designed for single-threaded use.
/// Implementations should only be used on the main thread.
pub trait TransportReactiveService {
    /// Get the reactive streams
    fn streams(&self) -> &TransportStreams;

    /// Get current state (for reading)
    fn get_state(&self) -> Arc<Mutex<TransportReactiveState>>;

    /// Update transport for a specific project
    fn update_transport(&self, project: &Project<Transport>, transport: Transport);

    /// Update play state for a specific project
    fn update_play_state(&self, project: &Project<Transport>, play_state: PlayState);

    /// Update tempo for a specific project
    fn update_tempo(&self, project: &Project<Transport>, tempo: Tempo);

    /// Update position for a specific project
    fn update_position(&self, project: &Project<Transport>, position: f64);
}

/// Default implementation of TransportReactiveService
///
/// This manages state and emits to reactive streams.
/// Backend-specific implementations can wrap this and subscribe to backend events.
#[derive(Debug)]
pub struct DefaultTransportReactiveService {
    /// Current state
    state: Arc<Mutex<TransportReactiveState>>,

    /// Reactive streams
    streams: TransportStreams,

    /// Guard to prevent nested next() calls
    emitting: Mutex<bool>,
}

impl DefaultTransportReactiveService {
    pub fn new(streams: TransportStreams) -> Self {
        Self {
            state: Arc::new(Mutex::new(TransportReactiveState::default())),
            streams,
            emitting: Mutex::new(false),
        }
    }
}

impl TransportReactiveService for DefaultTransportReactiveService {
    fn streams(&self) -> &TransportStreams {
        &self.streams
    }

    fn get_state(&self) -> Arc<Mutex<TransportReactiveState>> {
        self.state.clone()
    }

    fn update_transport(&self, project: &Project<Transport>, transport: Transport) {
        let project_id = project.id().to_string();
        let changed = {
            let mut state = self.state.lock().unwrap();
            let changed = state
                .transport
                .get(&project_id)
                .map(|old| old != &transport)
                .unwrap_or(true);
            if changed {
                state
                    .transport
                    .insert(project_id.clone(), transport.clone());
            }
            changed
        };

        if changed {
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
            self.emit_position(project_id, position);
        }
    }
}

impl DefaultTransportReactiveService {
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
            .borrow_mut()
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
            .borrow_mut()
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
            .borrow_mut()
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
            .borrow_mut()
            .next((project_id, position));
        *self.emitting.lock().unwrap() = false;
    }
}

pub mod irpc;
