//! REAPER implementation of chords reactive service
//!
//! This module provides a REAPER-specific implementation of the ChordsReactiveService trait.
//! It reads chords state from REAPER and updates reactive streams.
//!
//! This module is only available when the `reaper` feature is enabled.

use std::sync::{Arc, Mutex};

use rxrust::prelude::Observer;
use tracing::info;

use super::{ChordsReactiveService, ChordsReactiveState, ChordsStreams};
use crate::chords::types::ChordsData;

/// REAPER implementation of ChordsReactiveService
///
/// This service reads chords state from REAPER and emits reactive events.
/// It's designed to be used on the main thread only.
#[derive(Debug)]
pub struct ReaperChordsReactiveService {
    /// Current state
    state: Arc<Mutex<ChordsReactiveState>>,

    /// Reactive streams
    streams: ChordsStreams,
}

impl ReaperChordsReactiveService {
    /// Create a new REAPER chords reactive service
    pub fn new(streams: ChordsStreams) -> Self {
        Self {
            state: Arc::new(Mutex::new(ChordsReactiveState::default())),
            streams,
        }
    }

    /// Emit chords changed event
    fn emit_chords(&self, project_name: String, chords: ChordsData) {
        self.streams
            .chords_changed
            .borrow_mut()
            .next((project_name, chords));
    }
}

impl ChordsReactiveService for ReaperChordsReactiveService {
    fn streams(&self) -> &ChordsStreams {
        &self.streams
    }

    fn get_state(&self) -> Arc<Mutex<ChordsReactiveState>> {
        self.state.clone()
    }

    fn update_chords(&self, project_name: &str, chords: ChordsData) {
        let changed = {
            let mut state = self.state.lock().unwrap();
            let changed = state
                .chords
                .get(project_name)
                .map(|old| old != &chords)
                .unwrap_or(true);
            if changed {
                state
                    .chords
                    .insert(project_name.to_string(), chords.clone());
            }
            changed
        };

        if changed {
            info!(
                project_name = %project_name,
                chord_count = chords.chords.len(),
                "Chords changed"
            );
            self.emit_chords(project_name.to_string(), chords);
        }
    }
}
