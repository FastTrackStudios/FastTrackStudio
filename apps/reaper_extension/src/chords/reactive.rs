//! Reactive service for chords
//!
//! Provides reactive streams for chord updates

use std::sync::{Arc, Mutex};
use rxrust::prelude::*;
use crate::chords::types::ChordsData;

/// Reactive state for chords
#[derive(Debug, Clone)]
pub struct ChordsReactiveState {
    /// Current chords data (project_name -> chords)
    pub chords: std::collections::HashMap<String, ChordsData>,
}

impl Default for ChordsReactiveState {
    fn default() -> Self {
        Self {
            chords: std::collections::HashMap::new(),
        }
    }
}

/// Reactive streams for chords
#[derive(Debug, Clone)]
pub struct ChordsStreams {
    /// Stream for chords changed events
    pub chords_changed: std::cell::RefCell<LocalSubject<'static, (String, ChordsData), ()>>,
}

impl ChordsStreams {
    pub fn new() -> Self {
        Self {
            chords_changed: std::cell::RefCell::new(LocalSubject::new()),
        }
    }
}

/// Trait for chords reactive service implementations
/// 
/// Different backends (REAPER, etc.) can implement this trait
/// to provide reactive streams using their own event systems.
/// 
/// Note: This trait is not `Send` or `Sync` because reactive streams
/// use `Rc<RefCell<...>>` internally and are designed for single-threaded use.
/// Implementations should only be used on the main thread.
pub trait ChordsReactiveService {
    /// Get the reactive streams
    fn streams(&self) -> &ChordsStreams;
    
    /// Get current state (for reading)
    fn get_state(&self) -> Arc<Mutex<ChordsReactiveState>>;
    
    /// Update chords for a specific project
    fn update_chords(&self, project_name: &str, chords: ChordsData);
}

/// Default implementation of ChordsReactiveService
/// 
/// This manages state and emits to reactive streams.
/// Backend-specific implementations can wrap this and subscribe to backend events.
#[derive(Debug)]
pub struct DefaultChordsReactiveService {
    /// Current state
    state: Arc<Mutex<ChordsReactiveState>>,
    
    /// Reactive streams
    streams: ChordsStreams,
}

impl DefaultChordsReactiveService {
    pub fn new(streams: ChordsStreams) -> Self {
        Self {
            state: Arc::new(Mutex::new(ChordsReactiveState::default())),
            streams,
        }
    }
}

impl ChordsReactiveService for DefaultChordsReactiveService {
    fn streams(&self) -> &ChordsStreams {
        &self.streams
    }
    
    fn get_state(&self) -> Arc<Mutex<ChordsReactiveState>> {
        self.state.clone()
    }
    
    fn update_chords(&self, project_name: &str, chords: ChordsData) {
        let changed = {
            let mut state = self.state.lock().unwrap();
            let changed = state.chords.get(project_name).map(|old| old != &chords).unwrap_or(true);
            if changed {
                state.chords.insert(project_name.to_string(), chords.clone());
            }
            changed
        };
        
        if changed {
            self.streams.chords_changed.borrow_mut().next((project_name.to_string(), chords));
        }
    }
}

