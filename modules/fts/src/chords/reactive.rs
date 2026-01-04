//! Reactive streams and services for chords state
//!
//! This module provides reactive streams and services for chords-related state changes.
//! All chords operations are scoped to a specific project.

#[cfg(not(target_arch = "wasm32"))]
pub mod irpc;

#[cfg(feature = "reaper")]
pub mod reaper;

use crate::chords::types::ChordsData;
use rxrust::prelude::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Re-export EventStreamSubject for convenience
pub type EventStreamSubject<T> = RefCell<LocalSubject<'static, T, ()>>;

/// Reactive streams for chords state
#[derive(Clone, Default, Debug)]
pub struct ChordsStreams {
    /// Chords changed for a specific project
    pub chords_changed: EventStreamSubject<(String, ChordsData)>, // project_name, chords
}

impl ChordsStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            chords_changed: default(),
        }
    }
}

/// State managed by the chords reactive service
#[derive(Debug, Clone)]
#[derive(Default)]
pub struct ChordsReactiveState {
    /// Chords for each project (project_name -> chords)
    pub chords: HashMap<String, ChordsData>,
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
            // Note: ChordsData doesn't implement Eq, so we can't compare directly
            // For now, always update (could add a custom comparison if needed)
            let changed = !state.chords.contains_key(project_name);
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

