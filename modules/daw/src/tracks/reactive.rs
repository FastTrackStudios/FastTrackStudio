//! Reactive streams and services for track state
//!
//! This module provides reactive streams and services for track-related state changes.
//! All track operations are scoped to a specific project.

use crate::tracks::Track;
use crate::project::Project;
use crate::transport::Transport;
use rxrust::prelude::*;
use std::cell::RefCell;
use std::collections::HashMap;
use std::sync::{Arc, Mutex};

/// Re-export EventStreamSubject for convenience
pub type EventStreamSubject<T> = RefCell<LocalSubject<'static, T, ()>>;

/// Reactive streams for track state
#[derive(Clone, Default, Debug)]
pub struct TrackStreams {
    /// Tracks changed for a specific project
    pub tracks_changed: EventStreamSubject<(String, Vec<Track>)>,
    
    /// A single track changed
    pub track_changed: EventStreamSubject<(String, usize, Track)>, // project_name, track_index, track
    
    /// Track added
    pub track_added: EventStreamSubject<(String, usize, Track)>, // project_name, track_index, track
    
    /// Track removed
    pub track_removed: EventStreamSubject<(String, usize)>, // project_name, track_index
}

impl TrackStreams {
    pub fn new() -> Self {
        fn default<T>() -> EventStreamSubject<T> {
            RefCell::new(LocalSubject::new())
        }
        Self {
            tracks_changed: default(),
            track_changed: default(),
            track_added: default(),
            track_removed: default(),
        }
    }
}

/// State managed by the track reactive service
#[derive(Debug, Clone)]
pub struct TrackReactiveState {
    /// Tracks for each project (project_name -> tracks)
    pub tracks: HashMap<String, Vec<Track>>,
}

impl Default for TrackReactiveState {
    fn default() -> Self {
        Self {
            tracks: HashMap::new(),
        }
    }
}

/// Trait for track reactive service implementations
/// 
/// Different DAW backends (REAPER, etc.) can implement this trait
/// to provide reactive streams using their own event systems.
/// 
/// Note: This trait is not `Send` or `Sync` because reactive streams
/// use `Rc<RefCell<...>>` internally and are designed for single-threaded use.
/// Implementations should only be used on the main thread.
pub trait TrackReactiveService {
    /// Get the reactive streams
    fn streams(&self) -> &TrackStreams;
    
    /// Get current state (for reading)
    fn get_state(&self) -> Arc<Mutex<TrackReactiveState>>;
    
    /// Update tracks for a specific project
    fn update_tracks(&self, project: &Project<Transport>, tracks: Vec<Track>);
    
    /// Update a single track for a specific project
    fn update_track(&self, project: &Project<Transport>, track_index: usize, track: Track);
    
    /// Add a track to a specific project
    fn add_track(&self, project: &Project<Transport>, track_index: usize, track: Track);
    
    /// Remove a track from a specific project
    fn remove_track(&self, project: &Project<Transport>, track_index: usize);
}

/// Default implementation of TrackReactiveService
/// 
/// This manages state and emits to reactive streams.
/// Backend-specific implementations can wrap this and subscribe to backend events.
#[derive(Debug)]
pub struct DefaultTrackReactiveService {
    /// Current state
    state: Arc<Mutex<TrackReactiveState>>,
    
    /// Reactive streams
    streams: TrackStreams,
    
    /// Guard to prevent nested next() calls
    emitting: Mutex<bool>,
}

impl DefaultTrackReactiveService {
    pub fn new(streams: TrackStreams) -> Self {
        Self {
            state: Arc::new(Mutex::new(TrackReactiveState::default())),
            streams,
            emitting: Mutex::new(false),
        }
    }
}

impl TrackReactiveService for DefaultTrackReactiveService {
    fn streams(&self) -> &TrackStreams {
        &self.streams
    }
    
    fn get_state(&self) -> Arc<Mutex<TrackReactiveState>> {
        self.state.clone()
    }
    
    fn update_tracks(&self, project: &Project<Transport>, tracks: Vec<Track>) {
        let project_id = project.id().to_string();
        let changed = {
            let mut state = self.state.lock().unwrap();
            let changed = state.tracks.get(&project_id).map(|old| old != &tracks).unwrap_or(true);
            if changed {
                state.tracks.insert(project_id.clone(), tracks.clone());
            }
            changed
        };
        
        if changed {
            self.emit_tracks(project_id, tracks);
        }
    }
    
    fn update_track(&self, project: &Project<Transport>, track_index: usize, track: Track) {
        let project_id = project.id().to_string();
        let changed = {
            let mut state = self.state.lock().unwrap();
            let changed = state.tracks.get(&project_id)
                .and_then(|tracks| tracks.get(track_index))
                .map(|old| old != &track)
                .unwrap_or(true);
            if changed {
                if let Some(tracks) = state.tracks.get_mut(&project_id) {
                    if track_index < tracks.len() {
                        tracks[track_index] = track.clone();
                    }
                }
            }
            changed
        };
        
        if changed {
            self.emit_track(project_id, track_index, track);
        }
    }
    
    fn add_track(&self, project: &Project<Transport>, track_index: usize, track: Track) {
        let project_id = project.id().to_string();
        {
            let mut state = self.state.lock().unwrap();
            let tracks = state.tracks.entry(project_id.clone()).or_insert_with(Vec::new);
            if track_index <= tracks.len() {
                tracks.insert(track_index, track.clone());
            }
        }
        
        self.emit_track_added(project_id, track_index, track);
    }
    
    fn remove_track(&self, project: &Project<Transport>, track_index: usize) {
        let project_id = project.id().to_string();
        {
            let mut state = self.state.lock().unwrap();
            if let Some(tracks) = state.tracks.get_mut(&project_id) {
                if track_index < tracks.len() {
                    tracks.remove(track_index);
                }
            }
        }
        
        self.emit_track_removed(project_id, track_index);
    }
}

impl DefaultTrackReactiveService {
    // Private emit methods (with guard to prevent nested calls)
    
    fn emit_tracks(&self, project_id: String, tracks: Vec<Track>) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams.tracks_changed.borrow_mut().next((project_id, tracks));
        *self.emitting.lock().unwrap() = false;
    }
    
    fn emit_track(&self, project_id: String, track_index: usize, track: Track) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams.track_changed.borrow_mut().next((project_id, track_index, track));
        *self.emitting.lock().unwrap() = false;
    }
    
    fn emit_track_added(&self, project_id: String, track_index: usize, track: Track) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams.track_added.borrow_mut().next((project_id, track_index, track));
        *self.emitting.lock().unwrap() = false;
    }
    
    fn emit_track_removed(&self, project_id: String, track_index: usize) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams.track_removed.borrow_mut().next((project_id, track_index));
        *self.emitting.lock().unwrap() = false;
    }
}

pub mod irpc;

