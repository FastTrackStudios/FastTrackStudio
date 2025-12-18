//! REAPER Implementation of TrackReactiveService
//!
//! Reads track state from REAPER and updates reactive streams.

use std::sync::{Arc, Mutex};
use reaper_high::{Reaper, Project as ReaperProject};
use daw::tracks::reactive::{TrackReactiveService, TrackStreams, TrackReactiveState};
use daw::tracks::Track;
use daw::project::Project;
use daw::transport::Transport;
use crate::services::SetlistService;
use tracing::{info, debug, warn};
use rxrust::prelude::Observer;
use crate::infrastructure::formatted_logging::{format_track_change, format_track_specific_change};

/// REAPER implementation of TrackReactiveService
pub struct ReaperTrackReactiveService {
    state: Arc<Mutex<TrackReactiveState>>,
    streams: TrackStreams,
    emitting: Mutex<bool>,
    setlist_service: Arc<SetlistService>,
}

impl ReaperTrackReactiveService {
    pub fn new(streams: TrackStreams, setlist_service: Arc<SetlistService>) -> Self {
        Self {
            state: Arc::new(Mutex::new(TrackReactiveState::default())),
            streams,
            emitting: Mutex::new(false),
            setlist_service,
        }
    }
    
    /// Update all tracks from current REAPER project
    /// Called from change detection middleware
    pub fn update_tracks_from_reaper(&self, reaper_project: ReaperProject) {
        // Get the Project<Transport> from the setlist
        if let Some(project) = self.get_project_from_reaper(reaper_project) {
            // Read tracks from REAPER
            let tracks = fts::setlist::infra::reaper::get_all_tracks(&reaper_project);
            // Update using the trait method (this will only emit if changed)
            self.update_tracks(&project, tracks);
        }
    }
    
    /// Update a single track from REAPER
    pub fn update_track_from_reaper(&self, reaper_project: ReaperProject, track_index: usize) {
        info!(
            track_index,
            "update_track_from_reaper called - looking up project"
        );
        // Get the Project<Transport> from the setlist
        if let Some(project) = self.get_project_from_reaper(reaper_project) {
            info!(
                project_id = %project.id(),
                track_index,
                "Project found in setlist, proceeding with track update"
            );
            // Check if tracks vector exists and is large enough
            let needs_full_update = {
                let state = self.state.lock().unwrap();
                let project_id = project.id();
                state.tracks.get(project_id)
                    .map(|tracks| track_index >= tracks.len())
                    .unwrap_or(true) // If tracks don't exist, we need to read all
            };
            
            // If tracks vector doesn't exist or index is out of bounds, read all tracks first
            if needs_full_update {
                self.update_tracks_from_reaper(reaper_project);
            } else {
                // Read the specific track from REAPER
                if let Some(track) = fts::setlist::infra::reaper::get_track(&reaper_project, track_index) {
                    info!(
                        project_id = %project.id(),
                        track_index,
                        track_name = %track.name,
                        "Read track from REAPER, calling update_track"
                    );
                    // Update using the trait method (this will only emit if changed)
                    self.update_track(&project, track_index, track);
                } else {
                    warn!(
                        project_id = %project.id(),
                        track_index,
                        "Failed to read track from REAPER"
                    );
                }
            }
        } else {
            // Project not found in setlist - log this for debugging
            let medium_reaper = Reaper::get().medium_reaper();
            let project_name = {
                let mut found_name = None;
                for i in 0..128u32 {
                    if let Some(result) = medium_reaper.enum_projects(reaper_medium::ProjectRef::Tab(i), 512) {
                        if result.project == reaper_project.raw() {
                            found_name = result.file_path.as_ref()
                                .and_then(|p| p.as_std_path().file_stem())
                                .and_then(|s| s.to_str())
                                .map(|s| s.to_string());
                            break;
                        }
                    }
                }
                found_name
            };
            warn!(
                project_name = ?project_name,
                track_index,
                "Track update skipped: project not found in setlist"
            );
        }
    }
    
    /// Get the Project<Transport> from the setlist by matching REAPER project
    fn get_project_from_reaper(&self, reaper_project: ReaperProject) -> Option<Project<Transport>> {
        // Get setlist
        let setlist_api = self.setlist_service.get_setlist()?;
        let setlist = setlist_api.get_setlist();
        
        // Get project name from REAPER
        let medium_reaper = Reaper::get().medium_reaper();
        let project_name = {
            let mut found_name = None;
            for i in 0..128u32 {
                if let Some(result) = medium_reaper.enum_projects(reaper_medium::ProjectRef::Tab(i), 512) {
                    if result.project == reaper_project.raw() {
                        found_name = result.file_path.as_ref()
                            .and_then(|p| p.as_std_path().file_stem())
                            .and_then(|s| s.to_str())
                            .map(|s| s.to_string());
                        break;
                    }
                }
            }
            found_name
        };
        
        // Find matching song in setlist
        if let Some(project_name) = project_name {
            for song in &setlist.songs {
                if song.project_name_from_metadata() == project_name {
                    return song.project.clone();
                }
            }
        }
        
        None
    }
}

impl TrackReactiveService for ReaperTrackReactiveService {
    fn streams(&self) -> &TrackStreams {
        &self.streams
    }
    
    fn get_state(&self) -> Arc<Mutex<TrackReactiveState>> {
        self.state.clone()
    }
    
    fn update_tracks(&self, project: &Project<Transport>, tracks: Vec<Track>) {
        let project_id = project.id().to_string();
        let (changed, changes) = {
            let mut state = self.state.lock().unwrap();
            let (changed, changes) = if let Some(old_tracks) = state.tracks.get(&project_id) {
                // Compare tracks and track what changed
                let mut changes = Vec::new();
                
                if old_tracks.len() != tracks.len() {
                    changes.push(format!("count: {} → {}", old_tracks.len(), tracks.len()));
                }
                
                // Check for track changes (name, volume, pan, mute, solo, etc.)
                let mut track_changes = Vec::new();
                for (i, (old, new)) in old_tracks.iter().zip(tracks.iter()).enumerate() {
                    if old != new {
                        let mut track_change_parts = Vec::new();
                        if old.name != new.name {
                            track_change_parts.push(format!("name: {} → {}", old.name, new.name));
                        }
                        if (old.volume - new.volume).abs() > 0.0001 {
                            track_change_parts.push(format!("volume: {:.2} → {:.2}", old.volume, new.volume));
                        }
                        if (old.pan - new.pan).abs() > 0.0001 {
                            track_change_parts.push(format!("pan: {:.2} → {:.2}", old.pan, new.pan));
                        }
                        if old.muted != new.muted {
                            track_change_parts.push(format!("muted: {} → {}", old.muted, new.muted));
                        }
                        if old.solo_state != new.solo_state {
                            track_change_parts.push(format!("solo: {:?} → {:?}", old.solo_state, new.solo_state));
                        }
                        if old.record_armed != new.record_armed {
                            track_change_parts.push(format!("armed: {} → {}", old.record_armed, new.record_armed));
                        }
                        
                        if !track_change_parts.is_empty() {
                            track_changes.push(format!("track[{}] {} ({})", i, new.name, track_change_parts.join(", ")));
                        }
                    }
                }
                
                // Check for new tracks (beyond old_tracks.len())
                for (i, new_track) in tracks.iter().enumerate().skip(old_tracks.len()) {
                    track_changes.push(format!("track[{}] {} (new)", i, new_track.name));
                }
                
                if !track_changes.is_empty() {
                    changes.extend(track_changes);
                }
                
                let any_changed = !changes.is_empty();
                (any_changed, changes)
            } else {
                // New project - initial tracks
                (true, vec![format!("initial tracks: {}", tracks.len())])
            };
            
            if changed {
                state.tracks.insert(project_id.clone(), tracks.clone());
            }
            (changed, changes)
        };
        
        if changed {
            let message = format_track_change(&changes, &project_id);
            info!("{}", message);
            self.emit_tracks(project_id, tracks);
        }
    }
    
    fn update_track(&self, project: &Project<Transport>, track_index: usize, track: Track) {
        let project_id = project.id().to_string();
        
        // Log what we're trying to update
        debug!(
            project_id = %project_id,
            track_index,
            track_name = %track.name,
            track_color = ?track.color,
            "update_track called - track data from REAPER"
        );
        
        let (changed, changes) = {
            let mut state = self.state.lock().unwrap();
            // Get or create tracks vector for this project
            let tracks = state.tracks.entry(project_id.clone()).or_insert_with(Vec::new);
            
            // Check if track actually changed and track what changed
            let (changed, changes) = if track_index < tracks.len() {
                // Track exists - compare values and track what changed
                let old_track = &tracks[track_index];
                let mut changes = Vec::new();
                
                if old_track.name != track.name {
                    info!(
                        project_id = %project_id,
                        track_index,
                        old_name = %old_track.name,
                        new_name = %track.name,
                        "Track name change detected in update_track"
                    );
                    changes.push(format!("name: {} → {}", old_track.name, track.name));
                }
                if (old_track.volume - track.volume).abs() > 0.0001 {
                    changes.push(format!("volume: {:.2} → {:.2}", old_track.volume, track.volume));
                }
                if (old_track.pan - track.pan).abs() > 0.0001 {
                    changes.push(format!("pan: {:.2} → {:.2}", old_track.pan, track.pan));
                }
                if old_track.muted != track.muted {
                    changes.push(format!("muted: {} → {}", old_track.muted, track.muted));
                }
                if old_track.solo_state != track.solo_state {
                    changes.push(format!("solo: {:?} → {:?}", old_track.solo_state, track.solo_state));
                }
                if old_track.record_armed != track.record_armed {
                    changes.push(format!("armed: {} → {}", old_track.record_armed, track.record_armed));
                }
                if old_track.automation_mode != track.automation_mode {
                    changes.push(format!("automation: {:?} → {:?}", old_track.automation_mode, track.automation_mode));
                }
                if old_track.locked != track.locked {
                    changes.push(format!("locked: {} → {}", old_track.locked, track.locked));
                }
                if old_track.color != track.color {
                    changes.push(format!("color: {:?} → {:?}", old_track.color, track.color));
                }
                if old_track.is_folder != track.is_folder {
                    changes.push(format!("is_folder: {} → {}", old_track.is_folder, track.is_folder));
                }
                if old_track.folder_depth_change != track.folder_depth_change {
                    changes.push(format!("folder_depth_change: {:?} → {:?}", old_track.folder_depth_change, track.folder_depth_change));
                }
                
                let any_changed = !changes.is_empty();
                (any_changed, changes)
            } else {
                // Track doesn't exist yet at this index - this is a new track
                (true, vec!["new track".to_string()])
            };
            
            if changed {
                // Track should always be in bounds at this point (update_track_from_reaper ensures this)
                if track_index < tracks.len() {
                    tracks[track_index] = track.clone();
                } else {
                    // This shouldn't happen if update_track_from_reaper is working correctly
                    // But if it does, we'll just extend the vector (this is a fallback)
                    tracks.resize(track_index + 1, track.clone());
                }
            }
            (changed, changes)
        };
        
        if changed {
            info!(
                project_id = %project_id,
                track_index,
                track_name = %track.name,
                changes = ?changes,
                "Emitting track change to reactive stream"
            );
            // Logging also happens in service layer (TrackApi subscription in irpc.rs)
            self.emit_track(project_id, track_index, track);
        } else {
            debug!(
                project_id = %project_id,
                track_index,
                track_name = %track.name,
                "Track update called but no changes detected - skipping emit"
            );
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
        
        debug!(
            project_id = %project_id,
            track_index,
            track_name = %track.name,
            "Track added - emitting to reactive stream"
        );
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
        
        debug!(
            project_id = %project_id,
            track_index,
            "Track removed - emitting to reactive stream"
        );
        self.emit_track_removed(project_id, track_index);
    }
}

impl ReaperTrackReactiveService {
    // Private emit methods (with guard to prevent nested calls)
    
    fn emit_tracks(&self, project_id: String, tracks: Vec<Track>) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams.tracks_changed.borrow().clone().next((project_id, tracks));
        *self.emitting.lock().unwrap() = false;
    }
    
    fn emit_track(&self, project_id: String, track_index: usize, track: Track) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams.track_changed.borrow().clone().next((project_id, track_index, track));
        *self.emitting.lock().unwrap() = false;
    }
    
    fn emit_track_added(&self, project_id: String, track_index: usize, track: Track) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams.track_added.borrow().clone().next((project_id, track_index, track));
        *self.emitting.lock().unwrap() = false;
    }
    
    fn emit_track_removed(&self, project_id: String, track_index: usize) {
        let mut emitting = self.emitting.lock().unwrap();
        if *emitting {
            return;
        }
        *emitting = true;
        drop(emitting);
        self.streams.track_removed.borrow().clone().next((project_id, track_index));
        *self.emitting.lock().unwrap() = false;
    }
}

