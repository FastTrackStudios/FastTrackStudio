//! Track State Manager
//!
//! Efficiently caches and manages track state, similar to CSI's TrackNavigationManager.
//! Rebuilds track lists periodically rather than on every access.

use std::sync::{Arc, Mutex, OnceLock};
use std::time::{Duration, Instant};
use reaper_high::Project;
use reaper_medium::ChunkCacheHint;
use daw::tracks::{Track, parse_track_chunk};
use tracing::{debug, warn, trace};

/// Track state manager that caches track data and rebuilds on change events
pub struct TrackStateManager {
    /// Cached tracks (indexed by track index)
    tracks: Vec<Track>,
    /// Track GUIDs for change detection
    track_guids: Vec<Option<String>>,
    /// Last rebuild time
    last_rebuild: Instant,
    /// Whether the cache is dirty and needs rebuilding
    cache_dirty: bool,
    /// Number of tracks last time we checked (for change detection)
    last_track_count: usize,
}

/// Project context wrapper for comparison
#[derive(Clone, Copy, PartialEq)]
struct ProjectContext {
    // We'll use project pointer as identifier
    // In practice, we might want to use project GUID or other identifier
    _phantom: std::marker::PhantomData<*const ()>,
}

impl TrackStateManager {
    /// Create a new track state manager
    pub fn new() -> Self {
        Self {
            tracks: Vec::new(),
            track_guids: Vec::new(),
            last_rebuild: Instant::now(),
            cache_dirty: true, // Start dirty to force initial rebuild
            last_track_count: 0,
        }
    }

    /// Mark the cache as dirty (needs rebuild)
    pub fn mark_dirty(&mut self) {
        self.cache_dirty = true;
    }

    /// Get cached tracks (rebuilds if needed)
    pub fn get_tracks(&mut self, project: &Project) -> &[Track] {
        // Check if we need to rebuild
        if self.should_rebuild(project) {
            self.rebuild_tracks(project);
        }
        
        &self.tracks
    }

    /// Force a rebuild of tracks (useful when tracks are added/removed)
    pub fn force_rebuild(&mut self, project: &Project) {
        debug!("Force rebuilding track cache");
        self.rebuild_tracks(project);
    }

    /// Check if we should rebuild tracks
    fn should_rebuild(&self, project: &Project) -> bool {
        // Always rebuild if we have no tracks
        if self.tracks.is_empty() {
            return true;
        }

        // Rebuild if cache is dirty (set by change detection events)
        if self.cache_dirty {
            return true;
        }

        // Rebuild if track count changed (fallback safety check)
        let current_count = project.tracks().count();
        if current_count != self.last_track_count {
            debug!(
                track_count_changed = true,
                old_count = self.last_track_count,
                new_count = current_count,
                "Track count changed, rebuilding cache"
            );
            return true;
        }

        false
    }

    /// Rebuild the track cache
    fn rebuild_tracks(&mut self, project: &Project) {
        let start = Instant::now();
        trace!("Rebuilding track cache");
        
        let mut new_tracks = Vec::new();
        let mut new_guids = Vec::new();
        
        for (index, reaper_track) in project.tracks().enumerate() {
            // Use high-level safe API for chunk access
            let chunk = match reaper_track.chunk(65536, ChunkCacheHint::NormalMode) {
                Ok(c) => c,
                Err(e) => {
                    warn!(
                        error = %e,
                        track_index = index,
                        "Failed to get track chunk"
                    );
                    new_guids.push(None);
                    continue;
                }
            };
            
            // Convert chunk to string for parsing
            let chunk_str: String = match chunk.try_into() {
                Ok(s) => s,
                Err(_) => {
                    warn!(
                        track_index = index,
                        "Failed to convert chunk to string"
                    );
                    new_guids.push(None);
                    continue;
                }
            };
            
            // Parse chunk and convert to Track
            match parse_track_chunk(&chunk_str, index) {
                Ok(parsed) => {
                    let guid = parsed.guid.clone();
                    let selected = reaper_track.is_selected();
                    let track_index = reaper_track.index().map(|idx| idx as usize).unwrap_or(index);
                    let track = parsed.to_track(Some(track_index), selected);
                    new_tracks.push(track);
                    new_guids.push(guid);
                }
                Err(e) => {
                    warn!(
                        error = %e,
                        track_index = index,
                        "Failed to parse track chunk"
                    );
                    new_guids.push(None);
                }
            }
        }

        self.tracks = new_tracks;
        self.track_guids = new_guids;
        self.last_track_count = self.tracks.len();
        self.last_rebuild = Instant::now();
        self.cache_dirty = false; // Clear dirty flag after rebuild

        let duration = start.elapsed();
        if duration.as_millis() > 10 {
            debug!(
                track_count = self.tracks.len(),
                duration_ms = duration.as_millis(),
                "Track cache rebuilt"
            );
        } else {
            trace!(
                track_count = self.tracks.len(),
                duration_us = duration.as_micros(),
                "Track cache rebuilt"
            );
        }
    }

    /// Get a specific track by index (uses cache)
    pub fn get_track(&mut self, project: &Project, index: usize) -> Option<&Track> {
        let tracks = self.get_tracks(project);
        tracks.get(index)
    }

    /// Get track count (uses cache)
    pub fn track_count(&mut self, project: &Project) -> usize {
        self.get_tracks(project).len()
    }
}

/// Global track state manager instance
static TRACK_STATE_MANAGER: OnceLock<Arc<Mutex<TrackStateManager>>> = OnceLock::new();

/// Initialize the global track state manager
pub fn init_track_state_manager() -> Arc<Mutex<TrackStateManager>> {
    TRACK_STATE_MANAGER.get_or_init(|| {
        Arc::new(Mutex::new(TrackStateManager::new()))
    }).clone()
}

/// Get the global track state manager
pub fn get_track_state_manager() -> Arc<Mutex<TrackStateManager>> {
    init_track_state_manager()
}

/// Get cached tracks from the global manager
pub fn get_cached_tracks(project: &Project) -> Vec<Track> {
    let manager = get_track_state_manager();
    let mut mgr = manager.lock().expect("Track state manager lock poisoned");
    mgr.get_tracks(project).to_vec()
}

/// Force rebuild of track cache
pub fn force_rebuild_tracks(project: &Project) {
    let manager = get_track_state_manager();
    let mut mgr = manager.lock().expect("Track state manager lock poisoned");
    mgr.force_rebuild(project);
}

/// Mark the track cache as dirty (needs rebuild on next access)
pub fn mark_cache_dirty() {
    let manager = get_track_state_manager();
    let _guard = manager.lock();
    if let Ok(mut mgr) = _guard {
        mgr.mark_dirty();
    }
}

