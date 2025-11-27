//! REAPER Track Reading Utilities
//!
//! Utility functions for reading tracks from REAPER projects using the
//! high-level API with chunk parsing for efficient property access.
//!
//! - Uses `Project::tracks()` for track iteration (high-level, safe API)
//! - Uses `Track::chunk()` for efficient property access (safe wrapper)
//! - Provides lightweight track summaries for API efficiency (similar to CSI approach)
//! - Caches track summaries per project and only rebuilds on structure changes (like CSI)

use reaper_high::{Project, Track as ReaperTrack};
use reaper_medium::ChunkCacheHint;
use daw::tracks::{Track, parse_track_chunk};
use tracing::{warn, debug};
use serde::{Serialize, Deserialize};
use std::sync::Mutex;
use std::collections::HashMap;

/// Lightweight track summary for efficient API transmission
/// 
/// Only includes essential track information, similar to CSI's approach.
/// Excludes heavy data like items, envelopes, FX chains, receives, etc.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TrackSummary {
    /// Track name
    pub name: String,
    /// Track index in the project
    pub index: usize,
    /// Track GUID (TRACKID) - REAPER track id
    pub guid: Option<String>,
    /// Volume (0.0 to 1.0)
    pub volume: f64,
    /// Pan (-1.0 to 1.0)
    pub pan: f64,
    /// Whether the track is muted
    pub muted: bool,
    /// Solo state
    pub solo_state: daw::tracks::api::solo::SoloMode,
    /// Track color (RGB value)
    pub color: Option<u32>,
    /// Whether the track is selected
    pub selected: bool,
    /// Whether this track is a folder
    pub is_folder: bool,
    /// Track depth (for folder hierarchy)
    pub track_depth: i32,
    /// Whether track has FX enabled
    pub has_fx: bool,
    /// Whether track is armed for recording
    pub record_armed: bool,
}

impl TrackSummary {
    /// Create a lightweight summary from a full Track
    pub fn from_track(track: &Track) -> Self {
        Self {
            name: track.name.clone(),
            index: track.index.unwrap_or(0),
            guid: track.guid.clone(),
            volume: track.volume,
            pan: track.pan,
            muted: track.muted,
            solo_state: track.solo_state.clone(),
            color: track.color,
            selected: track.selected,
            is_folder: track.is_folder,
            track_depth: track.track_depth,
            has_fx: track.has_fx,
            record_armed: track.record_armed,
        }
    }
    
    /// Convert to a minimal Track (for compatibility with Project type)
    /// Only essential fields are populated - items, envelopes, FX chains, etc. are empty
    pub fn to_minimal_track(&self) -> Track {
        
        
        
        let mut track = Track::new(self.name.clone());
        track.index = Some(self.index);
        track.guid = self.guid.clone();
        track.volume = self.volume;
        track.pan = self.pan;
        track.muted = self.muted;
        track.solo_state = self.solo_state.clone();
        track.color = self.color;
        track.selected = self.selected;
        track.is_folder = self.is_folder;
        track.track_depth = self.track_depth;
        track.has_fx = self.has_fx;
        track.record_armed = self.record_armed;
        
        // Set folder state based on is_folder
        if self.is_folder {
            use daw::tracks::api::folder::{TcpFolderState, McpFolderState};
            track.folder_state_tcp = Some(TcpFolderState::Normal);
            track.folder_state_mcp = Some(McpFolderState::Normal);
        }
        
        track
    }
}

/// Global cache for track summaries per project (similar to CSI's tracks_ vector)
/// Key: project GUID string, Value: cached track summaries
static TRACK_SUMMARY_CACHE: std::sync::OnceLock<Mutex<HashMap<String, Vec<TrackSummary>>>> = std::sync::OnceLock::new();

fn get_cache() -> &'static Mutex<HashMap<String, Vec<TrackSummary>>> {
    TRACK_SUMMARY_CACHE.get_or_init(|| Mutex::new(HashMap::new()))
}

/// Get lightweight track summaries from a REAPER project
/// 
/// This is the performant approach - only reads essential track information
/// without parsing heavy data like items, envelopes, FX chains, etc.
/// Similar to how CSI handles tracks efficiently.
/// 
/// Caches summaries per project and only rebuilds when track structure changes.
pub fn get_track_summaries(project: &Project) -> Vec<TrackSummary> {
    // Use project file path as cache key (or fallback to index)
    // Similar to CSI which uses project/track pointers as identifiers
    let project_key = {
        if let Some(file_path) = project.file() {
            file_path.to_string()
        } else {
            // Fallback: use project index for unnamed projects
            format!("project_index_{}", project.index().unwrap_or(0))
        }
    };
    
    // Check cache first
    {
        let cache = get_cache().lock().unwrap();
        if let Some(cached) = cache.get(&project_key) {
            // Verify cache is still valid by checking track count
            let current_track_count = project.track_count() as usize;
            if cached.len() == current_track_count {
                debug!(
                    project_key = %project_key,
                    track_count = cached.len(),
                    "Using cached track summaries"
                );
                return cached.clone();
            } else {
                debug!(
                    project_key = %project_key,
                    cached_count = cached.len(),
                    current_count = current_track_count,
                    "Cache invalidated - track count changed"
                );
            }
        }
    }
    
    // Cache miss or invalid - rebuild summaries
    let mut summaries = Vec::new();
    
    for (index, reaper_track) in project.tracks().enumerate() {
        match get_track_summary_from_reaper_track(&reaper_track, index) {
            Ok(summary) => summaries.push(summary),
            Err(e) => {
                warn!(
                    error = %e,
                    track_index = index,
                    "Failed to get track summary"
                );
            }
        }
    }
    
    // Update cache
    {
        let mut cache = get_cache().lock().unwrap();
        cache.insert(project_key.clone(), summaries.clone());
        debug!(
            project_key = %project_key,
            track_count = summaries.len(),
            "Cached track summaries"
        );
    }
    
    summaries
}

/// Invalidate track summary cache for a project
/// 
/// Call this when track structure changes (tracks added/removed/reordered)
/// Similar to CSI's RebuildTracks() which clears and rebuilds tracks_ vector
pub fn invalidate_track_cache(project: &Project) {
    let project_key = {
        if let Some(file_path) = project.file() {
            file_path.to_string()
        } else {
            // Fallback: use project index for unnamed projects
            format!("project_index_{}", project.index().unwrap_or(0))
        }
    };
    let mut cache = get_cache().lock().unwrap();
    if cache.remove(&project_key).is_some() {
        debug!(
            project_key = %project_key,
            "Invalidated track summary cache"
        );
    }
}

/// Invalidate all track summary caches
/// 
/// Call this when projects are closed or switched
pub fn invalidate_all_track_caches() {
    let mut cache = get_cache().lock().unwrap();
    let count = cache.len();
    cache.clear();
    debug!(
        cache_count = count,
        "Invalidated all track summary caches"
    );
}

/// Get a lightweight track summary directly from REAPER track (without full chunk parsing)
fn get_track_summary_from_reaper_track(
    reaper_track: &ReaperTrack,
    index: usize,
) -> Result<TrackSummary, String> {
    // Get basic properties using high-level API (fast, no chunk parsing needed)
    let name = reaper_track.name()
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!("Track {}", index));
    
    let guid = Some(reaper_track.guid().to_string_without_braces());
    
    let volume = reaper_track.volume();
    let pan_value = reaper_track.pan();
    let pan_f64 = pan_value.reaper_value().get();
    let muted = reaper_track.is_muted();
    let solo_mode = reaper_track.solo_mode();
    let selected = reaper_track.is_selected();
    let custom_color = reaper_track.custom_color();
    let color = custom_color.map(|rgb| {
        // Convert RgbColor to u32 packed RGB
        ((rgb.r as u32) << 16) | ((rgb.g as u32) << 8) | (rgb.b as u32)
    });
    
    let folder_depth = reaper_track.folder_depth_change();
    let is_folder = folder_depth > 0;
    let track_depth = folder_depth.max(0) as i32; // Ensure non-negative
    
    // Get FX enabled state (requires chunk, but we can do a minimal read)
    let has_fx = reaper_track.fx_is_enabled();
    
    // Get record armed state
    let record_armed = reaper_track.is_armed(false);
    
    // Convert SoloMode to our type using to_raw() to handle Unknown variants
    use daw::tracks::api::solo::SoloMode as DawSoloMode;
    let solo_state = DawSoloMode::from_raw(solo_mode.to_raw());
    
    Ok(TrackSummary {
        name,
        index,
        guid,
        volume: volume.get(),
        pan: pan_f64,
        muted,
        solo_state,
        color,
        selected,
        is_folder,
        track_depth,
        has_fx,
        record_armed,
    })
}

/// Get all tracks from a REAPER project using high-level API + chunk parsing
/// 
/// Uses the safe high-level API for track iteration and chunk access.
/// NOTE: This includes ALL track data (items, envelopes, FX chains, etc.) which can be heavy.
/// For API efficiency, prefer `get_track_summaries()` instead.
pub fn get_all_tracks(project: &Project) -> Vec<Track> {
    let mut tracks = Vec::new();
    
    for (index, reaper_track) in project.tracks().enumerate() {
        match get_track_from_chunk(&reaper_track, index) {
            Ok(track) => tracks.push(track),
            Err(e) => {
                warn!(
                    error = %e,
                    track_index = index,
                    "Failed to get track from chunk"
                );
            }
        }
    }
    
    tracks
}

/// Get a specific track by index using high-level API + chunk parsing
/// 
/// Returns `None` if the track doesn't exist or parsing fails.
pub fn get_track(project: &Project, index: usize) -> Option<Track> {
    let reaper_track = project.track_by_index(index as u32)?;
    
    get_track_from_chunk(&reaper_track, index)
        .map_err(|e| {
            warn!(
                error = %e,
                track_index = index,
                "Failed to get track from chunk"
            );
            e
        })
        .ok()
}

/// Get tracks by indices using high-level API + chunk parsing
/// 
/// Returns a vector of tracks for the specified indices.
/// Tracks that fail to parse are skipped.
pub fn get_tracks(project: &Project, indices: &[usize]) -> Vec<Track> {
    let mut tracks = Vec::new();
    
    for &index in indices {
        if let Some(reaper_track) = project.track_by_index(index as u32) {
            if let Ok(track) = get_track_from_chunk(&reaper_track, index) {
                tracks.push(track);
            }
        }
    }
    
    tracks
}

/// Internal helper to get a track from its chunk
/// 
/// Uses the high-level Track::chunk() API for safe property access.
fn get_track_from_chunk(
    reaper_track: &ReaperTrack,
    index: usize,
) -> Result<Track, String> {
    // Read track state chunk using high-level safe API
    // Track::chunk() is safe and internally handles the unsafe calls
    let chunk = reaper_track.chunk(65536, ChunkCacheHint::NormalMode)
        .map_err(|e| format!("Failed to get track chunk: {}", e))?;
    
    // Convert chunk to string for parsing
    let chunk_str: String = chunk.try_into()
        .map_err(|_| "Failed to convert chunk to string".to_string())?;
    
    // Parse chunk and convert to Track
    let parsed = parse_track_chunk(&chunk_str, index)?;
    
    // Get selected state using high-level API (safe)
    let selected = reaper_track.is_selected();
    
    // Convert parsed chunk to Track struct
    Ok(parsed.to_track(Some(index), selected))
}
