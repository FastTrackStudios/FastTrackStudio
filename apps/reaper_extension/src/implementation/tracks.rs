//! REAPER Track Reading Utilities
//!
//! Utility functions for reading tracks from REAPER projects using the
//! high-level API with chunk parsing for efficient property access.
//!
//! - Uses `Project::tracks()` for track iteration (high-level, safe API)
//! - Uses `Track::chunk()` for efficient property access (safe wrapper)

use reaper_high::{Project, Track as ReaperTrack};
use reaper_medium::ChunkCacheHint;
use daw::tracks::{Track, parse_track_chunk};
use tracing::warn;

/// Get all tracks from a REAPER project using high-level API + chunk parsing
/// 
/// Uses the safe high-level API for track iteration and chunk access.
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
