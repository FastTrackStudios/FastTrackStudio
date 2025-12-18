//! Matcher trait and TrackMatcher implementation
//!
//! Handles matching parsed track names to existing tracks, including support for
//! playlists (fixed item lanes in REAPER) when tracks match everything except playlist.

use crate::smart_template::naming::track_name::TrackNameLike;
use crate::smart_template::core::template::Template;
use daw::tracks::Track;
use std::collections::HashMap;

/// Match result from finding a track
#[derive(Debug, Clone)]
pub struct MatchResult {
    /// The matched track name
    pub track_name: String,
    
    /// Match type/confidence
    pub match_type: MatchType,
    
    /// Match score (0-100, higher is better)
    pub score: u8,
    
    /// Whether to use takes (item lanes) for this match
    pub use_takes: bool,
}

/// Type of match found
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MatchType {
    /// Exact name match
    Exact,
    /// Match by sub-type
    SubType,
    /// Match with different playlist (should use takes)
    PlaylistVariation,
    /// Partial name match
    Partial,
    /// No match found
    None,
}

/// Trait for matching parsed track names to template tracks
///
/// Implementations of this trait find the best matching track in a template
/// for a given parsed track name, or create a new track if no match is found.
pub trait Matcher: Send + Sync {
    /// The track name type this matcher works with
    type TrackName: TrackNameLike;
    
    /// Error type for matching operations
    type Error: std::error::Error + Send + Sync;
    
    /// Find the best matching track for a parsed track name
    fn find_best_match(&self, track_name: &Self::TrackName) -> Option<MatchResult>;
    
    /// Find or create a track for a parsed track name
    ///
    /// Returns the track name and whether to use takes (item lanes)
    fn find_or_create_track(&mut self, track_name: &Self::TrackName, base_name: Option<&str>) -> Result<(String, bool), Self::Error>;
    
    /// Get the name/identifier of this matcher
    fn name(&self) -> &str;
}

impl TrackMatcher {
    /// Normalize a track name by removing playlist identifier
    /// This allows matching "Kick In .1" and "Kick In .2" as the same track
    pub fn normalize_track_name(name: &str) -> String {
        // Remove playlist pattern (e.g., ".1", ".2", ".A")
        // Playlist is typically at the end: "Track Name .1"
        let mut normalized = name.to_string();
        
        // Try to match playlist pattern: . followed by digits or letters
        if let Some(dot_pos) = normalized.rfind('.') {
            if dot_pos < normalized.len() - 1 {
                let after_dot = &normalized[dot_pos + 1..];
                // Check if it's a playlist (digits or single letter)
                if after_dot.chars().all(|c| c.is_ascii_digit()) 
                    || (after_dot.len() == 1 && after_dot.chars().all(|c| c.is_ascii_alphabetic())) {
                    // Remove the playlist part
                    normalized = normalized[..dot_pos].trim_end().to_string();
                }
            }
        }
        
        normalized
    }
    
    /// Extract playlist from a track name
    pub fn extract_playlist(name: &str) -> Option<String> {
        if let Some(dot_pos) = name.rfind('.') {
            if dot_pos < name.len() - 1 {
                let after_dot = &name[dot_pos + 1..];
                // Check if it's a playlist (digits or single letter)
                if after_dot.chars().all(|c| c.is_ascii_digit()) 
                    || (after_dot.len() == 1 && after_dot.chars().all(|c| c.is_ascii_alphabetic())) {
                    return Some(format!(".{}", after_dot));
                }
            }
        }
        None
    }
}

/// Result of matching a track name to an existing track
#[derive(Debug, Clone)]
pub struct TrackMatch {
    /// The matched track (if found)
    pub track: Track,
    
    /// Whether this match should use playlists (fixed item lanes in REAPER)
    /// True when the track matches everything except playlist
    pub use_playlist: bool,
    
    /// The playlist identifier if this is a playlist variation (e.g., ".1", ".2")
    pub playlist: Option<String>,
    
    /// Match confidence score (0-100)
    pub score: u8,
}

/// Track matcher that matches parsed track names to existing tracks
pub struct TrackMatcher {
    /// Existing tracks indexed by a normalized key (without playlist)
    tracks_by_key: HashMap<String, Vec<Track>>,
    
    /// Tracks indexed by name for quick lookup
    tracks_by_name: HashMap<String, Track>,
}

impl TrackMatcher {
    /// Create a new track matcher from a collection of existing tracks
    pub fn new(tracks: Vec<Track>) -> Self {
        let mut tracks_by_key = HashMap::new();
        let mut tracks_by_name = HashMap::new();
        
        for track in tracks {
            // Index by full name
            tracks_by_name.insert(track.name.clone(), track.clone());
            
            // Index by normalized key (name without playlist)
            let normalized_key = Self::normalize_track_name(&track.name);
            tracks_by_key
                .entry(normalized_key)
                .or_insert_with(Vec::new)
                .push(track);
        }
        
        Self {
            tracks_by_key,
            tracks_by_name,
        }
    }
    
    /// Match a parsed track name to an existing track
    ///
    /// Returns a TrackMatch if a match is found, considering:
    /// - Exact name match
    /// - Match without playlist (for fixed item lanes)
    /// - Template-based matching
    pub fn match_track<T: TrackNameLike>(
        &self,
        parsed_name: &T,
        template: Option<&Template>,
    ) -> Option<TrackMatch> {
        // Get the formatted track name
        let track_name = if let Some(original) = parsed_name.original_name() {
            original.to_string()
        } else {
            return None;
        };
        
        // Normalize the track name to check for playlist variations
        let normalized = Self::normalize_track_name(&track_name);
        let has_playlist_variations = self.tracks_by_key
            .get(&normalized)
            .map(|tracks| tracks.len() > 1)
            .unwrap_or(false);
        
        // Try exact name match first
        if let Some(track) = self.tracks_by_name.get(&track_name) {
            return Some(TrackMatch {
                track: track.clone(),
                use_playlist: has_playlist_variations, // Use playlists if there are variations
                playlist: Self::extract_playlist(&track_name),
                score: 100,
            });
        }
        
        // Try normalized match (without playlist) - this indicates playlist variation
        if let Some(tracks) = self.tracks_by_key.get(&normalized) {
            // Found tracks with the same normalized name
            // Use the first one, but indicate we should use playlists (fixed item lanes)
            if let Some(first_track) = tracks.first() {
                return Some(TrackMatch {
                    track: first_track.clone(),
                    use_playlist: true, // Always use playlists for normalized matches
                    playlist: Self::extract_playlist(&track_name),
                    score: 90, // High score but slightly lower than exact match
                });
            }
        }
        
        // Try template-based matching if template is provided
        if let Some(template) = template {
            if let Some(template_match) = self.match_to_template(parsed_name, template) {
                return Some(template_match);
            }
        }
        
        None
    }
    
    /// Match a parsed track name to a template track
    fn match_to_template<T: TrackNameLike>(
        &self,
        parsed_name: &T,
        template: &Template,
    ) -> Option<TrackMatch> {
        // Build expected track name from parsed components
        let expected_name = self.build_track_name_from_parsed(parsed_name, template);
        
        if let Some(expected) = expected_name {
            // Try exact match
            if let Some(track) = self.tracks_by_name.get(&expected) {
                return Some(TrackMatch {
                    track: track.clone(),
                    use_playlist: false,
                    playlist: parsed_name.original_name().and_then(|n| Self::extract_playlist(n)),
                    score: 85,
                });
            }
            
            // Try normalized match
            let normalized = Self::normalize_track_name(&expected);
            if let Some(tracks) = self.tracks_by_key.get(&normalized) {
                if let Some(first_track) = tracks.first() {
                    return Some(TrackMatch {
                        track: first_track.clone(),
                        use_playlist: true,
                        playlist: parsed_name.original_name().and_then(|n| Self::extract_playlist(n)),
                        score: 75,
                    });
                }
            }
        }
        
        None
    }
    
    /// Build a track name from parsed components using template structure
    fn build_track_name_from_parsed<T: TrackNameLike>(
        &self,
        parsed_name: &T,
        template: &Template,
    ) -> Option<String> {
        // For now, use the original name
        // This can be enhanced to build names from components
        parsed_name.original_name().map(|s| s.to_string())
    }
    
    /// Get all tracks that match a normalized name (for playlists)
    ///
    /// Returns all tracks that have the same normalized name, which should
    /// be grouped together using playlists (fixed item lanes in REAPER).
    pub fn get_tracks_for_playlist(&self, track_name: &str) -> Vec<Track> {
        let normalized = Self::normalize_track_name(track_name);
        self.tracks_by_key
            .get(&normalized)
            .cloned()
            .unwrap_or_default()
    }
    
    /// Check if a track should use playlists
    ///
    /// Returns true if there are multiple tracks with the same normalized name
    /// (i.e., same name but different playlists)
    pub fn should_use_playlist(&self, track_name: &str) -> bool {
        let normalized = Self::normalize_track_name(track_name);
        self.tracks_by_key
            .get(&normalized)
            .map(|tracks| tracks.len() > 1)
            .unwrap_or(false)
    }
}
