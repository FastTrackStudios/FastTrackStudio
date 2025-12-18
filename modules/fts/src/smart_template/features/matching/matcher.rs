//! Matcher implementation
//!
//! Handles matching parsed track names to existing tracks, including support for
//! playlists (fixed item lanes in REAPER) when tracks match everything except playlist.

use crate::smart_template::features::naming::track_name::TrackNameLike;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::core::models::types::TrackName;
use daw::tracks::Track;
use std::collections::HashMap;

/// Match result from finding a track
#[derive(Debug, Clone)]
pub struct MatchResult {
    /// THE matched track name
    pub track_name: TrackName,
    
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

/// Strategy for scoring matches between parsed track names and template tracks
pub trait MatchStrategy: Send + Sync {
    /// Score a candidate track from a template against a parsed track name
    fn score(&self, template_track: &Track, parsed_name: &dyn TrackNameLike) -> u8;
    
    /// Get the name of this strategy
    fn name(&self) -> &str;
}

/// A matching strategy that only looks for exact name matches
pub struct ExactMatchStrategy;

impl MatchStrategy for ExactMatchStrategy {
    fn score(&self, template_track: &Track, parsed_name: &dyn TrackNameLike) -> u8 {
        if let Some(original) = parsed_name.original_name() {
            if template_track.name.eq_ignore_case(original) {
                return 100;
            }
        }
        0
    }
    
    fn name(&self) -> &str {
        "exact"
    }
}

/// A matching strategy that uses the naming convention components (sub-types, multi-mic, etc.)
pub struct SmartMatchStrategy;

impl MatchStrategy for SmartMatchStrategy {
    fn score(&self, template_track: &Track, parsed_name: &dyn TrackNameLike) -> u8 {
        // Try exact match first
        if let Some(original) = parsed_name.original_name() {
            if template_track.name.eq_ignore_case(original) {
                return 100;
            }
        }
        
        // Try sub-type match
        if let Some(sub_types) = parsed_name.sub_type() {
            for sub_type in sub_types {
                if template_track.name.contains_ignore_case(sub_type) {
                    return 80;
                }
            }
        }
        
        0
    }
    
    fn name(&self) -> &str {
        "smart"
    }
}

/// Result of matching a track name to an existing track
#[derive(Debug, Clone)]
pub struct TrackMatch {
    /// The matched track (if found)
    pub track: Track,
    
    /// Whether this match should use playlists (fixed item lanes in REAPER)
    pub use_playlist: bool,
    
    /// The playlist identifier if this is a playlist variation (e.g., ".1", ".2")
    pub playlist: Option<String>,
    
    /// Match confidence score (0-100)
    pub score: u8,
}

/// Helper for standard matching logic used by many instrument matchers
pub mod helpers {
    use super::*;
    use crate::smart_template::features::naming::item_properties::ItemProperties;

    /// Standard implementation of find_best_match for instrument matchers
    pub fn instrument_find_best_match(
        template: &Template,
        track_name: &ItemProperties,
        search_name: &str,
    ) -> Option<MatchResult> {
        // Look for exact match
        if let Some(track) = template.tracks.iter()
            .find(|t| t.name.eq_ignore_case(search_name)) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: MatchType::Exact,
                score: 100,
                use_takes: false,
            });
        }
        
        // Try partial match
        if let Some(track) = template.tracks.iter()
            .find(|t| t.name.contains_ignore_case(search_name)) {
            return Some(MatchResult {
                track_name: track.name.clone(),
                match_type: MatchType::Partial,
                score: 60,
                use_takes: false,
            });
        }
        
        // Check for playlist variation
        if track_name.playlist.is_some() {
            if let Some(original) = &track_name.original_name {
                let name_without_playlist = original
                    .trim_end_matches(|c: char| c == '.' || c.is_alphanumeric());
                
                if let Some(track) = template.tracks.iter()
                    .find(|t| t.name.eq_ignore_case(name_without_playlist)) {
                    return Some(MatchResult {
                        track_name: track.name.clone(),
                        match_type: MatchType::PlaylistVariation,
                        score: 90,
                        use_takes: true,
                    });
                }
            }
        }
        
        None
    }
}

/// Track matcher that matches parsed track names to existing tracks
pub struct TrackMatcher {
    /// Existing tracks indexed by a normalized key (without playlist)
    tracks_by_key: HashMap<String, Vec<Track>>,
    
    /// Tracks indexed by name for quick lookup
    tracks_by_name: HashMap<String, Track>,

    /// The matching strategy to use
    strategy: Box<dyn MatchStrategy>,
}

impl TrackMatcher {
    /// Create a new track matcher from a collection of existing tracks
    pub fn new(tracks: Vec<Track>) -> Self {
        Self::with_strategy(tracks, Box::new(SmartMatchStrategy))
    }

    /// Create a new track matcher with a specific strategy
    pub fn with_strategy(tracks: Vec<Track>, strategy: Box<dyn MatchStrategy>) -> Self {
        let mut tracks_by_key = HashMap::new();
        let mut tracks_by_name = HashMap::new();
        
        for track in tracks {
            // Index by full name
            tracks_by_name.insert(track.name.0.clone(), track.clone());
            
            // Index by normalized key (name without playlist)
            let normalized_key = Self::normalize_track_name(&track.name.0);
            tracks_by_key
                .entry(normalized_key)
                .or_insert_with(Vec::new)
                .push(track);
        }
        
        Self {
            tracks_by_key,
            tracks_by_name,
            strategy,
        }
    }
    
    /// Normalize a track name by removing playlist identifier
    pub fn normalize_track_name(name: &str) -> String {
        let mut normalized = name.to_string();
        if let Some(dot_pos) = normalized.rfind('.') {
            if dot_pos < normalized.len() - 1 {
                let after_dot = &normalized[dot_pos + 1..];
                if after_dot.chars().all(|c| c.is_ascii_digit()) 
                    || (after_dot.len() == 1 && after_dot.chars().all(|c| c.is_ascii_alphabetic())) {
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
                if after_dot.chars().all(|c| c.is_ascii_digit()) 
                    || (after_dot.len() == 1 && after_dot.chars().all(|c| c.is_ascii_alphabetic())) {
                    return Some(format!(".{}", after_dot));
                }
            }
        }
        None
    }

    /// Match a parsed track name to an existing track
    pub fn match_track<T: TrackNameLike>(
        &self,
        parsed_name: &T,
        template: Option<&Template>,
    ) -> Option<TrackMatch> {
        let track_name = if let Some(original) = parsed_name.original_name() {
            original.to_string()
        } else {
            return None;
        };
        
        let normalized = Self::normalize_track_name(&track_name);
        let has_playlist_variations = self.tracks_by_key
            .get(&normalized)
            .map(|tracks| tracks.len() > 1)
            .unwrap_or(false);
        
        if let Some(track) = self.tracks_by_name.get(&track_name) {
            return Some(TrackMatch {
                track: track.clone(),
                use_playlist: has_playlist_variations,
                playlist: Self::extract_playlist(&track_name),
                score: 100,
            });
        }
        
        if let Some(tracks) = self.tracks_by_key.get(&normalized) {
            if let Some(first_track) = tracks.first() {
                return Some(TrackMatch {
                    track: first_track.clone(),
                    use_playlist: true,
                    playlist: Self::extract_playlist(&track_name),
                    score: 90,
                });
            }
        }
        
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
        let mut best_match: Option<(Track, u8)> = None;

        for track in &template.tracks {
            let score = self.strategy.score(track, parsed_name);
            if score > 0 {
                if let Some((_, best_score)) = best_match {
                    if score > best_score {
                        best_match = Some((track.clone(), score));
                    }
                } else {
                    best_match = Some((track.clone(), score));
                }
            }
        }

        if let Some((track, score)) = best_match {
            return Some(TrackMatch {
                track,
                use_playlist: false,
                playlist: parsed_name.original_name().and_then(|n| Self::extract_playlist(n)),
                score,
            });
        }
        
        None
    }
}
