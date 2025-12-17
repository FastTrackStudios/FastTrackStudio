//! Matcher trait
//!
//! Defines the interface for matching parsed track names to template tracks.

use super::parser::TrackNameLike;

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
