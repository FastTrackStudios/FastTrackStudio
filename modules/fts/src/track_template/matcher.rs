//! Track matching engine
//!
//! Matches parsed track names to tracks in a template using the naming convention.

use crate::naming_convention::{TrackName, SimpleParser, format_track_name_default};
#[cfg(feature = "default-groups")]
use crate::naming_convention::create_default_groups;
use crate::track_list::TrackList;
use crate::track::{Track, SortStatus};
use tracing::{debug, trace};

/// Matches track names to tracks in a template
pub struct TrackMatcher {
    /// Parser for converting strings to TrackName
    parser: SimpleParser,
    
    /// Current track list to match against
    track_list: TrackList,
}

impl TrackMatcher {
    /// Create a new matcher with default groups
    pub fn new() -> Self {
        #[cfg(feature = "default-groups")]
        let groups = create_default_groups();
        #[cfg(not(feature = "default-groups"))]
        let groups = Vec::new();
        let parser = SimpleParser::new(groups);
        
        Self {
            parser,
            track_list: TrackList::new(),
        }
    }
    
    /// Create a new matcher with a custom track list
    pub fn with_track_list(track_list: TrackList) -> Self {
        #[cfg(feature = "default-groups")]
        let groups = create_default_groups();
        #[cfg(not(feature = "default-groups"))]
        let groups = Vec::new();
        let parser = SimpleParser::new(groups);
        
        Self {
            parser,
            track_list,
        }
    }
    
    /// Get the current track list
    pub fn track_list(&self) -> &TrackList {
        &self.track_list
    }
    
    /// Get a mutable reference to the track list
    pub fn track_list_mut(&mut self) -> &mut TrackList {
        &mut self.track_list
    }
    
    /// Parse a track name string into a TrackName
    pub fn parse(&self, name: &str) -> TrackName {
        self.parser.parse(name)
    }
    
    /// Find the best matching track for a parsed TrackName
    pub fn find_best_match(&self, track_name: &TrackName) -> Option<MatchResult> {
        debug!(parsed_name = ?track_name, "Finding best match for parsed track name");
        
        // Strategy: Try multiple matching approaches in order of specificity
        // 1. Exact name match (case-insensitive)
        // 2. Match by group prefix + sub-type (checking for playlist variations)
        // 3. Match by group prefix only
        // 4. Partial name match
        
        // Try exact match first
        if track_name.group_prefix.is_some() {
            // Try matching with full prefix path
            let full_name = format_track_name_for_matching(track_name);
            if let Some(track) = self.track_list.find_track_exact(&full_name) {
                debug!(matched_track = %track.name, "Found exact match");
                return Some(MatchResult {
                    track: track.clone(),
                    match_type: MatchType::Exact,
                    score: 100,
                    use_takes: false,
                });
            }
            
            // Try matching with just the sub-type (last component)
            if let Some(sub_types) = &track_name.sub_type {
                if let Some(last_subtype) = sub_types.last() {
                    if let Some(track) = self.track_list.find_track_exact(last_subtype) {
                        debug!(matched_track = %track.name, "Found match by sub-type");
                        return Some(MatchResult {
                            track: track.clone(),
                            match_type: MatchType::SubType,
                            score: 80,
                            use_takes: false,
                        });
                    }
                }
            }
        }
        
        // Check for playlist variations - items that are the same except for playlist
        // This should match to the same track but use takes (item lanes)
        // We check this even if the current track_name doesn't have a playlist,
        // because an existing track might have a playlist and we want to match to it
        for track in self.track_list.tracks() {
            // Parse the existing track name to compare
            let existing_parsed = self.parse(&track.name);
            
            // Check if they're the same except for playlist
            if Self::are_same_except_playlist(track_name, &existing_parsed) {
                debug!(
                    matched_track = %track.name,
                    current_playlist = ?track_name.playlist,
                    existing_playlist = ?existing_parsed.playlist,
                    "Found playlist variation - will use takes"
                );
                return Some(MatchResult {
                    track: track.clone(),
                    match_type: MatchType::PlaylistVariation,
                    score: 90,
                    use_takes: true,
                });
            }
        }
        
        // Try partial match
        if let Some(sub_types) = &track_name.sub_type {
            if let Some(last_subtype) = sub_types.last() {
                let matches = self.track_list.find_track_by_partial(last_subtype);
                if let Some(track) = matches.first() {
                    debug!(matched_track = %track.name, "Found partial match");
                    return Some(MatchResult {
                        track: (*track).clone(),
                        match_type: MatchType::Partial,
                        score: 60,
                        use_takes: false,
                    });
                }
            }
        }
        
        trace!("No match found");
        None
    }
    
    /// Check if two TrackName objects are the same except for playlist
    fn are_same_except_playlist(name1: &TrackName, name2: &TrackName) -> bool {
        // Compare all fields except playlist
        name1.rec_tag == name2.rec_tag
            && name1.group_prefix == name2.group_prefix
            && name1.sub_type == name2.sub_type
            && name1.performer == name2.performer
            && name1.arrangement == name2.arrangement
            && name1.section == name2.section
            && name1.layers == name2.layers
            && name1.multi_mic == name2.multi_mic
            && name1.effect == name2.effect
            && name1.increment == name2.increment
            && name1.channel == name2.channel
            && name1.track_type == name2.track_type
            // Playlist can be different
            && name1.playlist != name2.playlist // Must be different
            && (name1.playlist.is_some() || name2.playlist.is_some()) // At least one must have playlist
    }
    
    /// Find or create a track for a parsed TrackName
    /// Returns the track and whether to use takes (item lanes)
    /// 
    /// `use_takes` will be `true` when the track matches an existing track
    /// that differs only in playlist number, indicating items should be
    /// placed as takes (item lanes) on the same track.
    pub fn find_or_create_track(&mut self, track_name: &TrackName, base_name: Option<&str>) -> (Track, bool) {
        // First try to find an existing match
        if let Some(result) = self.find_best_match(track_name) {
            debug!(
                track = %result.track.name,
                use_takes = result.use_takes,
                match_type = ?result.match_type,
                score = result.score,
                "Using existing track"
            );
            
            // Update sort status based on match confidence
            // Exact matches are sorted, partial matches might need confirmation
            let sort_status = match result.match_type {
                MatchType::Exact => SortStatus::Sorted,
                MatchType::SubType => SortStatus::Sorted, // Sub-type matches are usually good
                MatchType::PlaylistVariation => SortStatus::Sorted, // Playlist variations are sorted
                MatchType::Partial => {
                    // Partial matches might need confirmation if score is low
                    if result.score < 70 {
                        SortStatus::NeedsConfirmation
                    } else {
                        SortStatus::Sorted
                    }
                }
            };
            
            // Update the track's sort status if needed
            if let Some(track) = self.track_list.get_track_mut(&result.track.name) {
                track.set_sort_status(sort_status);
            }
            
            // Return the updated track
            let updated_track = self.track_list.get_track(&result.track.name)
                .cloned()
                .unwrap_or(result.track);
            
            return (updated_track, result.use_takes);
        }
        
        // No match found, create a new track
        let new_track_name = base_name
            .map(|s| s.to_string())
            .or_else(|| {
                // Generate name from parsed components
                if let Some(sub_types) = &track_name.sub_type {
                    sub_types.last().cloned()
                } else {
                    track_name.group_prefix.clone()
                }
            })
            .unwrap_or_else(|| "Unknown".to_string());
        
        debug!(new_track = %new_track_name, "Creating new track");
        
        let mut new_track = Track::new(&new_track_name);
        
        // Set track type if present
        if let Some(track_type) = &track_name.track_type {
            new_track.track_type = Some(track_type.clone());
        }
        
        // Determine sort status based on what we could parse
        // If we have a group prefix, we might need confirmation
        // If we have nothing, it's not sorted
        let sort_status = if track_name.group_prefix.is_some() {
            // We have some information but couldn't match - might need confirmation
            SortStatus::NeedsConfirmation
        } else {
            // No group prefix or other identifying info - can't be sorted
            SortStatus::NotSorted
        };
        new_track.set_sort_status(sort_status);
        
        // Set parent if we have group prefix (parent track)
        // The parent would be the group's parent track
        // For now, we'll set it based on the prefix
        // This could be enhanced to look up the actual parent track name
        if track_name.group_prefix.is_some() {
            // TODO: Look up parent track from group configuration
        }
        
        // Add the new track to the list
        self.track_list.add_track(new_track.clone());
        
        (new_track, false) // New tracks don't use takes
    }
}

/// Result of a track match
#[derive(Debug, Clone)]
pub struct MatchResult {
    /// The matched track
    pub track: Track,
    
    /// The type of match
    pub match_type: MatchType,
    
    /// Match score (0-100, higher is better)
    pub score: u8,
    
    /// Whether to use takes (item lanes) for this match
    /// This is true when items are the same except for playlist numbers
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
}

/// Format a TrackName for matching purposes
fn format_track_name_for_matching(track_name: &TrackName) -> String {
    format_track_name_default(track_name)
}

impl Default for TrackMatcher {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_are_same_except_playlist() {
        let mut name1 = TrackName::new();
        name1.group_prefix = Some("D".to_string());
        name1.sub_type = Some(vec!["Kick".to_string()]);
        name1.playlist = Some(".1".to_string());
        
        let mut name2 = TrackName::new();
        name2.group_prefix = Some("D".to_string());
        name2.sub_type = Some(vec!["Kick".to_string()]);
        name2.playlist = Some(".2".to_string());
        
        assert!(TrackMatcher::are_same_except_playlist(&name1, &name2));
        
        // Different sub-type should not match
        let mut name3 = TrackName::new();
        name3.group_prefix = Some("D".to_string());
        name3.sub_type = Some(vec!["Snare".to_string()]);
        name3.playlist = Some(".1".to_string());
        
        assert!(!TrackMatcher::are_same_except_playlist(&name1, &name3));
    }
    
    #[test]
    fn test_playlist_variation_matching() {
        let mut matcher = TrackMatcher::new();
        
        // Add a track with playlist .1
        let track1 = Track::new("D Kick In .1");
        matcher.track_list_mut().add_track(track1);
        
        // Parse a track name with playlist .2
        let parsed = matcher.parse("D Kick In .2");
        
        // Should match and indicate use_takes
        if let Some(result) = matcher.find_best_match(&parsed) {
            assert_eq!(result.match_type, MatchType::PlaylistVariation);
            assert!(result.use_takes);
        } else {
            panic!("Should have found a playlist variation match");
        }
    }
}
