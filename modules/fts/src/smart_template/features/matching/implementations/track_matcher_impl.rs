//! Implementation of track matching with playlist support
//!
//! This module provides functionality to:
//! 1. Parse string track names using naming convention parsers
//! 2. Match them to existing tracks
//! 3. Handle playlists (fixed item lanes in REAPER) for playlist variations

use crate::smart_template::core::traits::Parser;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::features::matching::matcher::{TrackMatcher, TrackMatch};
use crate::smart_template::features::naming::track_name::TrackNameLike;
use daw::tracks::Track;
use daw::tracks::api::free_mode::FreeMode;
use daw::tracks::api::fixed_lanes::FixedLanesSettings;
use std::collections::HashMap;

/// Track matching service that combines parsing and matching
pub struct TrackMatchingService {
    matcher: TrackMatcher,
}

impl TrackMatchingService {
    /// Create a new track matching service from existing tracks
    pub fn new(tracks: Vec<Track>) -> Self {
        Self {
            matcher: TrackMatcher::new(tracks),
        }
    }
    
    /// Parse and match a track name string
    ///
    /// Uses the provided parser to parse the track name, then matches it
    /// to existing tracks. Returns the match result and whether to use playlists.
    pub fn parse_and_match<P>(
        &self,
        track_name: &str,
        parser: &P,
        template: Option<&Template>,
    ) -> Option<(TrackMatch, P::Output)>
    where
        P: Parser,
        P::Output: TrackNameLike,
    {
        // Parse the track name
        let parsed = parser.parse(track_name).ok()?;
        
        // Match to existing tracks
        let match_result = self.matcher.match_track(&parsed, template)?;
        
        Some((match_result, parsed))
    }
    
    /// Enable playlists (fixed item lanes) on a track
    ///
    /// This sets up the track to use fixed item lanes mode in REAPER, which allows
    /// multiple items (with different playlists) to exist on the same track.
    pub fn enable_playlists(track: &mut Track) {
        // Set free mode to Fixed Item Lanes
        track.free_mode = Some(FreeMode::FixedItemLanes);
        
        // Initialize fixed lanes settings if not already set
        if track.fixed_lanes.is_none() {
            track.fixed_lanes = Some(FixedLanesSettings {
                bitfield: 0,
                allow_editing: true,
                show_play_only_lane: false,
                mask_playback: false,
                recording_behavior: 0,
            });
        }
    }
    
    /// Group tracks by normalized name for playlists
    ///
    /// Returns a map of normalized track names to lists of tracks that should
    /// be grouped together using playlists (fixed item lanes in REAPER).
    pub fn group_tracks_for_playlists(&self, tracks: &[Track]) -> HashMap<String, Vec<Track>> {
        let mut groups = HashMap::new();
        
        for track in tracks {
            let normalized = TrackMatcher::normalize_track_name(&track.name.0);
            groups
                .entry(normalized)
                .or_insert_with(Vec::new)
                .push(track.clone());
        }
        
        // Filter to only groups with multiple tracks (playlist variations)
        groups.retain(|_, tracks| tracks.len() > 1);
        
        groups
    }
    
    /// Process a collection of tracks and enable playlists where needed
    ///
    /// This identifies tracks that should use playlists (same name, different playlists)
    /// and enables fixed item lanes mode on those tracks.
    pub fn process_tracks_for_playlists(tracks: &mut [Track]) {
        let groups = Self::group_tracks_static(tracks);
        
        for (_normalized_name, track_indices) in groups {
            if track_indices.len() > 1 {
                // Multiple tracks with same normalized name - enable playlists on first one
                if let Some(&first_idx) = track_indices.first() {
                    Self::enable_playlists(&mut tracks[first_idx]);
                }
            }
        }
    }
    
    /// Static version of group_tracks_for_playlists for use without self
    fn group_tracks_static(tracks: &[Track]) -> HashMap<String, Vec<usize>> {
        let mut groups = HashMap::new();
        
        for (idx, track) in tracks.iter().enumerate() {
            let normalized = TrackMatcher::normalize_track_name(&track.name.0);
            groups
                .entry(normalized)
                .or_insert_with(Vec::new)
                .push(idx);
        }
        
        // Filter to only groups with multiple tracks
        groups.retain(|_, indices| indices.len() > 1);
        
        groups
    }
}
