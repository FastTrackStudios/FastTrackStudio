//! Helper functions for parsing track names into templates
//!
//! Provides easy-to-use functions that combine parsing, template creation,
//! and fold pattern transformations.

use crate::smart_template::presets::drums::{
    Kick, Snare, Tom, Cymbals, Room,
};
use crate::smart_template::core::traits::Parser;
use crate::smart_template::core::models::template::Template;
use crate::smart_template::utils::track_helpers::{create_track, TrackExt};
use crate::smart_template::features::transform::fold::*;
use daw::tracks::{Track, TrackName};

/// Parse track names into a template structure
///
/// This function:
/// 1. Parses track names using all available parsers
/// 2. Creates a Template with parsed tracks
/// 3. Applies default fold transformations (auto-increment, playlist matching)
///
/// # Arguments
/// * `track_names` - Slice of track name strings
///
/// # Returns
/// A `Template` containing all successfully parsed tracks
pub fn parse_fts_structure(track_names: &[&str]) -> Template {
    parse_fts_structure_with_folders(track_names, DefaultFolders::new())
}

/// Parse track names into a template with custom folders
///
/// Allows you to specify which fold transformations to apply.
pub fn parse_fts_structure_with_folders(track_names: &[&str], folders: DefaultFolders) -> Template {
    let kick = Kick::new();
    let snare = Snare::new();
    let tom = Tom::new();
    let cymbals = Cymbals::new();
    let room = Room::new();
    
    // Parse all track names into a Vec<Track> with groups
    let mut parsed_tracks = Vec::new();
    
    for track_name in track_names {
        let group = if kick.parse(track_name).is_ok() {
            Some("kick")
        } else if snare.parse(track_name).is_ok() {
            Some("snare")
        } else if tom.parse(track_name).is_ok() {
            Some("tom")
        } else if cymbals.parse(track_name).is_ok() {
            Some("cymbals")
        } else if room.parse(track_name).is_ok() {
            Some("room")
        } else {
            None
        };
        
        if let Some(group_name) = group {
            let mut track = create_track(track_name.to_string(), None, None, &[]);
            track.set_group(group_name);
            parsed_tracks.push(track);
        }
    }
    
    // Build folder hierarchy as Vec<Track> using folder properties
    // Structure: BUS -> SUM -> individual tracks
    // Group tracks by their group name to build hierarchy
    let mut all_tracks = Vec::new();
    let mut current_group: Option<String> = None;
    let mut current_group_tracks: Vec<Track> = Vec::new();
    
    for track in parsed_tracks {
        let track_group = track.get_group().unwrap_or_default();
        
        // If we hit a new group, build hierarchy for previous group
        if current_group.is_some() && current_group.as_ref() != Some(&track_group) {
            if !current_group_tracks.is_empty() {
                let group_name = current_group.as_ref().unwrap();
                let bus_name = format!("{}", group_name.chars().next().unwrap().to_uppercase().collect::<String>() + &group_name[1..]);
                let sum_name = format!("{} (SUM)", bus_name);
                
                // Build hierarchy: BUS -> SUM -> individual tracks
                let group_tracks = std::mem::take(&mut current_group_tracks);
                
                let mut bus_track = Track::new(bus_name);
                bus_track.is_folder = true;
                bus_track.folder_depth_change = daw::tracks::api::folder::FolderDepthChange::FolderStart;
                
                let mut sum_track = Track::new(sum_name);
                sum_track.is_folder = true;
                sum_track.folder_depth_change = daw::tracks::api::folder::FolderDepthChange::FolderStart;
                
                let mut tracks = vec![bus_track, sum_track];
                let group_tracks_len = group_tracks.len();
                for (i, mut t) in group_tracks.into_iter().enumerate() {
                    if i == group_tracks_len - 1 {
                        t.folder_depth_change = daw::tracks::api::folder::FolderDepthChange::ClosesLevels(-2);
                    } else {
                        t.folder_depth_change = daw::tracks::api::folder::FolderDepthChange::Normal;
                    }
                    tracks.push(t);
                }
                
                all_tracks.extend(tracks);
            }
        }
        
        current_group = Some(track_group.clone());
        current_group_tracks.push(track);
    }
    
    // Build hierarchy for the last group
    if !current_group_tracks.is_empty() {
        let group_name = current_group.as_ref().unwrap();
        let bus_name = format!("{}", group_name.chars().next().unwrap().to_uppercase().collect::<String>() + &group_name[1..]);
        let sum_name = format!("{} (SUM)", bus_name);
        
        let group_tracks: Vec<Track> = std::mem::take(&mut current_group_tracks);
        
        let mut bus_track = Track::new(bus_name);
        bus_track.is_folder = true;
        bus_track.folder_depth_change = daw::tracks::api::folder::FolderDepthChange::FolderStart;
        
        let mut sum_track = Track::new(sum_name);
        sum_track.is_folder = true;
        sum_track.folder_depth_change = daw::tracks::api::folder::FolderDepthChange::FolderStart;
        
        let mut tracks = vec![bus_track, sum_track];
        let group_tracks_len = group_tracks.len();
        for (i, mut t) in group_tracks.into_iter().enumerate() {
            if i == group_tracks_len - 1 {
                t.folder_depth_change = daw::tracks::api::folder::FolderDepthChange::ClosesLevels(-2);
            } else {
                t.folder_depth_change = daw::tracks::api::folder::FolderDepthChange::Normal;
            }
            tracks.push(t);
        }
        
        all_tracks.extend(tracks);
    }
    
    // Create initial template
    let template = Template {
        name: TrackName::from("Parsed Tracks"),
        tracks: all_tracks,
    };
    
    // Apply fold transformations
    let template = if folders.auto_increment {
        template.fold(AutoIncrementGroupFolder::new())
    } else {
        template
    };
    
    let template = if folders.playlist_matching {
        template.fold(PlaylistMatchingFolder::new())
    } else {
        template
    };
    
    template
}

/// Configuration for default fold transformations
pub struct DefaultFolders {
    /// Apply auto-increment grouping (Snare -> Snare 2 separate groups)
    pub auto_increment: bool,
    
    /// Apply playlist matching (Snare .1, Snare .2)
    pub playlist_matching: bool,
}

impl DefaultFolders {
    pub fn new() -> Self {
        Self {
            auto_increment: true,
            playlist_matching: true,
        }
    }
    
    pub fn with_auto_increment(mut self, enable: bool) -> Self {
        self.auto_increment = enable;
        self
    }
    
    pub fn with_playlist_matching(mut self, enable: bool) -> Self {
        self.playlist_matching = enable;
        self
    }
}

impl Default for DefaultFolders {
    fn default() -> Self {
        Self::new()
    }
}

/// Convenience macro for building expected tracks using the Track builder pattern
#[macro_export]
macro_rules! tracks {
    ($(($name:expr, $group:expr)),* $(,)?) => {
        {
            use $crate::smart_template::utils::track_helpers::{create_track, TrackExt};
            
            let mut tracks_vec = Vec::new();
            $(
                {
                    let mut track = create_track($name, None, None, &[]);
                    track.set_group($group);
                    tracks_vec.push(track);
                }
            )*
            tracks_vec
        }
    };
    
    ($(($name:expr, $group:expr, $parent:expr)),* $(,)?) => {
        {
            use $crate::smart_template::utils::track_helpers::{create_track, TrackExt};
            
            let mut tracks_vec = Vec::new();
            $(
                {
                    let mut track = create_track($name, None, $parent, &[]);
                    track.set_group($group);
                    tracks_vec.push(track);
                }
            )*
            tracks_vec
        }
    };
    
    ($(($name:expr, $group:expr, $parent:expr, $track_type:expr)),* $(,)?) => {
        {
            use $crate::smart_template::utils::track_helpers::{create_track, TrackExt};
            
            let mut tracks_vec = Vec::new();
            $(
                {
                    let mut track = create_track($name, $track_type, $parent, &[]);
                    track.set_group($group);
                    tracks_vec.push(track);
                }
            )*
            tracks_vec
        }
    };
}

/// Helper function for building hierarchical track structures
pub fn build_tracks_with_hierarchy(
    hierarchy: Vec<(
        &str,                    // name
        &str,                    // group
        Option<&str>,            // track_type
        Option<&str>,            // parent
        Vec<(&str, &str, Option<&str>, Option<&str>)>, // children
    )>,
) -> Vec<Track> {
    use crate::smart_template::utils::track_helpers::{TrackExt, create_track};
    
    let mut tracks = Vec::new();
    
    for (name, group, track_type, _parent, children) in hierarchy {
        let mut track = create_track(name, track_type, None, &[]);
        track.set_group(group);
        tracks.push(track);
        
        for (child_name, child_group, child_type, parent_override) in children {
            let mut child = create_track(child_name, child_type, None, &[]);
            child.set_group(child_group);
            
            let parent_name = parent_override.unwrap_or(name);
            child.set_parent_name(parent_name);
            
            tracks.push(child);
        }
    }
    
    tracks
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    fn test_parse_fts_structure() {
        let track_names = &["Kick In", "Kick Out", "Snare Top", "Snare Bottom"];
        let template = parse_fts_structure(track_names);
        
        // Structure is BUS -> SUM -> tracks
        assert_eq!(template.tracks.len(), 4 + 4); // 2 sets of BUS/SUM + 4 tracks
        assert!(template.tracks.iter().any(|t| t.name.0 == "Kick In"));
        assert!(template.tracks.iter().any(|t| t.name.0 == "Snare Top"));
    }
}
