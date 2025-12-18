//! Helper functions for parsing track names into templates
//!
//! Provides easy-to-use functions that combine parsing, template creation,
//! and fold pattern transformations.

use crate::smart_template::naming::implementations::{
    kick::KickParser, snare::SnareParser, tom::TomParser,
    cymbals::CymbalsParser, room::RoomParser,
};
use crate::smart_template::naming::parser::Parser;
use crate::smart_template::core::template::Template;
use crate::smart_template::helpers::track_helpers::{create_track, TrackExt};
use crate::smart_template::transform::fold::*;
use daw::tracks::Track;

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
    let kick_parser = KickParser::new();
    let snare_parser = SnareParser::new();
    let tom_parser = TomParser::new();
    let cymbals_parser = CymbalsParser::new();
    let room_parser = RoomParser::new();
    
    // Parse all track names into a Vec<Track> with groups
    let mut parsed_tracks = Vec::new();
    
    for track_name in track_names {
        let group = if kick_parser.parse(track_name).is_ok() {
            Some("kick")
        } else if snare_parser.parse(track_name).is_ok() {
            Some("snare")
        } else if tom_parser.parse(track_name).is_ok() {
            Some("tom")
        } else if cymbals_parser.parse(track_name).is_ok() {
            Some("cymbals")
        } else if room_parser.parse(track_name).is_ok() {
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
    let mut current_group_tracks = Vec::new();
    
    for track in parsed_tracks {
        let track_group = track.get_group().unwrap_or_default();
        
        // If we hit a new group, build hierarchy for previous group
        if current_group.is_some() && current_group.as_ref() != Some(&track_group) {
            if !current_group_tracks.is_empty() {
                let group_name = current_group.as_ref().unwrap();
                let bus_name = format!("{}", group_name.chars().next().unwrap().to_uppercase().collect::<String>() + &group_name[1..]);
                let sum_name = format!("{} (SUM)", bus_name);
                
                // Build hierarchy: BUS -> SUM -> individual tracks
                // Take ownership of current_group_tracks
                let group_tracks = std::mem::take(&mut current_group_tracks);
                let sum_tracks = {
                    let mut builder = Track::builder();
                    builder.name(sum_name);
                    builder.add_child(group_tracks)
                };
                
                let bus_tracks = {
                    let mut builder = Track::builder();
                    builder.name(bus_name);
                    builder.add_child(sum_tracks)
                };
                
                all_tracks.extend(bus_tracks);
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
        
        // Take ownership of current_group_tracks
        let group_tracks = std::mem::take(&mut current_group_tracks);
        let sum_tracks = {
            let mut builder = Track::builder();
            builder.name(sum_name);
            builder.add_child(group_tracks)
        };
        
        let bus_tracks = {
            let mut builder = Track::builder();
            builder.name(bus_name);
            builder.add_child(sum_tracks)
        };
        
        all_tracks.extend(bus_tracks);
    }
    
    // Create initial template
    let template = Template {
        name: "Parsed Tracks".to_string(),
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
///
/// Usage (simple - just name and group):
/// ```rust
/// let expected = tracks!(
///     ("Kick In", "kick"),
///     ("Kick Out", "kick"),
///     ("Snare Top", "snare"),
/// );
/// ```
///
/// Usage (with hierarchy - name, group, and optional parent):
/// ```rust
/// let expected = tracks!(
///     ("Snare", "snare", None),  // Root track
///     ("Snare Top", "snare", Some("Snare")),  // Child of Snare
///     ("Snare Bottom", "snare", Some("Snare")),  // Child of Snare
/// );
/// ```
///
/// Usage (with track type):
/// ```rust
/// let expected = tracks!(
///     ("Snare", "snare", None, Some("BUS")),  // Root with type
///     ("Snare Top", "snare", Some("Snare"), None),  // Child without type
/// );
/// ```
///
/// This macro uses `Track::builder().name()` to create tracks, then sets the group, parent, and type.
#[macro_export]
macro_rules! tracks {
    // Simple form: (name, group)
    ($(($name:expr, $group:expr)),* $(,)?) => {
        {
            use $crate::smart_template::helpers::track_helpers::{create_track, TrackExt};
            
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
    
    // With parent: (name, group, parent)
    ($(($name:expr, $group:expr, $parent:expr)),* $(,)?) => {
        {
            use $crate::smart_template::helpers::track_helpers::{create_track, TrackExt};
            
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
    
    // Full form: (name, group, parent, track_type)
    ($(($name:expr, $group:expr, $parent:expr, $track_type:expr)),* $(,)?) => {
        {
            use $crate::smart_template::helpers::track_helpers::{create_track, TrackExt};
            
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
///
/// This makes it easy to define parent-child relationships when building test expectations.
///
/// # Example
/// ```rust
/// let tracks = build_tracks_with_hierarchy(vec![
///     ("Snare", "snare", Some("BUS"), None, vec![
///         ("Snare Top", "snare", None, None),
///         ("Snare Bottom", "snare", None, None),
///     ]),
/// ]);
/// ```
///
/// This creates:
/// - "Snare" track with group "snare" and type "BUS" (root)
/// - "Snare Top" track with group "snare" and parent "Snare"
/// - "Snare Bottom" track with group "snare" and parent "Snare"
///
/// # Arguments
/// * `hierarchy` - Vector of tuples: (name, group, track_type, parent, children)
///   - `name`: Track name
///   - `group`: Group name
///   - `track_type`: Optional track type (e.g., "BUS", "SUM")
///   - `parent`: Optional parent name (None for root tracks)
///   - `children`: Vector of child tracks: (name, group, track_type, parent_override)
pub fn build_tracks_with_hierarchy(
    hierarchy: Vec<(
        &str,                    // name
        &str,                    // group
        Option<&str>,            // track_type
        Option<&str>,            // parent (for root, this should be None)
        Vec<(&str, &str, Option<&str>, Option<&str>)>, // children: (name, group, track_type, parent_name_override)
    )>,
) -> Vec<Track> {
    use crate::smart_template::helpers::track_helpers::{TrackExt, create_track};
    
    let mut tracks = Vec::new();
    
    for (name, group, track_type, _parent, children) in hierarchy {
        // Create parent/root track using create_track helper
        let mut track = create_track(name, track_type, None, &[]);
        track.set_group(group);
        tracks.push(track);
        
        // Create children
        for (child_name, child_group, child_type, parent_override) in children {
            let mut child = create_track(child_name, child_type, None, &[]);
            child.set_group(child_group);
            
            // Use override parent name if provided, otherwise use the parent's name
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
        
        assert_eq!(template.tracks.len(), 4);
        assert!(template.tracks.iter().any(|t| t.name == "Kick In"));
        assert!(template.tracks.iter().any(|t| t.name == "Snare Top"));
    }
    
    #[test]
    fn test_tracks_macro() {
        let expected = tracks!(
            ("Kick In", "kick"),
            ("Kick Out", "kick"),
            ("Snare Top", "snare"),
        );
        
        assert_eq!(expected.len(), 3);
        assert_eq!(expected[0].name, "Kick In");
        assert_eq!(expected[0].get_group(), Some("kick".to_string()));
    }
    
    #[test]
    fn test_tracks_macro_with_parent() {
        let expected = tracks!(
            ("Snare", "snare", None),
            ("Snare Top", "snare", Some("Snare")),
            ("Snare Bottom", "snare", Some("Snare")),
        );
        
        assert_eq!(expected.len(), 3);
        assert_eq!(expected[0].name, "Snare");
        assert_eq!(expected[0].parent_name(), None);
        assert_eq!(expected[1].name, "Snare Top");
        assert_eq!(expected[1].parent_name(), Some("Snare"));
    }
    
    #[test]
    fn test_tracks_macro_with_type() {
        let expected = tracks!(
            ("Snare", "snare", None, Some("BUS")),
            ("Snare Top", "snare", Some("Snare"), None),
        );
        
        assert_eq!(expected.len(), 2);
        assert_eq!(expected[0].track_type(), Some("BUS"));
        assert_eq!(expected[1].track_type(), None);
    }
    
    #[test]
    fn test_build_tracks_with_hierarchy() {
        use super::build_tracks_with_hierarchy;
        
        let tracks = build_tracks_with_hierarchy(vec![
            ("Snare", "snare", Some("BUS"), None, vec![
                ("Snare Top", "snare", None, None),
                ("Snare Bottom", "snare", None, None),
            ]),
        ]);
        
        assert_eq!(tracks.len(), 3);
        assert_eq!(tracks[0].name, "Snare");
        assert_eq!(tracks[0].track_type(), Some("BUS"));
        assert_eq!(tracks[1].name, "Snare Top");
        assert_eq!(tracks[1].parent_name(), Some("Snare"));
        assert_eq!(tracks[2].name, "Snare Bottom");
        assert_eq!(tracks[2].parent_name(), Some("Snare"));
    }
}
