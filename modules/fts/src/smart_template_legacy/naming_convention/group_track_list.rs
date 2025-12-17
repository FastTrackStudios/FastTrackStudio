//! Group TrackList Provider
//!
//! Provides a trait-based system for groups to generate TrackList structures
//! with different presets. Groups can implement only the traits they need:
//! - GroupTrackListProvider (base, required for all groups)
//! - RecordingTrackListProvider (for groups with recording tracks)
//! - MidiTrackListProvider (for groups that need MIDI tracks)

use super::Group;
use super::track_structure::TrackStructure;
use crate::smart_template::track_template::{TrackList, Track};

/// Base trait for groups that can generate TrackList structures
/// 
/// All groups should implement at least the basic track structure methods.
pub trait GroupTrackListProvider {
    /// Generate the default TrackStructure for this group
    /// This is a good starting point with a balanced set of tracks
    fn default_track_structure(&self) -> TrackStructure;
    
    /// Generate TrackStructure for Full preset (all track possibilities)
    /// 
    /// This is the complete structure with all tracks including BUS, SUM, source tracks, effects, etc.
    /// Default implementation returns the default_track_structure.
    fn full_track_structure(&self) -> TrackStructure {
        self.default_track_structure()
    }
    
    /// Generate TrackStructure for Minimal preset (absolute minimum to get started)
    /// 
    /// Default implementation returns just the group name.
    fn minimal_track_structure(&self) -> TrackStructure {
        let default = self.default_track_structure();
        TrackStructure::new(default.name)
    }
    
    /// Convert a TrackStructure to a TrackList
    fn structure_to_track_list(&self, structure: &TrackStructure) -> TrackList {
        let mut track_list = TrackList::new();
        self.add_structure_to_list(&mut track_list, structure, None);
        track_list
    }
    
    /// Recursively add a TrackStructure to a TrackList
    fn add_structure_to_list(
        &self,
        track_list: &mut TrackList,
        structure: &TrackStructure,
        parent_name: Option<&str>,
    ) {
        // Create track from structure
        let mut track = if let Some(track_type) = &structure.track_type {
            Track::with_type(&structure.name, track_type)
        } else {
            Track::new(&structure.name)
        };
        
        // Set parent if provided
        if let Some(parent) = parent_name {
            track.set_parent(parent);
        }
        
        // Add track to list
        track_list.add_track(track);
        
        // Recursively add children
        let child_parent = if structure.track_type.is_some() {
            if let Some(parent_type) = &structure.track_type {
                format!("{} ({})", structure.name, parent_type)
            } else {
                structure.name.clone()
            }
        } else {
            structure.name.clone()
        };
        
        for child in &structure.children {
            let parent_ref = if child_parent.contains("(") {
                Some(child_parent.as_str())
            } else {
                Some(structure.name.as_str())
            };
            
            self.add_structure_to_list(track_list, child, parent_ref);
        }
    }
}

/// Trait for groups that have recording tracks (audio source tracks)
/// 
/// Groups like Kick, Snare, etc. that have multi-mic recordings should implement this.
/// Individual instrument groups typically implement this, not parent groups.
pub trait RecordingTrackListProvider: GroupTrackListProvider {
    /// Generate TrackStructure for Recording preset (only audio source tracks)
    /// 
    /// This should return only the actual recording tracks (e.g., Kick In, Kick Out, Kick Trig)
    /// without BUS/SUM tracks.
    fn recording_track_structure(&self) -> TrackStructure;
    
    /// Get TrackList for recording preset
    fn recording_track_list(&self) -> TrackList {
        self.structure_to_track_list(&self.recording_track_structure())
    }
    
    /// Get track names from recording preset (for filtering/visibility)
    fn recording_track_names(&self) -> Vec<String> {
        let structure = self.recording_track_structure();
        Self::collect_track_names(&structure)
    }
    
    /// Helper to collect track names from a structure
    fn collect_track_names(structure: &TrackStructure) -> Vec<String> {
        let mut names = vec![];
        Self::collect_names_recursive(structure, &mut names);
        names
    }
    
    fn collect_names_recursive(structure: &TrackStructure, names: &mut Vec<String>) {
        let track_name = if let Some(track_type) = &structure.track_type {
            format!("{} ({})", structure.name, track_type)
        } else {
            structure.name.clone()
        };
        names.push(track_name);
        
        for child in &structure.children {
            Self::collect_names_recursive(child, names);
        }
    }
}

/// Trait for groups that have MIDI tracks
/// 
/// Groups like Drums (parent group) that need MIDI tracks should implement this.
/// Individual instrument groups like Kick typically don't need this.
pub trait MidiTrackListProvider: GroupTrackListProvider {
    /// Generate TrackStructure for Midi preset (tracks for mocking with MIDI)
    /// 
    /// This should return tracks suitable for MIDI input/mocking.
    fn midi_track_structure(&self) -> TrackStructure;
    
    /// Get TrackList for MIDI preset
    fn midi_track_list(&self) -> TrackList {
        self.structure_to_track_list(&self.midi_track_structure())
    }
    
    /// Get track names from MIDI preset (for filtering/visibility)
    fn midi_track_names(&self) -> Vec<String> {
        let structure = self.midi_track_structure();
        Self::collect_track_names(&structure)
    }
    
    /// Helper to collect track names from a structure
    fn collect_track_names(structure: &TrackStructure) -> Vec<String> {
        let mut names = vec![];
        Self::collect_names_recursive(structure, &mut names);
        names
    }
    
    fn collect_names_recursive(structure: &TrackStructure, names: &mut Vec<String>) {
        let track_name = if let Some(track_type) = &structure.track_type {
            format!("{} ({})", structure.name, track_type)
        } else {
            structure.name.clone()
        };
        names.push(track_name);
        
        for child in &structure.children {
            Self::collect_names_recursive(child, names);
        }
    }
}

// Default implementation for Group
// This provides a fallback implementation that works for most groups.
// Groups with specific requirements should use wrapper types like KickGroupTrackList.
impl GroupTrackListProvider for Group {
    fn default_track_structure(&self) -> TrackStructure {
        // Try to use group-specific track structure functions
        // This is a fallback - groups should provide their own implementations
        // via wrapper types like KickGroupTrackList
        
        // Check for known groups with specific track structures
        match self.name.as_str() {
            "Kick" => {
                // Use the Kick-specific track structure
                #[cfg(feature = "default-groups")]
                {
                    use crate::smart_template::naming_convention::default_groups::drum_kit::kick::create_kick_track_structure;
                    return create_kick_track_structure();
                }
                // Fall through to default if feature not enabled
            }
            _ => {}
        }
        
        // Default implementation: create a simple structure from the group
        let mut structure = TrackStructure::new(&self.name);
        
        // Add child groups as tracks
        for child_group in &self.children {
            let child_structure = child_group.default_track_structure();
            structure.add_child(child_structure);
        }
        
        // Add multi-mic descriptors as tracks
        for descriptor in &self.multi_mic_descriptors {
            structure.add_child(TrackStructure::new(format!("{} {}", self.name, descriptor.name)));
        }
        
        structure
    }
}
