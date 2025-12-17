//! Default Kick group configuration
//!
//! Based on the fts-naming-parser config.json structure, converted to
//! the new nested Group format using declarative macros.
//!
//! Multi-mic descriptors (In, Out, Trig, Sub, Ambient) each have their own
//! patterns and negative patterns.

use super::super::super::{Group, TrackStructure, GroupTrackListProvider, RecordingTrackListProvider};
use crate::smart_template::track_template::TrackList;

/// Create the default Kick group with multi-mic descriptors
pub fn create_kick_group() -> Group {
    let mut kick = define_group! {
        name = "Kick",
        prefix = "K",
        patterns = ["kick", "kik", "bd", "bassdrum"],
        negative_patterns = ["keys", "guitar", "gtr", "k", "bass"],
        parent_track = "D KICK (Sum)",
        arrangement_patterns = ["Thump", "Click", "Sub", "Beater", "Shell", "Fundamental", "Harmonic", "Trig"],
        layers_patterns = ["Click", "Thump", "Attack", "Body"],
    };
    
    kick.multi_mic("In")
        .patterns(["Inside", "Internal", "Beater", "Attack"]);
    
    kick.multi_mic("Out")
        .patterns(["Outside", "External", "Shell", "Body"]);
    
    kick.multi_mic("Trig")
        .patterns(["trigger", "Trigger", "Sample", "Replacement"]);
    
    kick.multi_mic("Sub")
        .patterns(["Deep", "Low", "Fundamental", "Thump"]);
    
    kick.multi_mic("Ambient");
    
    kick
}

/// Create the track structure for the Kick group
/// 
/// Structure:
/// ```
/// Kick (BUS)
/// ├── Kick (SUM)
/// │   ├── Kick In
/// │   ├── Kick Out
/// │   └── Kick Trig
/// ├── Kick Sub
/// └── Kick Ambient
/// ```
pub fn create_kick_track_structure() -> TrackStructure {
    let mut kick_bus = TrackStructure::with_track_type("Kick", "BUS");
    
    // Kick SUM with multi-mic tracks
    let mut kick_sum = TrackStructure::with_track_type("Kick", "SUM");
    kick_sum.add_child(TrackStructure::new("Kick In"));
    kick_sum.add_child(TrackStructure::new("Kick Out"));
    kick_sum.add_child(TrackStructure::new("Kick Trig"));
    
    kick_bus.add_child(kick_sum);
    kick_bus.add_child(TrackStructure::new("Kick Sub"));
    kick_bus.add_child(TrackStructure::new("Kick Ambient"));
    
    kick_bus
}

/// Kick group implementation of GroupTrackListProvider
/// 
/// This wrapper type allows the Kick group to provide custom track structure
/// implementations for different presets.
pub struct KickGroupTrackList {
    group: Group,
}

impl KickGroupTrackList {
    /// Create a new KickGroupTrackList from a Group
    /// 
    /// # Panics
    /// Panics if the group name is not "Kick"
    pub fn new(group: Group) -> Self {
        assert_eq!(group.name, "Kick", "KickGroupTrackList can only be used with Kick group");
        Self { group }
    }
    
    /// Get a reference to the underlying group
    pub fn group(&self) -> &Group {
        &self.group
    }
    
    /// Create from the default kick group
    pub fn from_default() -> Self {
        Self::new(create_kick_group())
    }
    
    /// Convenience methods for each preset
    pub fn full(&self) -> TrackStructure {
        self.full_track_structure()
    }
    
    pub fn default_preset(&self) -> TrackStructure {
        self.default_track_structure()
    }
    
    pub fn minimal(&self) -> TrackStructure {
        self.minimal_track_structure()
    }
    
    /// Get TrackList for each preset
    pub fn full_track_list(&self) -> TrackList {
        self.structure_to_track_list(&self.full_track_structure())
    }
    
    pub fn default_track_list(&self) -> TrackList {
        self.structure_to_track_list(&self.default_track_structure())
    }
    
    pub fn minimal_track_list(&self) -> TrackList {
        self.structure_to_track_list(&self.minimal_track_structure())
    }
}

/// Module-style API for Kick group presets
/// 
/// Usage:
/// ```rust
/// use fts::smart_template::naming_convention::default_groups::drum_kit::kick::Kick;
/// 
/// let full_structure = Kick::Full.structure();
/// let recording_tracks = Kick::Recording.track_list();
/// let recording_names = Kick::Recording.track_names(); // For filtering/visibility
/// ```
pub mod Kick {
    use super::*;
    
    /// Kick group preset provider for base presets (Full, Default, Minimal)
    pub struct BasePreset {
        get_structure: fn(&KickGroupTrackList) -> TrackStructure,
    }
    
    impl BasePreset {
        /// Get the TrackStructure for this preset
        pub fn structure(&self) -> TrackStructure {
            let kick = KickGroupTrackList::from_default();
            (self.get_structure)(&kick)
        }
        
        /// Get the TrackList for this preset
        pub fn track_list(&self) -> TrackList {
            let kick = KickGroupTrackList::from_default();
            kick.structure_to_track_list(&(self.get_structure)(&kick))
        }
        
        /// Get track names from this preset (for filtering/visibility)
        pub fn track_names(&self) -> Vec<String> {
            let structure = self.structure();
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
    
    /// Recording preset provider (implements RecordingTrackListProvider)
    pub struct RecordingPreset;
    
    impl RecordingPreset {
        /// Get the TrackStructure for recording preset
        pub fn structure(&self) -> TrackStructure {
            let kick = KickGroupTrackList::from_default();
            kick.recording_track_structure()
        }
        
        /// Get the TrackList for recording preset
        pub fn track_list(&self) -> TrackList {
            let kick = KickGroupTrackList::from_default();
            kick.recording_track_list()
        }
        
        /// Get track names from recording preset (for filtering/visibility)
        pub fn track_names(&self) -> Vec<String> {
            let kick = KickGroupTrackList::from_default();
            kick.recording_track_names()
        }
        
        /// Helper to collect track names from a structure
        pub fn collect_track_names(structure: &TrackStructure) -> Vec<String> {
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
    
    /// Full preset - all track possibilities
    pub const Full: BasePreset = BasePreset { 
        get_structure: |k| k.full_track_structure() 
    };
    
    /// Default preset - good starting point
    pub const Default: BasePreset = BasePreset { 
        get_structure: |k| k.default_track_structure() 
    };
    
    /// Minimal preset - absolute minimum to get started
    pub const Minimal: BasePreset = BasePreset { 
        get_structure: |k| k.minimal_track_structure() 
    };
    
    /// Recording preset - just audio source tracks
    pub const Recording: RecordingPreset = RecordingPreset;
}

impl GroupTrackListProvider for KickGroupTrackList {
    /// Default: Good starting point (full structure for Kick)
    fn default_track_structure(&self) -> TrackStructure {
        create_kick_track_structure()
    }
    
    /// Full: Complete structure with all tracks (same as default for Kick)
    fn full_track_structure(&self) -> TrackStructure {
        create_kick_track_structure()
    }
    
    /// Minimal: Just the Kick BUS track
    fn minimal_track_structure(&self) -> TrackStructure {
        TrackStructure::with_track_type("Kick", "BUS")
    }
}

impl RecordingTrackListProvider for KickGroupTrackList {
    /// Recording: Only audio source tracks (In, Out, Trig)
    /// Structure:
    /// ```
    /// Kick
    /// ├── Kick In
    /// ├── Kick Out
    /// └── Kick Trig
    /// ```
    fn recording_track_structure(&self) -> TrackStructure {
        let mut result = TrackStructure::new("Kick");
        result.add_child(TrackStructure::new("Kick In"));
        result.add_child(TrackStructure::new("Kick Out"));
        result.add_child(TrackStructure::new("Kick Trig"));
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_kick_group() {
        let kick = create_kick_group();
        
        assert_eq!(kick.name, "Kick");
        assert_eq!(kick.prefix, "K");
        assert!(!kick.patterns.is_empty());
        // In/Out/Trigger/Sub are multi-mic patterns, not nested groups
        assert!(!kick.has_children());
    }

    #[test]
    fn test_kick_group_structure() {
        let kick = create_kick_group();
        
        // In/Out/Trigger/Sub/Ambient are multi-mic descriptors, not nested groups
        assert!(!kick.has_children());
        
        // Verify multi-mic descriptors exist
        let descriptor_names = kick.multi_mic_descriptor_names();
        assert_eq!(descriptor_names.len(), 5);
        assert!(descriptor_names.contains(&"In".to_string()));
        assert!(descriptor_names.contains(&"Out".to_string()));
        assert!(descriptor_names.contains(&"Trig".to_string()));
        assert!(descriptor_names.contains(&"Sub".to_string()));
        assert!(descriptor_names.contains(&"Ambient".to_string()));
    }

    #[test]
    fn test_kick_group_patterns() {
        let kick = create_kick_group();
        
        // Verify main patterns
        assert!(kick.patterns.contains(&"kick".to_string()));
        assert!(kick.patterns.contains(&"kik".to_string()));
        assert!(kick.patterns.contains(&"bd".to_string()));
        assert!(kick.patterns.contains(&"bassdrum".to_string()));
        
        // Verify negative patterns
        assert!(kick.negative_patterns.contains(&"keys".to_string()));
        assert!(kick.negative_patterns.contains(&"guitar".to_string()));
        
        // Verify arrangement patterns
        let arrangement_patterns = kick.arrangement_patterns();
        assert!(!arrangement_patterns.is_empty());
        assert!(arrangement_patterns.contains(&"Thump".to_string()));
        assert!(arrangement_patterns.contains(&"Click".to_string()));
        
        // Verify multi-mic descriptors have their own patterns
        let in_desc = kick.find_multi_mic_descriptor("In").unwrap();
        assert!(in_desc.patterns.contains(&"Inside".to_string()));
        assert!(in_desc.patterns.contains(&"Internal".to_string()));
        assert!(in_desc.patterns.contains(&"Beater".to_string()));
        assert!(in_desc.patterns.contains(&"Attack".to_string()));
        
        let out_desc = kick.find_multi_mic_descriptor("Out").unwrap();
        assert!(out_desc.patterns.contains(&"Outside".to_string()));
        assert!(out_desc.patterns.contains(&"External".to_string()));
        assert!(out_desc.patterns.contains(&"Shell".to_string()));
        assert!(out_desc.patterns.contains(&"Body".to_string()));
        
        let trig_desc = kick.find_multi_mic_descriptor("Trig").unwrap();
        assert!(trig_desc.patterns.contains(&"trigger".to_string()));
        assert!(trig_desc.patterns.contains(&"Trigger".to_string()));
        assert!(trig_desc.patterns.contains(&"Sample".to_string()));
        assert!(trig_desc.patterns.contains(&"Replacement".to_string()));
        
        let sub_desc = kick.find_multi_mic_descriptor("Sub").unwrap();
        assert!(sub_desc.patterns.contains(&"Deep".to_string()));
        assert!(sub_desc.patterns.contains(&"Low".to_string()));
        assert!(sub_desc.patterns.contains(&"Fundamental".to_string()));
        assert!(sub_desc.patterns.contains(&"Thump".to_string()));
        
        let ambient_desc = kick.find_multi_mic_descriptor("Ambient").unwrap();
        assert!(ambient_desc.patterns.is_empty()); // Ambient has no additional patterns
    }

    #[test]
    fn test_create_kick_track_structure() {
        let structure = create_kick_track_structure();
        
        assert_eq!(structure.name, "Kick");
        assert_eq!(structure.track_type, Some("BUS".to_string()));
        assert!(structure.has_children());
        assert_eq!(structure.children.len(), 3);
        
        // Check Kick SUM
        let kick_sum = structure.find_child("Kick").unwrap();
        assert_eq!(kick_sum.track_type, Some("SUM".to_string()));
        assert!(kick_sum.has_children());
        assert_eq!(kick_sum.children.len(), 3);
        
        // Check multi-mic tracks
        assert!(kick_sum.find_child("Kick In").is_some());
        assert!(kick_sum.find_child("Kick Out").is_some());
        assert!(kick_sum.find_child("Kick Trig").is_some());
        
        // Check other Kick tracks
        assert!(structure.find_child("Kick Sub").is_some());
        assert!(structure.find_child("Kick Ambient").is_some());
    }
    
    #[test]
    fn test_kick_group_track_list_provider() {
        let kick = KickGroupTrackList::from_default();
        
        // Test base presets
        let full = kick.full_track_structure();
        assert_eq!(full.name, "Kick");
        assert_eq!(full.track_type, Some("BUS".to_string()));
        
        let default = kick.default_track_structure();
        assert_eq!(default.name, "Kick");
        
        let minimal = kick.minimal_track_structure();
        assert_eq!(minimal.name, "Kick");
        assert_eq!(minimal.track_type, Some("BUS".to_string()));
        assert!(!minimal.has_children());
    }
    
    #[test]
    fn test_kick_recording_track_list_provider() {
        let kick = KickGroupTrackList::from_default();
        
        // Test recording preset
        let recording = kick.recording_track_structure();
        assert_eq!(recording.name, "Kick");
        assert!(recording.has_children());
        assert_eq!(recording.children.len(), 3);
        
        // Check recording tracks
        let track_names: Vec<String> = recording.children.iter()
            .map(|c| c.name.clone())
            .collect();
        assert!(track_names.contains(&"Kick In".to_string()));
        assert!(track_names.contains(&"Kick Out".to_string()));
        assert!(track_names.contains(&"Kick Trig".to_string()));
        
        // Test track names for filtering/visibility
        let names = kick.recording_track_names();
        assert!(names.contains(&"Kick In".to_string()));
        assert!(names.contains(&"Kick Out".to_string()));
        assert!(names.contains(&"Kick Trig".to_string()));
    }
    
    #[test]
    fn test_kick_module_api() {
        // Test module-style API
        use Kick;
        
        // Test Full preset
        let full_structure = Kick::Full.structure();
        assert_eq!(full_structure.name, "Kick");
        assert!(full_structure.has_children());
        
        // Test Default preset
        let default_structure = Kick::Default.structure();
        assert_eq!(default_structure.name, "Kick");
        
        // Test Minimal preset
        let minimal_structure = Kick::Minimal.structure();
        assert_eq!(minimal_structure.name, "Kick");
        assert_eq!(minimal_structure.track_type, Some("BUS".to_string()));
        
        // Test Recording preset
        let recording_structure = Kick::Recording.structure();
        assert_eq!(recording_structure.name, "Kick");
        assert_eq!(recording_structure.children.len(), 3);
        
        // Test track names for filtering
        let recording_names = Kick::Recording.track_names();
        assert_eq!(recording_names.len(), 4); // Kick + 3 children
        assert!(recording_names.contains(&"Kick".to_string()));
        assert!(recording_names.contains(&"Kick In".to_string()));
        assert!(recording_names.contains(&"Kick Out".to_string()));
        assert!(recording_names.contains(&"Kick Trig".to_string()));
    }
}
