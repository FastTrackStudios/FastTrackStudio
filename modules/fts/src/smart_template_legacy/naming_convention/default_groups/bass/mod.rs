//! Default Bass group configuration
//!
//! Bass group includes Bass Guitar and Synth Bass as sub-types.
//! Bass Guitar has multi-mic descriptors (DI, Amp).

use super::super::{Group, TrackStructure};

/// Create the default Bass group with sub-types
pub fn create_bass_group() -> Group {
    let mut bass = define_group! {
        name = "Bass",
        prefix = "B",
        patterns = ["bass"],
        negative_patterns = ["bassdrum", "bd"],
        priority = "10",
        parent_track = "B (BUS)",
        children = [
            define_group! {
                name = "Bass Guitar",
                prefix = "Bass",
                patterns = ["bass guitar", "bassguitar", "bg", "electric bass"],
            },
            define_group! {
                name = "Synth Bass",
                prefix = "SY Bass",
                patterns = ["synth bass", "synthbass", "sb", "bass synth"],
            },
        ],
    };
    
    // Add multi-mic descriptors to Bass Guitar
    let bass_guitar = bass.find_child_mut("Bass Guitar").unwrap();
    bass_guitar.multi_mic("DI")
        .patterns(["di", "DI", "direct", "Direct"]);
    
    bass_guitar.multi_mic("Amp")
        .patterns(["amp", "Amp", "amplifier", "Amplifier"]);
    
    bass
}

/// Create the track structure for the Bass group
/// 
/// Structure:
/// ```
/// Bass (BUS)
/// ├── Bass Guitar (BUS)
/// │   ├── Bass Guitar DI
/// │   └── Bass Guitar Amp
/// └── Synth Bass
/// ```
pub fn create_bass_track_structure() -> TrackStructure {
    let mut bass_bus = TrackStructure::with_track_type("Bass", "BUS");
    
    // Bass Guitar structure
    let mut bass_guitar_bus = TrackStructure::with_track_type("Bass Guitar", "BUS");
    bass_guitar_bus.add_child(TrackStructure::new("Bass Guitar DI"));
    bass_guitar_bus.add_child(TrackStructure::new("Bass Guitar Amp"));
    
    bass_bus.add_child(bass_guitar_bus);
    bass_bus.add_child(TrackStructure::new("Synth Bass"));
    
    bass_bus
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_bass_group() {
        let bass = create_bass_group();
        
        assert_eq!(bass.name, "Bass");
        assert_eq!(bass.prefix, "B");
        assert!(bass.patterns.contains(&"bass".to_string()));
        assert!(bass.has_children());
        assert_eq!(bass.children.len(), 2);
    }

    #[test]
    fn test_bass_group_structure() {
        let bass = create_bass_group();
        
        // Verify all sub-type child groups exist
        assert!(bass.has_children());
        assert!(bass.find_child("Bass Guitar").is_some());
        assert!(bass.find_child("Synth Bass").is_some());
        
        // Verify child group patterns
        let bass_guitar = bass.find_child("Bass Guitar").unwrap();
        assert_eq!(bass_guitar.prefix, "Bass");
        assert!(bass_guitar.patterns.contains(&"bass guitar".to_string()));
        assert!(bass_guitar.patterns.contains(&"bg".to_string()));
        
        let synth_bass = bass.find_child("Synth Bass").unwrap();
        assert_eq!(synth_bass.prefix, "SY Bass");
        assert!(synth_bass.patterns.contains(&"synth bass".to_string()));
        assert!(synth_bass.patterns.contains(&"sb".to_string()));
    }

    #[test]
    fn test_bass_guitar_multi_mic_descriptors() {
        let bass = create_bass_group();
        let bass_guitar = bass.find_child("Bass Guitar").unwrap();
        
        // Verify multi-mic descriptors exist
        let descriptor_names = bass_guitar.multi_mic_descriptor_names();
        assert_eq!(descriptor_names.len(), 2);
        assert!(descriptor_names.contains(&"DI".to_string()));
        assert!(descriptor_names.contains(&"Amp".to_string()));
        
        // Verify DI patterns
        let di_desc = bass_guitar.find_multi_mic_descriptor("DI").unwrap();
        assert!(di_desc.patterns.contains(&"di".to_string()));
        assert!(di_desc.patterns.contains(&"DI".to_string()));
        assert!(di_desc.patterns.contains(&"direct".to_string()));
        assert!(di_desc.patterns.contains(&"Direct".to_string()));
        
        // Verify Amp patterns
        let amp_desc = bass_guitar.find_multi_mic_descriptor("Amp").unwrap();
        assert!(amp_desc.patterns.contains(&"amp".to_string()));
        assert!(amp_desc.patterns.contains(&"Amp".to_string()));
        assert!(amp_desc.patterns.contains(&"amplifier".to_string()));
        assert!(amp_desc.patterns.contains(&"Amplifier".to_string()));
    }

    #[test]
    fn test_synth_bass_no_multi_mic() {
        let bass = create_bass_group();
        let synth_bass = bass.find_child("Synth Bass").unwrap();
        
        // Verify Synth Bass has no multi-mic descriptors
        assert!(synth_bass.multi_mic_descriptors.is_empty());
    }

    #[test]
    fn test_create_bass_track_structure() {
        let structure = create_bass_track_structure();
        
        assert_eq!(structure.name, "Bass");
        assert_eq!(structure.track_type, Some("BUS".to_string()));
        assert!(structure.has_children());
        assert_eq!(structure.children.len(), 2);
        
        // Check Bass Guitar BUS
        let bass_guitar_bus = structure.find_child("Bass Guitar").unwrap();
        assert_eq!(bass_guitar_bus.track_type, Some("BUS".to_string()));
        assert!(bass_guitar_bus.has_children());
        assert_eq!(bass_guitar_bus.children.len(), 2);
        
        // Check Bass Guitar tracks
        assert!(bass_guitar_bus.find_child("Bass Guitar DI").is_some());
        assert!(bass_guitar_bus.find_child("Bass Guitar Amp").is_some());
        
        // Check Synth Bass
        assert!(structure.find_child("Synth Bass").is_some());
        let synth_bass = structure.find_child("Synth Bass").unwrap();
        assert_eq!(synth_bass.track_type, None);
    }
}

