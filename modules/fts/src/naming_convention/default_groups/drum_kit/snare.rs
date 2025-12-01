//! Default Snare group configuration
//!
//! Based on the fts-naming-parser config.json structure, converted to
//! the new nested FullGroup format using declarative macros.
//!
//! Multi-mic descriptors (Top, Bottom, Trig) each have their own patterns.
//! Effect patterns (Verb) are defined separately.

use crate::{FullGroup, define_group, TrackStructure};

/// Create the default Snare group with multi-mic descriptors and effect patterns
pub fn create_snare_group() -> FullGroup {
    let mut snare = define_group! {
        name = "Snare",
        prefix = "S",
        patterns = ["snare", "sn"],
        negative_patterns = [],
        parent_track = "D SNARE (Sum)",
    };
    
    // Multi-mic descriptors
    snare.multi_mic("Top")
        .patterns(["top", "Top"]);
    
    snare.multi_mic("Bottom")
        .patterns(["bottom", "Bottom", "bot"]);
    
    snare.multi_mic("Trig")
        .patterns(["trigger", "Trigger", "trig"]);
    
    // Effect patterns (Verb can combine with multi-mic positions)
    snare.set_effect_patterns(vec!["verb".to_string(), "Verb".to_string(), "reverb".to_string(), "Reverb".to_string()]);
    
    snare
}

/// Create the track structure for the Snare group
/// 
/// Structure:
/// ```
/// Snare (BUS)
/// ├── Snare (SUM)
/// │   ├── Snare Top
/// │   ├── Snare Bottom
/// │   └── Snare Trig
/// └── Snare Verb
/// ```
pub fn create_snare_track_structure() -> TrackStructure {
    let mut snare_bus = TrackStructure::with_track_type("Snare", "BUS");
    
    // Snare SUM with multi-mic tracks
    let mut snare_sum = TrackStructure::with_track_type("Snare", "SUM");
    snare_sum.add_child(TrackStructure::new("Snare Top"));
    snare_sum.add_child(TrackStructure::new("Snare Bottom"));
    snare_sum.add_child(TrackStructure::new("Snare Trig"));
    
    snare_bus.add_child(snare_sum);
    snare_bus.add_child(TrackStructure::new("Snare Verb"));
    
    snare_bus
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_snare_group() {
        let snare = create_snare_group();
        
        assert_eq!(snare.name, "Snare");
        assert_eq!(snare.prefix, "S");
        assert!(!snare.patterns.is_empty());
        // Top/Bottom/Trig are multi-mic patterns, not nested groups
        assert!(!snare.has_children());
    }

    #[test]
    fn test_snare_group_structure() {
        let snare = create_snare_group();
        
        // Top/Bottom/Trig are multi-mic descriptors, not nested groups
        assert!(!snare.has_children());
        
        // Verify multi-mic descriptors exist
        let descriptor_names = snare.multi_mic_descriptor_names();
        assert_eq!(descriptor_names.len(), 3);
        assert!(descriptor_names.contains(&"Top".to_string()));
        assert!(descriptor_names.contains(&"Bottom".to_string()));
        assert!(descriptor_names.contains(&"Trig".to_string()));
        
        // Verify effect patterns exist
        let effect_patterns = snare.effect_patterns();
        assert!(!effect_patterns.is_empty());
        assert!(effect_patterns.contains(&"verb".to_string()));
        assert!(effect_patterns.contains(&"Verb".to_string()));
    }

    #[test]
    fn test_snare_group_patterns() {
        let snare = create_snare_group();
        
        // Verify main patterns
        assert!(snare.patterns.contains(&"snare".to_string()));
        assert!(snare.patterns.contains(&"sn".to_string()));
        
        // Verify multi-mic descriptors have their own patterns
        let top_desc = snare.find_multi_mic_descriptor("Top").unwrap();
        assert!(top_desc.patterns.contains(&"top".to_string()));
        assert!(top_desc.patterns.contains(&"Top".to_string()));
        
        let bottom_desc = snare.find_multi_mic_descriptor("Bottom").unwrap();
        assert!(bottom_desc.patterns.contains(&"bottom".to_string()));
        assert!(bottom_desc.patterns.contains(&"Bottom".to_string()));
        assert!(bottom_desc.patterns.contains(&"bot".to_string()));
        
        let trig_desc = snare.find_multi_mic_descriptor("Trig").unwrap();
        assert!(trig_desc.patterns.contains(&"trigger".to_string()));
        assert!(trig_desc.patterns.contains(&"Trigger".to_string()));
        assert!(trig_desc.patterns.contains(&"trig".to_string()));
    }

    #[test]
    fn test_create_snare_track_structure() {
        let structure = create_snare_track_structure();
        
        assert_eq!(structure.name, "Snare");
        assert_eq!(structure.track_type, Some("BUS".to_string()));
        assert!(structure.has_children());
        assert_eq!(structure.children.len(), 2);
        
        // Check Snare SUM
        let snare_sum = structure.find_child("Snare").unwrap();
        assert_eq!(snare_sum.track_type, Some("SUM".to_string()));
        assert!(snare_sum.has_children());
        assert_eq!(snare_sum.children.len(), 3);
        
        // Check multi-mic tracks
        assert!(snare_sum.find_child("Snare Top").is_some());
        assert!(snare_sum.find_child("Snare Bottom").is_some());
        assert!(snare_sum.find_child("Snare Trig").is_some());
        
        // Check Snare Verb
        assert!(structure.find_child("Snare Verb").is_some());
    }
}

