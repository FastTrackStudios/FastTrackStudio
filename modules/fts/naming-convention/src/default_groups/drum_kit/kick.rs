//! Default Kick group configuration
//!
//! Based on the fts-naming-parser config.json structure, converted to
//! the new nested FullGroup format using declarative macros.
//!
//! Multi-mic descriptors (In, Out, Trig, Sub, Ambient) each have their own
//! patterns and negative patterns.

use crate::{FullGroup, define_group, TrackStructure};

/// Create the default Kick group with multi-mic descriptors
pub fn create_kick_group() -> FullGroup {
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
}
