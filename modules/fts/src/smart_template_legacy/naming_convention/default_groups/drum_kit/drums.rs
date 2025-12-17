//! Default Drums group configuration
//!
//! The Drums group is the parent container for all drum-related groups:
//! - Kick
//! - Tom
//! - Cymbals
//!
//! This group provides the top-level organization for drum tracks.

use super::super::super::Group;
use super::{
    kick::create_kick_group, 
    snare::create_snare_group,
    tom::create_tom_group, 
    cymbals::create_cymbals_group
};

/// Create the default Drums group with all drum sub-groups as children
/// 
/// The children are added in a specific order for consistent sorting:
/// 1. Kick
/// 2. Snare
/// 3. Tom
/// 4. Cymbals
pub fn create_drums_group() -> Group {
    let mut drums = define_group! {
        name = "Drums",
        prefix = "D",
        patterns = ["drum", "drums"],
        parent_track = "D (BUS)",
    };
    
    // Add child groups in order (this order determines sorting)
    drums.add_child(create_kick_group());
    drums.add_child(create_snare_group());
    drums.add_child(create_tom_group());
    drums.add_child(create_cymbals_group());
    
    drums
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Group;

    #[test]
    fn test_create_drums_group() {
        let drums = create_drums_group();
        
        assert_eq!(drums.name, "Drums");
        assert_eq!(drums.prefix, "D");
        assert!(drums.patterns.contains(&"drum".to_string()));
        assert!(drums.patterns.contains(&"drums".to_string()));
        assert!(drums.has_children());
        assert_eq!(drums.children.len(), 3);
    }

    #[test]
    fn test_drums_group_structure() {
        let drums = create_drums_group();
        
        // Verify all child groups exist
        assert!(drums.find_child("Kick").is_some());
        assert!(drums.find_child("Snare").is_some());
        assert!(drums.find_child("Tom").is_some());
        assert!(drums.find_child("Cymbals").is_some());
    }

    #[test]
    fn test_drums_children_sorting_order() {
        let drums = create_drums_group();
        
        // Verify children are in the correct order
        // Order should be: Kick, Snare, Tom, Cymbals
        assert_eq!(drums.children.len(), 4);
        
        // Check order by index
        assert_eq!(drums.children[0].name, "Kick");
        assert_eq!(drums.children[1].name, "Snare");
        assert_eq!(drums.children[2].name, "Tom");
        assert_eq!(drums.children[3].name, "Cymbals");
        
        // Verify prefixes match expected order
        assert_eq!(drums.children[0].prefix, "K");
        assert_eq!(drums.children[1].prefix, "S");
        assert_eq!(drums.children[2].prefix, "T");
        assert_eq!(drums.children[3].prefix, "C");
    }

    #[test]
    fn test_drums_kick_child_structure() {
        let drums = create_drums_group();
        let kick = drums.find_child("Kick").unwrap();
        
        assert_eq!(kick.name, "Kick");
        assert_eq!(kick.prefix, "K");
        assert!(kick.patterns.contains(&"kick".to_string()));
        assert!(kick.patterns.contains(&"bd".to_string()));
        
        // Verify kick has multi-mic descriptors
        assert!(!kick.multi_mic_descriptors.is_empty());
        let multi_mic_names: Vec<String> = kick.multi_mic_descriptor_names();
        assert!(multi_mic_names.contains(&"In".to_string()));
        assert!(multi_mic_names.contains(&"Out".to_string()));
        assert!(multi_mic_names.contains(&"Trig".to_string()));
        assert!(multi_mic_names.contains(&"Sub".to_string()));
        assert!(multi_mic_names.contains(&"Ambient".to_string()));
    }

    #[test]
    fn test_drums_tom_child_structure() {
        let drums = create_drums_group();
        let tom = drums.find_child("Tom").unwrap();
        
        assert_eq!(tom.name, "Tom");
        assert_eq!(tom.prefix, "T");
        assert!(tom.patterns.contains(&"tom".to_string()));
        
        // Verify tom has children (sub-types)
        assert!(tom.has_children());
        assert!(tom.find_child("Rack").is_some());
        assert!(tom.find_child("Floor").is_some());
        assert!(tom.find_child("Mid").is_some());
    }

    #[test]
    fn test_drums_cymbals_child_structure() {
        let drums = create_drums_group();
        let cymbals = drums.find_child("Cymbals").unwrap();
        
        assert_eq!(cymbals.name, "Cymbals");
        assert_eq!(cymbals.prefix, "C");
        assert!(cymbals.patterns.contains(&"cymbal".to_string()));
        
        // Verify cymbals has children (sub-types)
        assert!(cymbals.has_children());
        assert!(cymbals.find_child("Hi Hat").is_some());
        assert!(cymbals.find_child("Ride").is_some());
        assert!(cymbals.find_child("Overheads").is_some());
    }

    #[test]
    fn test_drums_children_priority() {
        let drums = create_drums_group();
        
        // Verify all children have priority set (default is 0)
        for child in &drums.children {
            assert_eq!(child.priority, 0);
        }
    }

    #[test]
    fn test_drums_nested_hierarchy() {
        let drums = create_drums_group();
        
        // Test nested hierarchy: Drums -> Tom -> Rack
        let tom = drums.find_child("Tom").unwrap();
        let rack_tom = tom.find_child("Rack").unwrap();
        
        assert_eq!(rack_tom.name, "Rack");
        assert_eq!(rack_tom.prefix, "RACK");
        assert!(rack_tom.patterns.contains(&"rack".to_string()));
        
        // Test nested hierarchy: Drums -> Cymbals -> Hi Hat
        let cymbals = drums.find_child("Cymbals").unwrap();
        let hi_hat = cymbals.find_child("Hi Hat").unwrap();
        
        assert_eq!(hi_hat.name, "Hi Hat");
        assert_eq!(hi_hat.prefix, "HH");
        assert!(hi_hat.patterns.contains(&"hi hat".to_string()));
    }

    #[test]
    fn test_drums_patterns_match() {
        let drums = create_drums_group();
        
        // Test that drum patterns match
        assert!(drums.patterns.contains(&"drum".to_string()));
        assert!(drums.patterns.contains(&"drums".to_string()));
        
        // Test that child patterns are accessible
        let kick = drums.find_child("Kick").unwrap();
        assert!(kick.patterns.contains(&"kick".to_string()));
        assert!(kick.patterns.contains(&"bd".to_string()));
    }
}

