//! Default Cymbals group configuration
//!
//! Based on the fts-naming-parser config.json structure, converted to
//! the new nested Group format using declarative macros.
//!
//! Cymbals group includes Hi Hat, Ride, and Overheads as sub-types.

use super::super::super::{Group, TrackStructure};

/// Create the default Cymbals group with sub-types
pub fn create_cymbals_group() -> Group {
    define_group! {
        name = "Cymbals",
        prefix = "C",
        patterns = ["cymbal", "cymbals"],
        parent_track = "D CYMBALS (BUS)",
        children = [
            define_group! {
                name = "Hi Hat",
                prefix = "HH",
                patterns = ["hi hat", "hihat", "hi%-hat", "hh", "hat", "closed hat", "open hat"],
            },
            define_group! {
                name = "Ride",
                prefix = "R",
                patterns = ["ride"],
            },
            define_group! {
                name = "Overheads",
                prefix = "OH",
                patterns = ["overhead", "overheads", "oh"],
            },
        ],
    }
}

/// Create the track structure for the Cymbals group
/// 
/// Structure:
/// ```
/// Cymbals (BUS)
/// ├── Hi-Hat
/// ├── Ride
/// └── OH
/// ```
pub fn create_cymbals_track_structure() -> TrackStructure {
    let mut cymbals_bus = TrackStructure::with_track_type("Cymbals", "BUS");
    
    cymbals_bus.add_child(TrackStructure::new("Hi-Hat"));
    cymbals_bus.add_child(TrackStructure::new("Ride"));
    cymbals_bus.add_child(TrackStructure::new("OH"));
    
    cymbals_bus
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_cymbals_group() {
        let cymbals = create_cymbals_group();
        
        assert_eq!(cymbals.name, "Cymbals");
        assert_eq!(cymbals.prefix, "C");
        assert!(cymbals.patterns.contains(&"cymbal".to_string()));
        assert!(cymbals.patterns.contains(&"cymbals".to_string()));
        assert!(cymbals.has_children());
    }

    #[test]
    fn test_cymbals_group_structure() {
        let cymbals = create_cymbals_group();
        
        // Verify all sub-type child groups exist
        assert!(cymbals.has_children());
        assert!(cymbals.find_child("Hi Hat").is_some());
        assert!(cymbals.find_child("Ride").is_some());
        assert!(cymbals.find_child("Overheads").is_some());
        
        // Verify child group patterns
        let hi_hat = cymbals.find_child("Hi Hat").unwrap();
        assert_eq!(hi_hat.prefix, "HH");
        assert!(hi_hat.patterns.contains(&"hi hat".to_string()));
        assert!(hi_hat.patterns.contains(&"hihat".to_string()));
        assert!(hi_hat.patterns.contains(&"hh".to_string()));
        assert!(hi_hat.patterns.contains(&"hat".to_string()));
        assert!(hi_hat.patterns.contains(&"closed hat".to_string()));
        assert!(hi_hat.patterns.contains(&"open hat".to_string()));
        
        let ride = cymbals.find_child("Ride").unwrap();
        assert_eq!(ride.prefix, "R");
        assert!(ride.patterns.contains(&"ride".to_string()));
        
        let overheads = cymbals.find_child("Overheads").unwrap();
        assert_eq!(overheads.prefix, "OH");
        assert!(overheads.patterns.contains(&"overhead".to_string()));
        assert!(overheads.patterns.contains(&"overheads".to_string()));
        assert!(overheads.patterns.contains(&"oh".to_string()));
    }

    #[test]
    fn test_create_cymbals_track_structure() {
        let structure = create_cymbals_track_structure();
        
        assert_eq!(structure.name, "Cymbals");
        assert_eq!(structure.track_type, Some("BUS".to_string()));
        assert!(structure.has_children());
        assert_eq!(structure.children.len(), 3);
        
        // Verify child tracks exist
        assert!(structure.find_child("Hi-Hat").is_some());
        assert!(structure.find_child("Ride").is_some());
        assert!(structure.find_child("OH").is_some());
        
        // Verify child tracks have no track type (they're regular audio tracks)
        let hi_hat = structure.find_child("Hi-Hat").unwrap();
        assert_eq!(hi_hat.track_type, None);
        
        let ride = structure.find_child("Ride").unwrap();
        assert_eq!(ride.track_type, None);
        
        let oh = structure.find_child("OH").unwrap();
        assert_eq!(oh.track_type, None);
    }
}

