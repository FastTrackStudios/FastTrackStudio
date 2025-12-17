//! Default track structure definitions
//!
//! Defines the expected track hierarchy for different instrument groups.
//! These structures represent how tracks should be organized in a DAW project.
//!
//! Note: Individual group track structures are defined in their respective
//! group files (e.g., `kick.rs` defines `create_kick_track_structure()`).
//! This file composes them into larger structures.

use super::track_structure::TrackStructure;
use super::drum_kit::{
    kick::create_kick_track_structure,
    snare::create_snare_track_structure,
    tom::create_tom_track_structure,
    cymbals::create_cymbals_track_structure,
    rooms::create_rooms_track_structure,
};

/// Create the default Drums track structure
/// 
/// Structure:
/// ```
/// Drums
/// ├── Kick (BUS)
/// │   ├── Kick (SUM)
/// │   │   ├── Kick In
/// │   │   ├── Kick Out
/// │   │   └── Kick Trig
/// │   ├── Kick Sub
/// │   └── Kick Ambient
/// ├── Snare (BUS)
/// │   ├── Snare (SUM)
/// │   │   ├── Snare Top
/// │   │   ├── Snare Bottom
/// │   │   └── Snare Trig
/// │   └── Snare Verb
/// ├── Tom (BUS)
/// │   ├── Tom 1
/// │   ├── Tom 2
/// │   └── Tom 3
/// ├── Cymbals (BUS)
/// │   ├── Hi-Hat
/// │   ├── Ride
/// │   └── OH
/// └── Room (BUS)
///     ├── Rooms
///     ├── Rooms Far
///     └── Rooms Mono
/// ```
pub fn create_drums_track_structure() -> TrackStructure {
    let mut drums = TrackStructure::new("Drums");
    
    // Use the track structures from their respective group files
    drums.add_child(create_kick_track_structure());
    drums.add_child(create_snare_track_structure());
    drums.add_child(create_tom_track_structure());
    drums.add_child(create_cymbals_track_structure());
    drums.add_child(create_rooms_track_structure());
    
    drums
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_drums_track_structure() {
        let structure = create_drums_track_structure();
        
        assert_eq!(structure.name, "Drums");
        assert!(structure.has_children());
        assert_eq!(structure.children.len(), 2);
    }

    #[test]
    fn test_drums_kick_structure() {
        let structure = create_drums_track_structure();
        let kick_bus = structure.find_child("Kick").unwrap();
        
        assert_eq!(kick_bus.name, "Kick");
        assert_eq!(kick_bus.track_type, Some("BUS".to_string()));
        assert!(kick_bus.has_children());
        
        // Check Kick SUM
        let kick_sum = kick_bus.find_child("Kick").unwrap();
        assert_eq!(kick_sum.track_type, Some("SUM".to_string()));
        assert!(kick_sum.has_children());
        
        // Check multi-mic tracks
        assert!(kick_sum.find_child("Kick In").is_some());
        assert!(kick_sum.find_child("Kick Out").is_some());
        assert!(kick_sum.find_child("Kick Trig").is_some());
        
        // Check other Kick tracks
        assert!(kick_bus.find_child("Kick Sub").is_some());
        assert!(kick_bus.find_child("Kick Ambient").is_some());
    }

    #[test]
    fn test_drums_snare_structure() {
        let structure = create_drums_track_structure();
        let snare_bus = structure.find_child("Snare").unwrap();
        
        assert_eq!(snare_bus.name, "Snare");
        assert_eq!(snare_bus.track_type, Some("BUS".to_string()));
        assert!(snare_bus.has_children());
        
        // Check Snare SUM
        let snare_sum = snare_bus.find_child("Snare").unwrap();
        assert_eq!(snare_sum.track_type, Some("SUM".to_string()));
        assert!(snare_sum.has_children());
        
        // Check multi-mic tracks
        assert!(snare_sum.find_child("Snare Top").is_some());
        assert!(snare_sum.find_child("Snare Bottom").is_some());
        assert!(snare_sum.find_child("Snare Trig").is_some());
        
        // Check Snare Verb
        assert!(snare_bus.find_child("Snare Verb").is_some());
    }

    #[test]
    fn test_drums_structure_hierarchy() {
        let structure = create_drums_track_structure();
        
        // Verify hierarchy depth
        assert_eq!(structure.depth(), 3); // Drums -> Kick BUS -> Kick SUM -> Kick In
        
        // Verify total node count
        // Drums (1) + Kick BUS (1) + Kick SUM (1) + Kick In/Out/Trig (3) + Kick Sub (1) + Kick Ambient (1)
        // + Snare BUS (1) + Snare SUM (1) + Snare Top/Bottom/Trig (3) + Snare Verb (1)
        // = 1 + 1 + 1 + 3 + 1 + 1 + 1 + 1 + 3 + 1 = 14
        assert_eq!(structure.node_count(), 14);
    }

    #[test]
    fn test_drums_leaf_nodes() {
        let structure = create_drums_track_structure();
        let leaves = structure.leaf_nodes();
        
        // Should have: Kick In, Kick Out, Kick Trig, Kick Sub, Kick Ambient,
        //              Snare Top, Snare Bottom, Snare Trig, Snare Verb
        assert_eq!(leaves.len(), 9);
        
        let leaf_names: Vec<&str> = leaves.iter().map(|l| l.name.as_str()).collect();
        assert!(leaf_names.contains(&"Kick In"));
        assert!(leaf_names.contains(&"Kick Out"));
        assert!(leaf_names.contains(&"Kick Trig"));
        assert!(leaf_names.contains(&"Kick Sub"));
        assert!(leaf_names.contains(&"Kick Ambient"));
        assert!(leaf_names.contains(&"Snare Top"));
        assert!(leaf_names.contains(&"Snare Bottom"));
        assert!(leaf_names.contains(&"Snare Trig"));
        assert!(leaf_names.contains(&"Snare Verb"));
    }

    #[test]
    fn test_track_type_consistency() {
        let structure = create_drums_track_structure();
        
        // All BUS tracks should have track_type = "BUS"
        let kick_bus = structure.find_child("Kick").unwrap();
        assert_eq!(kick_bus.track_type, Some("BUS".to_string()));
        
        let snare_bus = structure.find_child("Snare").unwrap();
        assert_eq!(snare_bus.track_type, Some("BUS".to_string()));
        
        // All SUM tracks should have track_type = "SUM"
        let kick_sum = kick_bus.find_child("Kick").unwrap();
        assert_eq!(kick_sum.track_type, Some("SUM".to_string()));
        
        let snare_sum = snare_bus.find_child("Snare").unwrap();
        assert_eq!(snare_sum.track_type, Some("SUM".to_string()));
        
        // All leaf tracks should have no track_type
        for leaf in structure.leaf_nodes() {
            assert_eq!(leaf.track_type, None);
        }
    }
}

