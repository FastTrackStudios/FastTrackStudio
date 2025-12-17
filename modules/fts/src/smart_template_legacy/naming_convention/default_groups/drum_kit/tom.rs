//! Default Tom group configuration
//!
//! Based on the fts-naming-parser config.json structure, converted to
//! the new nested Group format using declarative macros.
//!
//! Tom group supports auto-increment (Tom 1, Tom 2, etc.) and uses nested
//! child groups (sub-types) for Rack, Floor, Hi, Mid, Low toms.

use super::super::super::{Group, GroupType, TrackStructure};

/// Create the default Tom group with auto-increment support and sub-types
/// 
/// Supports sorting by increment number (Tom 1, Tom 2, Tom 3) regardless of
/// sub-type (Rack, Floor, Hi, Mid, Low). The sub-types help identify the tom
/// type, but sorting is based on the increment number.
pub fn create_tom_group() -> Group {
    let mut tom = define_group! {
        name = "Tom",
        prefix = "T",
        patterns = ["tom"],
        parent_track = "D TOM (BUS)",
        group_type = Increment,
        children = [
            define_group! {
                name = "Rack",
                prefix = "RACK",
                patterns = ["rack", "rack tom"],
            },
            define_group! {
                name = "Floor",
                prefix = "FLOOR",
                patterns = ["floor", "floor tom"],
            },
            define_group! {
                name = "Hi",
                prefix = "HI",
                patterns = ["hi", "hi tom", "high", "high tom"],
            },
            define_group! {
                name = "Mid",
                prefix = "MID",
                patterns = ["mid", "mid tom", "med", "med tom"],
            },
            define_group! {
                name = "Low",
                prefix = "LOW",
                patterns = ["low", "low tom"],
            },
        ],
    };
    
    tom
}

/// Create the track structure for the Tom group
/// 
/// Structure:
/// ```
/// Tom (BUS)
/// ├── Tom 1
/// ├── Tom 2
/// └── ... (increment-based)
/// ```
/// 
/// Note: The actual number of tom tracks is determined at runtime based on
/// the tracks found. This structure represents the expected hierarchy.
pub fn create_tom_track_structure() -> TrackStructure {
    let mut tom_bus = TrackStructure::with_track_type("Tom", "BUS");
    
    // Add a few example tom tracks to show the structure
    // In practice, the actual number of toms will be determined dynamically
    tom_bus.add_child(TrackStructure::new("Tom 1"));
    tom_bus.add_child(TrackStructure::new("Tom 2"));
    
    tom_bus
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_tom_group() {
        let tom = create_tom_group();
        
        assert_eq!(tom.name, "Tom");
        assert_eq!(tom.prefix, "T");
        assert!(tom.patterns.contains(&"tom".to_string()));
        assert_eq!(tom.group_type, Some(GroupType::Increment));
        assert!(tom.has_children());
    }

    #[test]
    fn test_tom_group_structure() {
        let tom = create_tom_group();
        
        // Verify all sub-type child groups exist
        assert!(tom.has_children());
        assert!(tom.find_child("Rack").is_some());
        assert!(tom.find_child("Floor").is_some());
        assert!(tom.find_child("Hi").is_some());
        assert!(tom.find_child("Mid").is_some());
        assert!(tom.find_child("Low").is_some());
        
        // Verify child group patterns
        let rack = tom.find_child("Rack").unwrap();
        assert!(rack.patterns.contains(&"rack".to_string()));
        assert!(rack.patterns.contains(&"rack tom".to_string()));
        
        let floor = tom.find_child("Floor").unwrap();
        assert!(floor.patterns.contains(&"floor".to_string()));
        assert!(floor.patterns.contains(&"floor tom".to_string()));
        
        let hi = tom.find_child("Hi").unwrap();
        assert!(hi.patterns.contains(&"hi".to_string()));
        assert!(hi.patterns.contains(&"hi tom".to_string()));
        assert!(hi.patterns.contains(&"high".to_string()));
        assert!(hi.patterns.contains(&"high tom".to_string()));
        
        let mid = tom.find_child("Mid").unwrap();
        assert!(mid.patterns.contains(&"mid".to_string()));
        assert!(mid.patterns.contains(&"mid tom".to_string()));
        assert!(mid.patterns.contains(&"med".to_string()));
        assert!(mid.patterns.contains(&"med tom".to_string()));
        
        let low = tom.find_child("Low").unwrap();
        assert!(low.patterns.contains(&"low".to_string()));
        assert!(low.patterns.contains(&"low tom".to_string()));
    }

    #[test]
    fn test_tom_group_increment_type() {
        let tom = create_tom_group();
        
        // Verify it's set to Increment type for auto-numbering
        // This allows sorting by increment (Tom 1, Tom 2, Tom 3) regardless
        // of sub-type (Rack, Floor, Hi, Mid, Low)
        assert_eq!(tom.group_type, Some(GroupType::Increment));
    }
    
    #[test]
    fn test_tom_group_sorting_concept() {
        // This test documents the expected behavior:
        // - "Rack Tom 1", "Floor Tom 1", "Hi Tom 1" should all sort as "Tom 1"
        // - "Rack Tom 2", "Mid Tom 2", "Low Tom 2" should all sort as "Tom 2"
        // - The increment number determines sort order, not the sub-type
        let tom = create_tom_group();
        
        // The group supports increment-based sorting
        assert_eq!(tom.group_type, Some(GroupType::Increment));
        
        // Sub-types are available for identification but don't affect sort order
        assert!(tom.has_children());
        
        // Examples of what should parse:
        // - "Rack Tom 1" → sub_type: ["Rack"], increment: "1" → sorts as "Tom 1"
        // - "Floor Tom 2" → sub_type: ["Floor"], increment: "2" → sorts as "Tom 2"
        // - "Hi Tom 1" → sub_type: ["Hi"], increment: "1" → sorts as "Tom 1"
        // - "Tom 3" → increment: "3" → sorts as "Tom 3"
    }

    #[test]
    fn test_create_tom_track_structure() {
        let structure = create_tom_track_structure();
        
        assert_eq!(structure.name, "Tom");
        assert_eq!(structure.track_type, Some("BUS".to_string()));
        assert!(structure.has_children());
        
        // Verify example tom tracks exist
        assert!(structure.find_child("Tom 1").is_some());
        assert!(structure.find_child("Tom 2").is_some());
        
        // Verify tom tracks have no track type (they're regular audio tracks)
        let tom_1 = structure.find_child("Tom 1").unwrap();
        assert_eq!(tom_1.track_type, None);
        
        let tom_2 = structure.find_child("Tom 2").unwrap();
        assert_eq!(tom_2.track_type, None);
    }
}

