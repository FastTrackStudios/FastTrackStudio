//! Default Room group configuration
//!
//! Room group for room/ambient microphone tracks.

use crate::{FullGroup, define_group, TrackStructure};

/// Create the default Room group with multi-mic descriptors
pub fn create_rooms_group() -> FullGroup {
    let mut rooms = define_group! {
        name = "Room",
        prefix = "R",
        patterns = ["room", "rooms"],
        parent_track = "D ROOM (BUS)",
    };
    
    // Multi-mic descriptors
    rooms.multi_mic("Short")
        .patterns(["short", "Short", "close", "Close"]);
    
    rooms.multi_mic("Far")
        .patterns(["far", "Far", "distant", "Distant"]);
    
    rooms.multi_mic("Mono")
        .patterns(["mono", "Mono", "mon", "Mon"]);
    
    rooms
}

/// Create the track structure for the Room group
/// 
/// Structure:
/// ```
/// Room (BUS)
/// ├── Rooms
/// ├── Rooms Far
/// └── Rooms Mono
/// ```
pub fn create_rooms_track_structure() -> TrackStructure {
    let mut room_bus = TrackStructure::with_track_type("Room", "BUS");
    
    room_bus.add_child(TrackStructure::new("Rooms"));
    room_bus.add_child(TrackStructure::new("Rooms Far"));
    room_bus.add_child(TrackStructure::new("Rooms Mono"));
    
    room_bus
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_create_rooms_group() {
        let rooms = create_rooms_group();
        
        assert_eq!(rooms.name, "Room");
        assert_eq!(rooms.prefix, "R");
        assert!(rooms.patterns.contains(&"room".to_string()));
        assert!(rooms.patterns.contains(&"rooms".to_string()));
        
        // Verify multi-mic descriptors exist
        let descriptor_names = rooms.multi_mic_descriptor_names();
        assert_eq!(descriptor_names.len(), 3);
        assert!(descriptor_names.contains(&"Short".to_string()));
        assert!(descriptor_names.contains(&"Far".to_string()));
        assert!(descriptor_names.contains(&"Mono".to_string()));
    }

    #[test]
    fn test_rooms_group_multi_mic_patterns() {
        let rooms = create_rooms_group();
        
        // Verify Short patterns
        let short_desc = rooms.find_multi_mic_descriptor("Short").unwrap();
        assert!(short_desc.patterns.contains(&"short".to_string()));
        assert!(short_desc.patterns.contains(&"Short".to_string()));
        assert!(short_desc.patterns.contains(&"close".to_string()));
        assert!(short_desc.patterns.contains(&"Close".to_string()));
        
        // Verify Far patterns
        let far_desc = rooms.find_multi_mic_descriptor("Far").unwrap();
        assert!(far_desc.patterns.contains(&"far".to_string()));
        assert!(far_desc.patterns.contains(&"Far".to_string()));
        assert!(far_desc.patterns.contains(&"distant".to_string()));
        assert!(far_desc.patterns.contains(&"Distant".to_string()));
        
        // Verify Mono patterns
        let mono_desc = rooms.find_multi_mic_descriptor("Mono").unwrap();
        assert!(mono_desc.patterns.contains(&"mono".to_string()));
        assert!(mono_desc.patterns.contains(&"Mono".to_string()));
        assert!(mono_desc.patterns.contains(&"mon".to_string()));
        assert!(mono_desc.patterns.contains(&"Mon".to_string()));
    }

    #[test]
    fn test_create_rooms_track_structure() {
        let structure = create_rooms_track_structure();
        
        assert_eq!(structure.name, "Room");
        assert_eq!(structure.track_type, Some("BUS".to_string()));
        assert!(structure.has_children());
        assert_eq!(structure.children.len(), 3);
        
        // Verify child tracks exist
        assert!(structure.find_child("Rooms").is_some());
        assert!(structure.find_child("Rooms Far").is_some());
        assert!(structure.find_child("Rooms Mono").is_some());
        
        // Verify child tracks have no track type (they're regular audio tracks)
        let rooms = structure.find_child("Rooms").unwrap();
        assert_eq!(rooms.track_type, None);
        
        let rooms_far = structure.find_child("Rooms Far").unwrap();
        assert_eq!(rooms_far.track_type, None);
        
        let rooms_mono = structure.find_child("Rooms Mono").unwrap();
        assert_eq!(rooms_mono.track_type, None);
    }
}

