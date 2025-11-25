//! Electronic Drums (E-DRUMS) template example
//!
//! This example demonstrates the E-DRUMS template structure.
//! More flexible than acoustic drums, just sorted as an electronic drum kit.

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating E-DRUMS template...\n");
    
    let mut template = Template::new("E-DRUMS Template");
    
    // Helper function to create a TrackName and format it
    fn create_track_name(group_prefix: &str, sub_type: Option<&str>, track_type: Option<&str>, _unused: Option<&str>) -> String {
        let mut track_name = TrackName::new();
        track_name.group_prefix = Some(group_prefix.to_string());
        if let Some(st) = sub_type {
            track_name.sub_type = Some(vec![st.to_string()]);
        }
        if let Some(tt) = track_type {
            track_name.track_type = Some(tt.to_string());
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // E-DRUMS (BUS) - Top level parent
    // ============================================
    let e_drums_bus_name = create_track_name("ED", Some("E-Drums"), Some("BUS"), None);
    let e_drums_bus = Track::new(&e_drums_bus_name);
    template.add_track(e_drums_bus);
    
    // E-DRUMS is flexible - tracks are just sorted as an electronic drum kit
    // Example tracks that might appear:
    let example_tracks = vec![
        "Kick",
        "Snare",
        "HiHat",
        "Tom 1",
        "Tom 2",
        "Crash",
        "Ride",
    ];
    
    for track_name in example_tracks {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("ED".to_string());
        track_name_obj.sub_type = Some(vec![track_name.to_string()]);
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
        track.set_parent(&e_drums_bus_name);
        track.add_take(Take::new(track_name));
        template.add_track(track);
    }
    
    // ============================================
    // Display the complete template
    // ============================================
    println!("{}", template);
    
    // ============================================
    // Verify structure
    // ============================================
    println!("\n=== Template Statistics ===");
    println!("Total tracks: {}", template.track_list().len());
    println!("Root tracks: {}", template.track_list().get_roots().len());
    println!("  {}: {} children", e_drums_bus_name, template.track_list().get_children(&e_drums_bus_name).len());
}

