//! Percussion template example
//!
//! This example demonstrates the percussion template structure with various
//! percussion instruments: Tambourine, Shaker, Congas, Melodic, Hit Big,
//! Sweep, Cym Swell, Boom, Claps

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating percussion template...\n");
    
    let mut template = Template::new("Percussion Template");
    
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
    // PERC (BUS) - Top level parent
    // ============================================
    let perc_bus_name = create_track_name("P", Some("Perc"), Some("BUS"), None);
    let perc_bus = Track::new(&perc_bus_name);
    template.add_track(perc_bus);
    
    // ============================================
    // Percussion tracks - Children of PERC
    // ============================================
    let perc_tracks = vec![
        "Tambourine",
        "Shaker",
        "Congas",
        "Melodic",
        "Hit Big",
        "Sweep",
        "Cym Swell",
        "Boom",
        "Claps",
    ];
    
    for track_name in perc_tracks {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("P".to_string());
        track_name_obj.sub_type = Some(vec![track_name.to_string()]);
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
        track.set_parent(&perc_bus_name);
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
    println!("  {}: {} children", perc_bus_name, template.track_list().get_children(&perc_bus_name).len());
}

