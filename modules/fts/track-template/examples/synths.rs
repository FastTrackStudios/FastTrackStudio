//! Synths template example
//!
//! This example demonstrates the synths template structure:
//! Chords, Pads, Arps, Leads, Other

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating synths template...\n");
    
    let mut template = Template::new("Synths Template");
    
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
    // SYNTHS (BUS) - Top level parent
    // ============================================
    let synths_bus_name = create_track_name("SY", Some("Synths"), Some("BUS"), None);
    let synths_bus = Track::new(&synths_bus_name);
    template.add_track(synths_bus);
    
    // ============================================
    // Synth tracks - Children of SYNTHS
    // ============================================
    let synth_tracks = vec![
        "Chords",
        "Pads",
        "Arps",
        "Leads",
        "Other",
    ];
    
    for track_name in synth_tracks {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("SY".to_string());
        track_name_obj.sub_type = Some(vec![track_name.to_string()]);
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
        track.set_parent(&synths_bus_name);
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
    println!("  {}: {} children", synths_bus_name, template.track_list().get_children(&synths_bus_name).len());
}

