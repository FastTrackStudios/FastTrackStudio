//! Background Vocals (BGVs) template example
//!
//! This example demonstrates the BGVs template structure:
//! Soprano, Alto, Tenor, Baritone, Bass

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating BGVs template...\n");
    
    let mut template = Template::new("BGVs Template");
    
    // Helper function to create a TrackName and format it
    fn create_track_name(group_prefix: &str, sub_type: Option<&str>) -> String {
        let mut track_name = TrackName::new();
        track_name.group_prefix = Some(group_prefix.to_string());
        if let Some(st) = sub_type {
            track_name.sub_type = Some(vec![st.to_string()]);
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // BGVs - Top level (no BUS, just the group)
    // ============================================
    let bgvs_tracks = vec![
        "Soprano",
        "Alto",
        "Tenor",
        "Baritone",
        "Bass",
    ];
    
    for track_name in bgvs_tracks {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("BGV".to_string());
        track_name_obj.sub_type = Some(vec![track_name.to_string()]);
        let formatted_name = format_track_name_default(&track_name_obj);
        
        let mut track = Track::new(&formatted_name);
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
}

