//! Acoustic Guitar (GTR AG) template example
//!
//! This example demonstrates the GTR AG structure:
//! GTR AG (BUS) -> Performer -> Layer (L/R/DBL) -> Multi-Mic (Neck/Bridge/DI)
//!
//! The default structure is more like:
//! Performer -> Layer -> Multi-Mic Sources

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating Acoustic Guitar template...\n");
    
    let mut template = Template::new("Acoustic Guitar Template");
    
    // Helper function to create a TrackName with all components
    fn create_track_name(
        group_prefix: &str,
        sub_type: Option<&str>,
        track_type: Option<&str>,
        performer: Option<&str>,
        layer: Option<&str>,
        multi_mic: Option<&str>,
    ) -> String {
        let mut track_name = TrackName::new();
        track_name.group_prefix = Some(group_prefix.to_string());
        if let Some(st) = sub_type {
            track_name.sub_type = Some(vec![st.to_string()]);
        }
        if let Some(tt) = track_type {
            track_name.track_type = Some(tt.to_string());
        }
        if let Some(p) = performer {
            track_name.performer = Some(p.to_string());
        }
        if let Some(l) = layer {
            track_name.channel = Some(l.to_string()); // L/R/DBL are channels
        }
        if let Some(mm) = multi_mic {
            track_name.multi_mic = Some(vec![mm.to_string()]);
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // GTR AG (BUS) - Top level parent
    // ============================================
    let gtr_ag_bus_name = create_track_name("G", Some("GTR AG"), Some("BUS"), None, None, None);
    let gtr_ag_bus = Track::new(&gtr_ag_bus_name);
    template.add_track(gtr_ag_bus);
    
    // ============================================
    // Example: Performer "John" -> Layer "L" -> Multi-Mic "Neck"
    // ============================================
    // Main track: Neck (this is what you're recording)
    let neck_track_name = create_track_name("G", Some("GTR AG"), None, Some("John"), Some("L"), Some("Neck"));
    let mut neck_track = Track::new(&neck_track_name);
    neck_track.set_parent(&gtr_ag_bus_name);
    neck_track.add_take(Take::new("GTR AG John L Neck"));
    template.add_track(neck_track);
    
    // Bridge becomes a child of Neck
    let bridge_track_name = create_track_name("G", Some("GTR AG"), None, Some("John"), Some("L"), Some("Bridge"));
    let mut bridge_track = Track::new(&bridge_track_name);
    bridge_track.set_parent(&neck_track_name);
    bridge_track.add_take(Take::new("GTR AG John L Bridge"));
    template.add_track(bridge_track);
    
    // DI becomes a child of Neck
    let di_track_name = create_track_name("G", Some("GTR AG"), None, Some("John"), Some("L"), Some("DI"));
    let mut di_track = Track::new(&di_track_name);
    di_track.set_parent(&neck_track_name);
    di_track.add_take(Take::new("GTR AG John L DI"));
    template.add_track(di_track);
    
    // ============================================
    // Example: Performer "Jane" -> Layer "R" -> Multi-Mic "Bridge"
    // ============================================
    let bridge_track2_name = create_track_name("G", Some("GTR AG"), None, Some("Jane"), Some("R"), Some("Bridge"));
    let mut bridge_track2 = Track::new(&bridge_track2_name);
    bridge_track2.set_parent(&gtr_ag_bus_name);
    bridge_track2.add_take(Take::new("GTR AG Jane R Bridge"));
    template.add_track(bridge_track2);
    
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
    println!("  {}: {} children", gtr_ag_bus_name, template.track_list().get_children(&gtr_ag_bus_name).len());
    println!("  {}: {} children", neck_track_name, template.track_list().get_children(&neck_track_name).len());
}

