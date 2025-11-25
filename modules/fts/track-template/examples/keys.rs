//! Keys template example
//!
//! This example demonstrates the KEYS structure:
//! KEYS (BUS) -> Instrument (Piano, Electric, etc.) -> Multi-Mic
//!
//! For now, tracks are in a list based on what it is.
//! If there are layers, those will be included as children.
//! Multi-mic sources become children of the main track.

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating Keys template...\n");
    
    let mut template = Template::new("Keys Template");
    
    // Helper function to create a TrackName with all components
    fn create_track_name(
        group_prefix: &str,
        sub_type: Option<&str>,
        track_type: Option<&str>,
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
        if let Some(mm) = multi_mic {
            track_name.multi_mic = Some(vec![mm.to_string()]);
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // KEYS (BUS) - Top level parent
    // ============================================
    let keys_bus_name = create_track_name("K", Some("Keys"), Some("BUS"), None);
    let keys_bus = Track::new(&keys_bus_name);
    template.add_track(keys_bus);
    
    // ============================================
    // Piano - Main track
    // ============================================
    let piano_track_name = create_track_name("K", Some("Piano"), None, None);
    let mut piano_track = Track::new(&piano_track_name);
    piano_track.set_parent(&keys_bus_name);
    piano_track.add_take(Take::new("Piano"));
    template.add_track(piano_track);
    
    // Piano could have multi-mic sources as children
    // (e.g., Close, Room, etc.)
    
    // ============================================
    // Electric - Main track with Amp and DI as children
    // ============================================
    let electric_track_name = create_track_name("K", Some("Electric"), None, None);
    let mut electric_track = Track::new(&electric_track_name);
    electric_track.set_parent(&keys_bus_name);
    electric_track.add_take(Take::new("Electric"));
    template.add_track(electric_track);
    
    // Amp becomes a child of Electric
    let electric_amp_name = create_track_name("K", Some("Electric"), None, Some("Amp"));
    let mut electric_amp = Track::new(&electric_amp_name);
    electric_amp.set_parent(&electric_track_name);
    electric_amp.add_take(Take::new("Electric Amp"));
    template.add_track(electric_amp);
    
    // DI becomes a child of Electric
    let electric_di_name = create_track_name("K", Some("Electric"), None, Some("DI"));
    let mut electric_di = Track::new(&electric_di_name);
    electric_di.set_parent(&electric_track_name);
    electric_di.add_take(Take::new("Electric DI"));
    template.add_track(electric_di);
    
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
    println!("  {}: {} children", keys_bus_name, template.track_list().get_children(&keys_bus_name).len());
    println!("  {}: {} children", electric_track_name, template.track_list().get_children(&electric_track_name).len());
}

