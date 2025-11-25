//! Bass template example
//!
//! This example demonstrates the bass template structure:
//! - Bass GTR with Amp and DI as children
//! - Bass Synth (flexible)

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating bass template...\n");
    
    let mut template = Template::new("Bass Template");
    
    // Helper function to create a TrackName and format it
    fn create_track_name(group_prefix: &str, sub_type: Option<&str>, track_type: Option<&str>, multi_mic: Option<&str>) -> String {
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
    // BASS (BUS) - Top level parent
    // ============================================
    let bass_bus_name = create_track_name("B", Some("Bass"), Some("BUS"), None);
    let bass_bus = Track::new(&bass_bus_name);
    template.add_track(bass_bus);
    
    // ============================================
    // Bass GTR (BUS) - Child of Bass
    // ============================================
    let bass_gtr_bus_name = create_track_name("B", Some("Bass GTR"), Some("BUS"), None);
    let mut bass_gtr_bus = Track::new(&bass_gtr_bus_name);
    bass_gtr_bus.set_parent(&bass_bus_name);
    template.add_track(bass_gtr_bus);
    
    // Bass Amp track - Child of Bass GTR
    let bass_amp_name = create_track_name("B", Some("Bass"), None, Some("Amp"));
    let mut bass_amp = Track::new(&bass_amp_name);
    bass_amp.set_parent(&bass_gtr_bus_name);
    bass_amp.add_take(Take::new("Bass Amp"));
    template.add_track(bass_amp);
    
    // Bass DI track - Child of Bass GTR
    let bass_di_name = create_track_name("B", Some("Bass"), None, Some("DI"));
    let mut bass_di = Track::new(&bass_di_name);
    bass_di.set_parent(&bass_gtr_bus_name);
    bass_di.add_take(Take::new("Bass DI"));
    template.add_track(bass_di);
    
    // ============================================
    // Bass Synth - Child of Bass (flexible)
    // ============================================
    let bass_synth_name = create_track_name("B", Some("Bass Synth"), None, None);
    let mut bass_synth = Track::new(&bass_synth_name);
    bass_synth.set_parent(&bass_bus_name);
    bass_synth.add_take(Take::new("Bass Synth"));
    template.add_track(bass_synth);
    
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
    
    println!("\nTracks by parent:");
    println!("  {}: {} children", bass_bus_name, template.track_list().get_children(&bass_bus_name).len());
    println!("  {}: {} children", bass_gtr_bus_name, template.track_list().get_children(&bass_gtr_bus_name).len());
}

