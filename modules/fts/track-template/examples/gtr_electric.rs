//! Electric Guitar (GTR E) template example
//!
//! This example demonstrates the complex GTR E structure:
//! GTR E (BUS) -> Performer Name -> Arrangement Descriptor -> 
//! Layer (Split/DBL/L/R) -> Multi-Mic (Amp/DI/NO-FX)
//!
//! The main audio file goes on the "What you're recording" track.
//! If there are matching DI and NO-FX files, they become children of the main track.

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating Electric Guitar template...\n");
    
    let mut template = Template::new("Electric Guitar Template");
    
    // Helper function to create a TrackName with all components
    fn create_track_name(
        group_prefix: &str,
        sub_type: Option<&str>,
        track_type: Option<&str>,
        performer: Option<&str>,
        arrangement: Option<&str>,
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
        if let Some(a) = arrangement {
            track_name.arrangement = Some(a.to_string());
        }
        if let Some(l) = layer {
            track_name.channel = Some(l.to_string()); // L/R/DBL/Split are channels
        }
        if let Some(mm) = multi_mic {
            track_name.multi_mic = Some(vec![mm.to_string()]);
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // GTR E (BUS) - Top level parent
    // ============================================
    let gtr_e_bus_name = create_track_name("G", Some("GTR E"), Some("BUS"), None, None, None, None);
    let gtr_e_bus = Track::new(&gtr_e_bus_name);
    template.add_track(gtr_e_bus);
    
    // ============================================
    // Example: Performer "John" -> Arrangement "Rhythm" -> Layer "L" -> Multi-Mic "Amp"
    // ============================================
    // Main track: Amp (this is what you're recording)
    let amp_track_name = create_track_name("G", Some("GTR E"), None, Some("John"), Some("Rhythm"), Some("L"), Some("Amp"));
    let mut amp_track = Track::new(&amp_track_name);
    amp_track.set_parent(&gtr_e_bus_name);
    amp_track.add_take(Take::new("GTR E John Rhythm L Amp"));
    template.add_track(amp_track);
    
    // DI becomes a child of Amp
    let di_track_name = create_track_name("G", Some("GTR E"), None, Some("John"), Some("Rhythm"), Some("L"), Some("DI"));
    let mut di_track = Track::new(&di_track_name);
    di_track.set_parent(&amp_track_name);
    di_track.add_take(Take::new("GTR E John Rhythm L DI"));
    template.add_track(di_track);
    
    // NO-FX becomes a child of Amp
    let no_fx_track_name = create_track_name("G", Some("GTR E"), None, Some("John"), Some("Rhythm"), Some("L"), Some("NO-FX"));
    let mut no_fx_track = Track::new(&no_fx_track_name);
    no_fx_track.set_parent(&amp_track_name);
    no_fx_track.add_take(Take::new("GTR E John Rhythm L NO-FX"));
    template.add_track(no_fx_track);
    
    // ============================================
    // Example: Performer "Jane" -> Arrangement "Lead" -> Layer "R" -> Multi-Mic "Amp"
    // ============================================
    let amp_track2_name = create_track_name("G", Some("GTR E"), None, Some("Jane"), Some("Lead"), Some("R"), Some("Amp"));
    let mut amp_track2 = Track::new(&amp_track2_name);
    amp_track2.set_parent(&gtr_e_bus_name);
    amp_track2.add_take(Take::new("GTR E Jane Lead R Amp"));
    template.add_track(amp_track2);
    
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
    println!("  {}: {} children", gtr_e_bus_name, template.track_list().get_children(&gtr_e_bus_name).len());
    println!("  {}: {} children", amp_track_name, template.track_list().get_children(&amp_track_name).len());
}

