//! Vocals template example
//!
//! This example demonstrates the VOCALS structure:
//! VOCALS (BUS) -> V LEAD (Performer Name) -> Alternates (English/Spanish/Portuguese)
//! VOCALS (BUS) -> V LEAD DBL (Performer Name) -> Alternates
//!
//! Each language track has the vocal comp on it.
//! If more than one track is needed, they are all moved to children of that specific track.

use track_template::{Track, Template, Take};
use naming_convention::{TrackName, format_track_name_default};

fn main() {
    println!("Creating Vocals template...\n");
    
    let mut template = Template::new("Vocals Template");
    
    // Helper function to create a TrackName with all components
    fn create_track_name(
        group_prefix: &str,
        sub_type: Option<&str>,
        track_type: Option<&str>,
        performer: Option<&str>,
        layer: Option<&str>, // For DBL (double)
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
            track_name.layers = Some(l.to_string()); // DBL is a layer
        }
        format_track_name_default(&track_name)
    }
    
    // ============================================
    // VOCALS (BUS) - Top level parent
    // ============================================
    let vocals_bus_name = create_track_name("V", Some("Vocals"), Some("BUS"), None, None);
    let vocals_bus = Track::new(&vocals_bus_name);
    template.add_track(vocals_bus);
    
    // ============================================
    // V LEAD (Performer Name) -> Alternates
    // ============================================
    // V LEAD for "John"
    let v_lead_john_name = create_track_name("V", Some("V LEAD"), None, Some("John"), None);
    let mut v_lead_john = Track::new(&v_lead_john_name);
    v_lead_john.set_parent(&vocals_bus_name);
    template.add_track(v_lead_john);
    
    // Alternates as children: English, Spanish, Portuguese
    let languages = vec!["English", "Spanish", "Portuguese"];
    
    for language in &languages {
        // Create track name with language as a section or sub-type
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("V".to_string());
        track_name_obj.sub_type = Some(vec!["V LEAD".to_string()]);
        track_name_obj.performer = Some("John".to_string());
        track_name_obj.section = Some(language.to_string());
        let language_track_name = format_track_name_default(&track_name_obj);
        
        let mut language_track = Track::new(&language_track_name);
        language_track.set_parent(&v_lead_john_name);
        language_track.add_take(Take::new(&format!("V LEAD John {}", language)));
        template.add_track(language_track);
    }
    
    // ============================================
    // V LEAD DBL (Performer Name) -> Alternates
    // ============================================
    // V LEAD DBL for "Jane"
    let v_lead_dbl_jane_name = create_track_name("V", Some("V LEAD"), None, Some("Jane"), Some("DBL"));
    let mut v_lead_dbl_jane = Track::new(&v_lead_dbl_jane_name);
    v_lead_dbl_jane.set_parent(&vocals_bus_name);
    template.add_track(v_lead_dbl_jane);
    
    // Alternates as children for DBL
    for language in &languages {
        let mut track_name_obj = TrackName::new();
        track_name_obj.group_prefix = Some("V".to_string());
        track_name_obj.sub_type = Some(vec!["V LEAD".to_string()]);
        track_name_obj.performer = Some("Jane".to_string());
        track_name_obj.layers = Some("DBL".to_string());
        track_name_obj.section = Some(language.to_string());
        let language_track_name = format_track_name_default(&track_name_obj);
        
        let mut language_track = Track::new(&language_track_name);
        language_track.set_parent(&v_lead_dbl_jane_name);
        language_track.add_take(Take::new(&format!("V LEAD DBL Jane {}", language)));
        template.add_track(language_track);
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
    println!("  {}: {} children", vocals_bus_name, template.track_list().get_children(&vocals_bus_name).len());
    println!("  {}: {} children", v_lead_john_name, template.track_list().get_children(&v_lead_john_name).len());
    println!("  {}: {} children", v_lead_dbl_jane_name, template.track_list().get_children(&v_lead_dbl_jane_name).len());
}

